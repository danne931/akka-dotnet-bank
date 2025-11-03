module Bank.Routes.Card

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open Akkling
open Akka.Actor
open FsToolkit.ErrorHandling

open Bank.Org.Domain
open Bank.Employee.Domain
open Bank.Purchase.Domain
open Bank.Card.Api
open CardIssuer.Service.Domain
open CommandApproval
open RoutePaths
open Lib.SharedTypes
open Bank.UserSession.Middleware
open Lib.NetworkQuery
open BankActorRegistry

let processCommand = Bank.Employee.Api.processCommand

let start (app: WebApplication) =
   app.MapGet(
      CardPath.Get,
      Func<
         Guid,
         string,
         Nullable<decimal>,
         Nullable<decimal>,
         string,
         string,
         Task<IResult>
       >
         (fun
              orgId
              ([<FromQuery>] createdAt)
              ([<FromQuery>] amountMin)
              ([<FromQuery>] amountMax)
              ([<FromQuery>] accountIds)
              ([<FromQuery>] employeeIds) ->
            let query = {
               Amount = AmountFilter.fromQuery amountMin amountMax
               CreatedAtDateRange = dateRangeFromQueryString createdAt
               AccountIds = CardQuery.accountIdsFromQueryString accountIds
               EmployeeIds = CardQuery.employeeIdsFromQueryString employeeIds
            }

            getCards (OrgId orgId) query |> RouteUtil.unwrapTaskResultOption)
   )
   |> ignore

   app
      .MapPost(
         CardPath.Base,
         Func<BankActorRegistry, CreateCardCommand, Task<IResult>>
            (fun registry cmd ->
               processCommand registry (EmployeeCommand.CreateCard cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.CreateCard)
   |> ignore

   (*
    * Sending this request to Lithic will simulate Lithic card issuer receiving
    * purchase requests from the card networks.  Lithic will validate the
    * request against my configured card program rules.  Since I am also
    * using Lithic's Auth Stream Access (ASA) feature, Lithic will forward
    * the transaction (if it passes the rules I configure for my Lithic card
    * program) to my server where I can provide additional custom logic 
    * within the employee actor to determine whether to approve the transaction.
    *
    * Alternatively, if we do not care to test the Lithic integration then the
    * purchase auth will bypass the (app -> Lithic -> app webhook) flow
    * & be sent directly to the employee actor.
    *)
   app
      .MapPost(
         CardPath.Purchase,
         Func<
            ActorSystem,
            BankActorRegistry,
            PurchaseIntentCommand,
            Task<IResult>
          >
            (fun system registry cmd ->
               taskResult {
                  let conf = EnvEmployee.config
                  let purchase = cmd.Data

                  let! envelope =
                     PurchaseIntentCommand.toEvent cmd
                     |> Result.map EmployeeEnvelope.get
                     |> Result.mapError Err.ValidationError

                  match conf.CardIssuerMockRequests, conf.CardIssuerApiKey with
                  // Route handler -> Lithic -> ASA Webhook route handler -> EmployeeActor
                  | false, Some apiKey ->
                     let! cardIssuerCardId =
                        getIssuerCardIdFromInternalCardId purchase.CardId

                     let! card =
                        CardIssuerService.getCard apiKey cardIssuerCardId

                     let! _ =
                        CardIssuerService.simulatePurchaseAuthorization apiKey {
                           Amount = purchase.Amount
                           Descriptor = purchase.Merchant
                           CardNumber = card.Number
                           MerchantCurrency = purchase.CurrencyMerchant
                           Metadata = {
                              OrgId = cmd.OrgId
                              CorrelationId = cmd.CorrelationId
                           }
                        }

                     return envelope
                  // Route handler -> EmployeeActor
                  | _ ->
                     let employeeRef =
                        (registry :> IEmployeeActor).EmployeeActor
                           purchase.EmployeeId

                     let msg =
                        cmd
                        |> EmployeeCommand.PurchaseIntent
                        |> EmployeeMessage.StateChange

                     let! (authResult: PurchaseAuthorizationStatus) =
                        employeeRef.Ask(msg, Some(TimeSpan.FromSeconds 4.5))
                        |> Async.map Ok
                        |> AsyncResult.catch Err.NetworkError

                     do! Async.Sleep 700

                     // Simulate Clearing event from card issuer
                     let progress: CardIssuerPurchaseProgress = {
                        Events =
                           NonEmptyList.create
                              {
                                 Type = PurchaseEventType.Auth
                                 Money = {
                                    Amount = purchase.Amount
                                    Flow = MoneyFlow.Out
                                 }
                                 EnforcedRules = []
                                 EventId = Guid.NewGuid()
                                 CreatedAt = cmd.Timestamp
                              }
                              [
                                 {
                                    Type = PurchaseEventType.Clearing
                                    Money = {
                                       Amount = purchase.Amount
                                       Flow = MoneyFlow.Out
                                    }
                                    EnforcedRules = []
                                    EventId = Guid.NewGuid()
                                    CreatedAt = cmd.Timestamp.AddSeconds(1)
                                 }
                              ]
                        Result = "APPROVED"
                        Status = PurchaseStatus.Settled
                        PurchaseId = purchase.CardIssuerTransactionId
                        CardIssuerCardId = purchase.CardIssuerCardId
                        MerchantName = purchase.Merchant
                        Amounts = {
                           Hold = {
                              Amount = 0m
                              Currency = purchase.CurrencyMerchant
                           }
                           Cardholder = {
                              Amount = purchase.Amount
                              Currency = purchase.CurrencyCardHolder
                              ConversionRate = 1m
                           }
                           Merchant = {
                              Amount = purchase.Amount
                              Currency = purchase.CurrencyMerchant
                           }
                           Settlement = {
                              Amount = purchase.Amount
                              Currency = purchase.CurrencyMerchant
                           }
                        }
                     }

                     employeeRef
                     <! EmployeeMessage.PurchaseProgress(
                        progress,
                        purchase.CardId
                     )

                     return!
                        match authResult with
                        | PurchaseAuthorizationStatus.Approved -> Ok envelope
                        | _ ->
                           Err.UnexpectedError "Purchase authorization failed"
                           |> Error
               }
               |> TaskResult.teeError (fun err ->
                  let err = exn (string err)
                  ActorUtil.SystemLog.error system err)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.DebitRequest)
   |> ignore

   (*
    * Lithic card issuer will forward purchases received from the card networks
    * to this webhook handler where I can provide additional custom logic 
    * within my employee actor to determine whether to approve the transaction.
    * In order to receive ASA webhook requests we must enroll a responder endpoint
    * (https://docs.lithic.com/reference/postresponderendpoints).  The webhook
    * URL to enroll should be equivalent to CardPath.AuthStreamAccessWebhook
    * used below.
    * For local dev use `ngrok http http://localhost:3000` to obtain a url
    * that can be used for enrolling a responder endpoint in ASA.
    * Ex: https://e1677f21bcf6.ngrok-free.app/purchase-auth-stream-access-webhook
   *)
   app
      .MapPost(
         CardPath.AuthStreamAccessWebhook,
         Func<
            ActorSystem,
            BankActorRegistry,
            AuthStreamAccessWebhookRequestDTO,
            Task<IResult>
          >
            (fun system registry purchaseAuthReq ->
               taskResult {
                  let dtoAsEntityMaybe = purchaseAuthReq.AsEntity

                  dtoAsEntityMaybe
                  |> Result.teeError (exn >> ActorUtil.SystemLog.error system)
                  |> ignore

                  let! (purchaseAuthReq: AuthStreamAccessWebhookRequest) =
                     dtoAsEntityMaybe |> Result.mapError Err.UnexpectedError

                  let! cardId, employeeId =
                     getInternalIdsFromIssuerCardId
                        purchaseAuthReq.CardIssuerCardId

                  let purchaseAuth: PurchaseAuthorization = {
                     CardId = cardId
                     CardIssuerTransactionId =
                        purchaseAuthReq.CardIssuerTransactionId
                     CardIssuerCardId = purchaseAuthReq.CardIssuerCardId
                     Amount = purchaseAuthReq.Amount
                     MerchantCategoryCode =
                        purchaseAuthReq.MerchantCategoryCode
                     MerchantName = purchaseAuthReq.MerchantName
                     CreatedAt = DateTime.UtcNow
                     CurrencyCardHolder = purchaseAuthReq.CurrencyCardHolder
                     CurrencyMerchant = purchaseAuthReq.CurrencyMerchant
                     Type =
                        match purchaseAuthReq.Action with
                        | AuthorizationStreamAction.Auth ->
                           PurchaseAuthType.Debit
                        | AuthorizationStreamAction.FinancialAuth ->
                           PurchaseAuthType.DebitSMS
                  }

                  let employeeRef =
                     (registry :> IEmployeeActor).EmployeeActor employeeId

                  let msg = EmployeeMessage.AuthorizePurchase purchaseAuth

                  let! (authResult: PurchaseAuthorizationStatus) =
                     employeeRef.Ask(msg, Some(TimeSpan.FromSeconds 4.5))
                     |> Async.map Ok
                     |> AsyncResult.catch Err.NetworkError

                  let response: AuthStreamAccessWebhookResponse = {
                     TransactionId = purchaseAuthReq.CardIssuerTransactionId
                     Result = authResult
                  }

                  return response.AsDTO
               }
               |> TaskResult.teeError (fun err ->
                  let err = exn (string err)
                  ActorUtil.SystemLog.error system err)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.DebitRequest)
   |> ignore

   (*
    * Lithic card issuer will forward events to this webhook handler
    * For local dev use `ngrok http http://localhost:3000` to obtain a url
    * that can be used for handling purchase events.
    * Ex: https://e1677f21bcf6.ngrok-free.app/purchase-card-transaction-updated-webhook
   *)
   app
      .MapPost(
         CardPath.CardTransactionUpdatedWebhook,
         Func<ActorSystem, BankActorRegistry, CardTransactionDTO, Task<IResult>>
            (fun system registry evt ->
               taskResult {
                  let dtoAsEntityMaybe = evt.AsEntity

                  dtoAsEntityMaybe
                  |> Result.teeError (exn >> ActorUtil.SystemLog.error system)
                  |> ignore

                  let! progress =
                     dtoAsEntityMaybe |> Result.mapError Err.UnexpectedError

                  let! cardId, employeeId =
                     getInternalIdsFromIssuerCardId progress.CardIssuerCardId

                  let employeeRef =
                     (registry :> IEmployeeActor).EmployeeActor employeeId

                  employeeRef
                  <! EmployeeMessage.PurchaseProgress(progress, cardId)

                  return ()

               }
               |> TaskResult.teeError (fun err ->
                  let err = exn (string err)
                  ActorUtil.SystemLog.error system err)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.DebitRequest)
   |> ignore

   app
      .MapPost(
         CardPath.PurchaseLimit,
         Func<
            BankActorRegistry,
            ConfigureRollingPurchaseLimitCommand,
            Task<IResult>
          >
            (fun registry cmd ->
               processCommand
                  registry
                  (EmployeeCommand.ConfigureRollingPurchaseLimit cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.UpdatePurchaseLimit)
   |> ignore

   app
      .MapPost(
         CardPath.LockCard,
         Func<BankActorRegistry, LockCardCommand, Task<IResult>>
            (fun registry cmd ->
               processCommand registry (EmployeeCommand.LockCard cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.LockCard)
   |> ignore

   app
      .MapPost(
         CardPath.UnlockCard,
         Func<BankActorRegistry, UnlockCardCommand, Task<IResult>>
            (fun registry cmd ->
               taskResult {
                  let validation =
                     cmd
                     |> UnlockCardCommand.toEvent
                     |> Result.map EmployeeEnvelope.get

                  let! res = validation |> Result.mapError Err.ValidationError

                  let msg =
                     cmd
                     |> UnlockCard
                     |> ApprovableCommand.PerCommand
                     |> OrgMessage.ApprovableRequest
                     |> GuaranteedDelivery.message cmd.OrgId.Value

                  let registry: IOrgGuaranteedDeliveryActor = registry
                  registry.OrgGuaranteedDeliveryActor() <! msg

                  return res
               }
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.UnlockCard)
   |> ignore

   app
      .MapPost(
         CardPath.UpdateNickname,
         Func<BankActorRegistry, EditCardNicknameCommand, Task<IResult>>
            (fun registry cmd ->
               processCommand registry (EmployeeCommand.EditCardNickname cmd)
               |> RouteUtil.unwrapTaskResult)
      )
      .RBAC(Permissions.EditCardNickname)
   |> ignore
