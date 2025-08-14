[<RequireQualifiedAccess>]
module EmailServiceActor

open Akka.Streams.Amqp.RabbitMq
open Akkling
open Akkling.Streams
open System
open System.Net.Http
open System.Net.Http.Json
open System.Threading.Tasks
open FsToolkit.ErrorHandling

open Lib.Types
open Lib.Postgres
open Lib.SharedTypes
open Lib.CircuitBreaker
open SignalRBroadcast
open Email
open OrgOnboardingSaga
open EmployeeOnboardingSaga
open CardSetupSaga
open PlatformTransferSaga
open PaymentRequestSaga
open PurchaseSaga
open DomesticTransferSaga
open CachedOrgSettings
open BankActorRegistry

module EmailRequest =
   type PreliminaryT = {
      OrgId: OrgId
      Event: string
      Email: string option
      Data: obj
   }

   type T = {
      OrgId: string
      Event: string
      Email: string
      Data: obj
   }

   let create (o: PreliminaryT) : T option =
      o.Email
      |> Option.map (fun email -> {
         OrgId = string o.OrgId
         Event = o.Event
         Email = email
         Data = o.Data
      })

let private emailPropsFromMessage
   (msg: EmailMessage)
   : EmailRequest.PreliminaryT
   =
   match msg.Info with
   | EmailInfo.OrgOnboardingApplicationSubmitted info -> {
      OrgId = msg.OrgId
      Event = "org-onboarding-application-submitted"
      Email = Some(string info.Email)
      Data = {| name = info.BusinessName |}
     }
   | EmailInfo.OrgOnboardingApplicationAccepted info -> {
      OrgId = msg.OrgId
      Event = "org-onboarding-application-accepted"
      Email = Some(string info.Email)
      Data = {| name = info.BusinessName |}
     }
   | EmailInfo.OrgOnboardingApplicationRejected info -> {
      OrgId = msg.OrgId
      Event = "org-onboarding-application-rejected"
      Email = Some(string info.Info.Email)
      Data = {|
         name = info.Info.BusinessName
         reason = info.Reason
      |}
     }
   | EmailInfo.OrgOnboardingApplicationRequiresRevision info -> {
      OrgId = msg.OrgId
      Event = "org-onboarding-application-requires-revision"
      Email = Some(string info.Info.Email)
      Data = {|
         name = info.Info.BusinessName
         reason = info.Reason
      |}
     }
   | EmailInfo.AccountOpen(accountName) -> {
      OrgId = msg.OrgId
      Event = "account-opened"
      Email = None
      Data = {| name = accountName |}
     }
   | EmailInfo.AccountClose(accountName) -> {
      OrgId = msg.OrgId
      Event = "account-closed"
      Email = None
      Data = {| name = accountName |}
     }
   // TODO: Include link to view statement
   | EmailInfo.BillingStatement -> {
      OrgId = msg.OrgId
      Event = "billing-statement"
      Email = None
      Data = {| |}
     }
   | EmailInfo.Purchase info -> {
      OrgId = msg.OrgId
      Event = "purchase"
      Email = Some(string info.Email)
      Data = {|
         amount = $"${info.Amount}"
         merchant = info.Merchant
         cardNumberLast4 = info.CardNumberLast4
      |}
     }
   | EmailInfo.PurchaseFailed info -> {
      OrgId = msg.OrgId
      Event = "debit-declined"
      Email = Some(string info.Email)
      Data = {| reason = info.Reason.Display |}
     }
   | EmailInfo.InternalTransferBetweenOrgs info -> {
      OrgId = msg.OrgId
      Event = "internal-transfer-between-orgs"
      Email = None
      Data = {|
         sender = info.SenderAccountName
         recipient = info.RecipientBusinessName
         amount = $"${info.Amount}"
         originatedFromPaymentRequest = info.OriginatedFromPaymentRequest.IsSome
      |}
     }
   | EmailInfo.InternalTransferBetweenOrgsDeposited info -> {
      OrgId = msg.OrgId
      Event = "internal-transfer-between-orgs-deposited"
      Email = None
      Data = {|
         sender = info.SenderBusinessName
         recipient = info.RecipientAccountName
         amount = $"${info.Amount}"
         originatedFromPaymentRequest = info.OriginatedFromPaymentRequest.IsSome
      |}
     }
   | EmailInfo.PlatformPaymentRequested info -> {
      OrgId = msg.OrgId
      Event = "platform-payment-requested"
      Email = None
      Data = {|
         payee = info.PayeeBusinessName
         payer = info.PayerBusinessName
         amount = $"${info.Amount}"
      |}
     }
   | EmailInfo.PlatformPaymentReminder info -> {
      OrgId = msg.OrgId
      Event = "platform-payment-reminder"
      Email = None
      Data = {|
         payee = info.PayeeBusinessName
         payer = info.PayerBusinessName
         amount = $"${info.Amount}"
      |}
     }
   | EmailInfo.PlatformPaymentDeclined info -> {
      OrgId = msg.OrgId
      Event = "platform-payment-declined"
      Email = None
      Data = {|
         payee = info.PayeeBusinessName
         payer = info.PayerBusinessName
         amount = $"${info.Amount}"
      |}
     }
   | EmailInfo.ThirdPartyPaymentRequested info -> {
      OrgId = msg.OrgId
      Event = "third-party-payment-requested"
      Email = Some(string info.PayerEmail)
      Data = {|
         payee = info.PayeeBusinessName
         amount = $"${info.Amount}"
         securePaymentFormUrl = info.SecurePaymentFormUrl
      |}
     }
   | EmailInfo.ThirdPartyPaymentReminder info -> {
      OrgId = msg.OrgId
      Event = "third-party-payment-reminder"
      Email = Some(string info.PayerEmail)
      Data = {|
         payee = info.PayeeBusinessName
         amount = $"${info.Amount}"
         securePaymentFormUrl = info.SecurePaymentFormUrl
      |}
     }
   | EmailInfo.DomesticTransfer info -> {
      OrgId = msg.OrgId
      Event = "domestic-transfer"
      Email = None
      Data = {|
         senderAccountName = info.SenderAccountName
         recipientName = info.RecipientName
         amount = $"${info.Amount}"
      |}
     }
   | EmailInfo.ApplicationErrorRequiresSupport(errMsg) -> {
      OrgId = msg.OrgId
      Event = "application-error-requires-support"
      Email = EnvNotifications.config.SupportEmail
      Data = {| error = errMsg |}
     }
   | EmailInfo.EmployeeInvite info -> {
      OrgId = msg.OrgId
      Event = "employee-invite"
      Email = Some(string info.Email)
      Data = {|
         name = info.Name
         // TODO: Domain not configured.
         inviteLink =
            $"localhost:8080{RoutePaths.UserSessionPath.AuthorizeInvite}?token={info.Token.Token}"
      |}
     }
   | EmailInfo.EmployeeOnboardingFail info -> {
      OrgId = msg.OrgId
      Event = "employee-onboarding-fail"
      Email = None
      Data = {|
         name = info.Name
         reason = info.Reason
      |}
     }
   | EmailInfo.CardSetupSuccess info -> {
      OrgId = msg.OrgId
      Event = "card-setup-success"
      Email = Some(string info.EmployeeEmail)
      Data = {| name = info.EmployeeName |}
     }
   | EmailInfo.CardSetupFail info -> {
      OrgId = msg.OrgId
      Event = "card-setup-fail"
      Email = None
      Data = {| name = info.EmployeeName |}
     }
   | EmailInfo.ScheduledTransferInsufficientBalanceWarning info -> {
      OrgId = msg.OrgId
      Event = "scheduled-transfer-low-balance-warning"
      Email = None
      Data = {|
         senderAccountName = info.SenderAccountName
         availableBalance = $"${info.AvailableBalance}"
         scheduledTransfersCount = info.ScheduledTransfersCount
         scheduledTransfersAmount = $"${info.ScheduledTransfersAmount}"
         imminentScheduledTransferDate = info.ImminentScheduledTransferDate
      |}
     }

let private sendEmail
   (client: HttpClient)
   (data: EmailRequest.T)
   : TaskResult<HttpResponseMessage, Err>
   =
   task {
      try
         use! response =
            client.PostAsJsonAsync(
               "track",
               {|
                  orgId = data.OrgId
                  event = data.Event
                  email =
                     EnvNotifications.config.OverrideEmailRecipient
                     |> Option.defaultValue data.Email
                  data = data.Data
               |}
            )

         if not response.IsSuccessStatusCode then
            let! content = response.Content.ReadFromJsonAsync()

            let errMsg =
               $"Error sending email: {response.ReasonPhrase} - {content}"

            return Error(Err.UnexpectedError errMsg)
         else
            return Ok response
      with e ->
         return Error(Err.UnexpectedError e.Message)
   }

let private mockSendEmail _ =
   new HttpResponseMessage() |> Ok |> Task.FromResult

let private createClient (bearerToken: string) =
   let client =
      new HttpClient(BaseAddress = Uri EnvNotifications.config.EmailServiceUri)

   client.DefaultRequestHeaders.Authorization <-
      Headers.AuthenticationHeaderValue("Bearer", bearerToken)

   client

// Formulate an EmailRequest configured with the specified Email
// from the EmailMessage.
// If no specified Email then formulate an EmailRequest for each
// admin of the organization.
let private queueMessageToActionRequest
   (getTeamEmail: Actor<_> -> OrgId -> Task<Result<Email option, Err>>)
   (mailbox: Actor<_>)
   (msg: EmailMessage)
   : EmailRequest.T option Task
   =
   task {
      let preliminaryInfo = emailPropsFromMessage msg
      let orgId = preliminaryInfo.OrgId

      match EmailRequest.create preliminaryInfo with
      | Some email -> return Some email
      | None ->
         let! email =
            getTeamEmail mailbox orgId
            |> TaskResult.map (
               Option.bind (fun email ->
                  EmailRequest.create {
                     preliminaryInfo with
                        Email = Some(string email)
                  })
            )

         return
            match email with
            | Error err ->
               logError mailbox $"Error getting admin email - {err}"
               None
            | Ok emailOpt ->
               emailOpt
               |> Option.teeNone (fun () ->
                  logError
                     mailbox
                     $"Could not retrieve admin email for email message: {msg}")
   }

let onSuccessfulServiceResponse
   (registry: #ISagaActor)
   (mailbox: Actor<_>)
   (msg: EmailMessage)
   =
   let orgId = msg.OrgId
   let corrId = msg.CorrelationId

   let txnSagaMessage =
      match msg.Info with
      | EmailInfo.OrgOnboardingApplicationSubmitted _ ->
         OrgOnboardingSagaEvent.ApplicationProcessingNotificationSent
         |> AppSaga.Message.orgOnboard orgId corrId
         |> Some
      | EmailInfo.OrgOnboardingApplicationAccepted _ ->
         OrgOnboardingSagaEvent.ApplicationAcceptedNotificationSent
         |> AppSaga.Message.orgOnboard orgId corrId
         |> Some
      | EmailInfo.OrgOnboardingApplicationRejected _ ->
         OrgOnboardingSagaEvent.ApplicationRejectedNotificationSent
         |> AppSaga.Message.orgOnboard orgId corrId
         |> Some
      | EmailInfo.OrgOnboardingApplicationRequiresRevision _ ->
         OrgOnboardingSagaEvent.ApplicationRequiresRevisionForKYCServiceNotificationSent
         |> AppSaga.Message.orgOnboard orgId corrId
         |> Some
      | EmailInfo.EmployeeInvite _ ->
         EmployeeOnboardingSagaEvent.InviteNotificationSent
         |> AppSaga.Message.employeeOnboard orgId corrId
         |> Some
      | EmailInfo.EmployeeOnboardingFail _ ->
         EmployeeOnboardingSagaEvent.OnboardingFailNotificationSent
         |> AppSaga.Message.employeeOnboard orgId corrId
         |> Some
      | EmailInfo.CardSetupSuccess _ ->
         CardSetupSagaEvent.CardSetupSuccessNotificationSent
         |> AppSaga.Message.cardSetup orgId corrId
         |> Some
      | EmailInfo.CardSetupFail _ ->
         CardSetupSagaEvent.CardSetupFailNotificationSent
         |> AppSaga.Message.cardSetup orgId corrId
         |> Some
      | EmailInfo.Purchase _ ->
         PurchaseSagaEvent.PurchaseNotificationSent
         |> AppSaga.Message.purchase orgId corrId
         |> Some
      | EmailInfo.DomesticTransfer _ ->
         DomesticTransferSagaEvent.TransferInitiatedNotificationSent
         |> AppSaga.Message.domesticTransfer orgId corrId
         |> Some
      | EmailInfo.InternalTransferBetweenOrgs _ ->
         PlatformTransferSagaEvent.TransferNotificationSent
         |> AppSaga.Message.platformTransfer orgId corrId
         |> Some
      | EmailInfo.InternalTransferBetweenOrgsDeposited _ ->
         PlatformTransferSagaEvent.TransferDepositNotificationSent
         |> AppSaga.Message.platformTransfer orgId corrId
         |> Some
      | EmailInfo.ThirdPartyPaymentRequested _ ->
         PaymentRequestSagaEvent.PaymentRequestNotificationSentToPayer
         |> AppSaga.Message.paymentRequest orgId corrId
         |> Some
      | EmailInfo.PlatformPaymentRequested _ ->
         PaymentRequestSagaEvent.PaymentRequestNotificationSentToPayer
         |> AppSaga.Message.paymentRequest orgId corrId
         |> Some
      | EmailInfo.PlatformPaymentDeclined _ ->
         PaymentRequestSagaEvent.PaymentDeclinedNotificationSentToPayee
         |> AppSaga.Message.paymentRequest orgId corrId
         |> Some
      | EmailInfo.BillingStatement ->
         BillingSaga.BillingSagaEvent.BillingEmailSent
         |> AppSaga.Message.billing orgId corrId
         |> Some
      | _ -> None

   txnSagaMessage |> Option.iter (fun msg -> registry.SagaActor corrId <! msg)

let actorProps
   (queueConnection: AmqpConnectionDetails)
   (queueSettings: QueueEnvConfig)
   (streamRestartSettings: Akka.Streams.RestartSettings)
   (breaker: Akka.Pattern.CircuitBreaker)
   (broadcaster: SignalRBroadcast)
   (getTeamEmail: Actor<_> -> OrgId -> Task<Result<Email option, Err>>)
   registry
   (sendEmail: EmailRequest.T -> TaskResult<HttpResponseMessage, Err>)
   : Props<obj>
   =
   let consumerQueueOpts
      : Lib.Queue.QueueConsumerOptions<
           EmailMessage,
           EmailRequest.T,
           HttpResponseMessage
         > = {
      Service = CircuitBreakerService.Email
      onCircuitBreakerEvent = broadcaster.circuitBreaker
      protectedAction = fun _ emailData -> sendEmail emailData
      queueMessageToActionRequest = queueMessageToActionRequest getTeamEmail
      onSuccessFlow =
         Flow.map
            (fun (mailbox, queueMessage, response) ->
               onSuccessfulServiceResponse registry mailbox queueMessage

               response)
            Flow.id
         |> Some
   }

   Lib.Queue.consumerActorProps
      queueConnection
      queueSettings
      streamRestartSettings
      breaker
      consumerQueueOpts

module Fields = OrganizationSqlMapper.OrgFields
module Reader = OrganizationSqlMapper.OrgSqlReader
module Writer = OrganizationSqlMapper.OrgSqlWriter

let getOrgTeamEmailFromDB (orgId: OrgId) =
   pgQuerySingle<Email>
      $"""
      SELECT {Fields.adminTeamEmail} FROM {OrganizationSqlMapper.table}
      WHERE {Fields.orgId} = @orgId
      """
      (Some [ "orgId", Writer.orgId orgId ])
      Reader.adminTeamEmail

let initProps
   (orgSettingsCache: OrgSettingsCache)
   (breaker: Akka.Pattern.CircuitBreaker)
   (broadcaster: SignalRBroadcast)
   (queueConnection: AmqpConnectionDetails)
   (queueSettings: QueueEnvConfig)
   (streamRestartSettings: Akka.Streams.RestartSettings)
   (bearerToken: string option)
   registry
   =
   let client = bearerToken |> Option.map createClient

   let getTeamEmailFromCacheOrDB (mailbox: Actor<_>) (orgId: OrgId) = taskResult {
      let! foundInCache =
         orgSettingsCache.Get orgId |> AsyncResultOption.map _.AdminTeamEmail

      if foundInCache.IsSome then
         return foundInCache
      else
         logDebug mailbox $"Team email not found in cache. {orgId}"
         return! getOrgTeamEmailFromDB orgId
   }

   let actorProps =
      actorProps
         queueConnection
         queueSettings
         streamRestartSettings
         breaker
         broadcaster
         getTeamEmailFromCacheOrDB
         registry

   match client with
   | Some client -> actorProps (sendEmail client)
   | None when EnvNotifications.config.MockSendingEmail ->
      actorProps mockSendEmail
   | None ->
      let name = $"{queueSettings.Name}-consumer"

      let handler ctx msg =
         match msg with
         | LifecycleEvent e ->
            match e with
            | PreStart ->
               logWarning
                  ctx
                  $"({name}): EmailBearerToken not set. Will not send email"

               ignored ()
            | _ -> ignored ()
         | msg ->
            logError ctx $"({name}) Unknown Message: {msg}"
            unhandled ()

      props (actorOf2 handler)
