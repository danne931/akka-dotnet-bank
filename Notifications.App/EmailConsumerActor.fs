[<RequireQualifiedAccess>]
module EmailConsumerActor

open Akka.Actor
open Akka.Streams.Amqp.RabbitMq
open Akkling
open Akkling.Streams
open Akkling.Cluster.Sharding
open System
open System.Net.Http
open System.Net.Http.Json
open System.Threading.Tasks
open FsToolkit.ErrorHandling

open Lib.Types
open Lib.Postgres
open Lib.SharedTypes
open SignalRBroadcast
open Email
open Lib.Saga
open OrgOnboardingSaga
open EmployeeOnboardingSaga
open CardSetupSaga
open PlatformTransferSaga
open PlatformPaymentSaga
open PurchaseSaga
open DomesticTransferSaga
open BillingSaga

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
   | EmailInfo.PlatformPaymentPaid info -> {
      OrgId = msg.OrgId
      Event = "platform-payment-paid"
      Email = None
      Data = {|
         payee = info.PayeeBusinessName
         payer = info.PayerBusinessName
         amount = $"${info.Amount}"
      |}
     }
   | EmailInfo.PlatformPaymentDeposited info -> {
      OrgId = msg.OrgId
      Event = "platform-payment-deposited"
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
      new HttpClient(BaseAddress = Uri(EnvNotifications.config.EmailServiceUri))

   client.DefaultRequestHeaders.Authorization <-
      Headers.AuthenticationHeaderValue("Bearer", bearerToken)

   client

// Formulate an EmailRequest configured with the specified Email
// from the EmailMessage.
// If no specified Email then formulate an EmailRequest for each
// admin of the organization.
let private queueMessageToActionRequest
   (getOrgTeamEmail: OrgId -> Task<Result<Email option, Err>>)
   (mailbox: Actor<_>)
   (msg: EmailMessage)
   : EmailRequest.T option Task
   =
   task {
      let preliminaryInfo = emailPropsFromMessage msg

      match EmailRequest.create preliminaryInfo with
      | Some email -> return Some email
      | None ->
         let! email =
            getOrgTeamEmail preliminaryInfo.OrgId
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
   (getSagaRef: CorrelationId -> IEntityRef<SagaMessage<AppSaga.Event>>)
   (mailbox: Actor<_>)
   (msg: EmailMessage)
   =
   let txnSagaEvt =
      match msg.Info with
      | EmailInfo.OrgOnboardingApplicationSubmitted _ ->
         OrgOnboardingSagaEvent.ApplicationProcessingNotificationSent
         |> AppSaga.Event.OrgOnboarding
         |> Some
      | EmailInfo.OrgOnboardingApplicationAccepted _ ->
         OrgOnboardingSagaEvent.ApplicationAcceptedNotificationSent
         |> AppSaga.Event.OrgOnboarding
         |> Some
      | EmailInfo.OrgOnboardingApplicationRejected _ ->
         OrgOnboardingSagaEvent.ApplicationRejectedNotificationSent
         |> AppSaga.Event.OrgOnboarding
         |> Some
      | EmailInfo.OrgOnboardingApplicationRequiresRevision _ ->
         OrgOnboardingSagaEvent.ApplicationRequiresRevisionForKYCServiceNotificationSent
         |> AppSaga.Event.OrgOnboarding
         |> Some
      | EmailInfo.EmployeeInvite _ ->
         EmployeeOnboardingSagaEvent.InviteNotificationSent
         |> AppSaga.Event.EmployeeOnboarding
         |> Some
      | EmailInfo.EmployeeOnboardingFail _ ->
         EmployeeOnboardingSagaEvent.OnboardingFailNotificationSent
         |> AppSaga.Event.EmployeeOnboarding
         |> Some
      | EmailInfo.CardSetupSuccess _ ->
         CardSetupSagaEvent.CardSetupSuccessNotificationSent
         |> AppSaga.Event.CardSetup
         |> Some
      | EmailInfo.CardSetupFail _ ->
         CardSetupSagaEvent.CardSetupFailNotificationSent
         |> AppSaga.Event.CardSetup
         |> Some
      | EmailInfo.Purchase _ ->
         PurchaseSagaEvent.PurchaseNotificationSent
         |> AppSaga.Event.Purchase
         |> Some
      | EmailInfo.DomesticTransfer _ ->
         DomesticTransferSagaEvent.TransferInitiatedNotificationSent
         |> AppSaga.Event.DomesticTransfer
         |> Some
      | EmailInfo.InternalTransferBetweenOrgs _ ->
         PlatformTransferSagaEvent.TransferNotificationSent
         |> AppSaga.Event.PlatformTransfer
         |> Some
      | EmailInfo.InternalTransferBetweenOrgsDeposited _ ->
         PlatformTransferSagaEvent.TransferDepositNotificationSent
         |> AppSaga.Event.PlatformTransfer
         |> Some
      | EmailInfo.PlatformPaymentRequested _ ->
         PlatformPaymentSagaEvent.PaymentRequestNotificationSentToPayer
         |> AppSaga.Event.PlatformPayment
         |> Some
      | EmailInfo.PlatformPaymentPaid _ ->
         PlatformPaymentSagaEvent.PaymentPaidNotificationSentToPayer
         |> AppSaga.Event.PlatformPayment
         |> Some
      | EmailInfo.PlatformPaymentDeposited _ ->
         PlatformPaymentSagaEvent.PaymentDepositedNotificationSentToPayee
         |> AppSaga.Event.PlatformPayment
         |> Some
      | EmailInfo.PlatformPaymentDeclined _ ->
         PlatformPaymentSagaEvent.PaymentDeclinedNotificationSentToPayee
         |> AppSaga.Event.PlatformPayment
         |> Some
      | EmailInfo.BillingStatement ->
         BillingSaga.BillingSagaEvent.BillingEmailSent
         |> AppSaga.Event.Billing
         |> Some
      | _ -> None

   match txnSagaEvt with
   | Some evt ->
      let sagaMsg =
         SagaEvent.create msg.OrgId msg.CorrelationId evt |> SagaMessage.Event

      getSagaRef msg.CorrelationId <! sagaMsg
   | None -> ()

let actorProps
   (queueConnection: AmqpConnectionDetails)
   (queueSettings: QueueSettings)
   (streamRestartSettings: Akka.Streams.RestartSettings)
   (breaker: Akka.Pattern.CircuitBreaker)
   (broadcaster: SignalRBroadcast)
   (getTeamEmailForOrg: OrgId -> Task<Result<Email option, Err>>)
   (getSagaRef: CorrelationId -> IEntityRef<SagaMessage<AppSaga.Event>>)
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
      queueMessageToActionRequest =
         queueMessageToActionRequest getTeamEmailForOrg
      onSuccessFlow =
         Flow.map
            (fun (mailbox, queueMessage, response) ->
               onSuccessfulServiceResponse getSagaRef mailbox queueMessage

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

let getOrgTeamEmail (orgId: OrgId) =
   pgQuerySingle<Email>
      $"""
      SELECT {Fields.adminTeamEmail} FROM {OrganizationSqlMapper.table}
      WHERE {Fields.orgId} = @orgId
      """
      (Some [ "orgId", Writer.orgId orgId ])
      Reader.adminTeamEmail

let initProps
   (breaker: Akka.Pattern.CircuitBreaker)
   (broadcaster: SignalRBroadcast)
   (queueConnection: AmqpConnectionDetails)
   (queueSettings: QueueSettings)
   (streamRestartSettings: Akka.Streams.RestartSettings)
   (bearerToken: string option)
   (getSagaRef: CorrelationId -> IEntityRef<SagaMessage<AppSaga.Event>>)
   =
   let client = bearerToken |> Option.map createClient

   let actorProps =
      actorProps
         queueConnection
         queueSettings
         streamRestartSettings
         breaker
         broadcaster
         getOrgTeamEmail
         getSagaRef

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

let getProducer (system: ActorSystem) : IActorRef<Email.EmailMessage> =
   Akka.Hosting.ActorRegistry
      .For(system)
      .Get<ActorUtil.ActorMetadata.EmailProducerMarker>()
   |> typed

let getProducerProxy (system: ActorSystem) : IActorRef<Email.EmailMessage> =
   Akka.Hosting.ActorRegistry
      .For(system)
      .Get<ActorUtil.ActorMetadata.EmailProxyMarker>()
   |> typed
