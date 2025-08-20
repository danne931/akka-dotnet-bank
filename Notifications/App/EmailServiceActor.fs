[<RequireQualifiedAccess>]
module EmailServiceActor

open Akka.Streams.Amqp.RabbitMq
open Akkling
open Akkling.Streams
open System.Net.Http
open System.Threading.Tasks
open FsToolkit.ErrorHandling

open Lib.Types
open Lib.Postgres
open Lib.SharedTypes
open Lib.CircuitBreaker
open SignalRBroadcast
open EmailMessage
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
open Flurl.Http

type EmailRequest = {
   OrgId: OrgId
   Event: string
   Email: Email | null
   Data: obj
}

let private emailPropsFromMessage (msg: EmailMessage) : EmailRequest =
   match msg.Info with
   | EmailInfo.OrgOnboardingApplicationSubmitted info -> {
      OrgId = msg.OrgId
      Event = "org-onboarding-application-submitted"
      Email = info.Email
      Data = {| name = info.BusinessName |}
     }
   | EmailInfo.OrgOnboardingApplicationAccepted info -> {
      OrgId = msg.OrgId
      Event = "org-onboarding-application-accepted"
      Email = info.Email
      Data = {| name = info.BusinessName |}
     }
   | EmailInfo.OrgOnboardingApplicationRejected info -> {
      OrgId = msg.OrgId
      Event = "org-onboarding-application-rejected"
      Email = info.Info.Email
      Data = {|
         name = info.Info.BusinessName
         reason = info.Reason
      |}
     }
   | EmailInfo.OrgOnboardingApplicationRequiresRevision info -> {
      OrgId = msg.OrgId
      Event = "org-onboarding-application-requires-revision"
      Email = info.Info.Email
      Data = {|
         name = info.Info.BusinessName
         reason = info.Reason
      |}
     }
   | EmailInfo.AccountOpen(accountName) -> {
      OrgId = msg.OrgId
      Event = "account-opened"
      Email = null
      Data = {| name = accountName |}
     }
   | EmailInfo.AccountClose(accountName) -> {
      OrgId = msg.OrgId
      Event = "account-closed"
      Email = null
      Data = {| name = accountName |}
     }
   // TODO: Include link to view statement
   | EmailInfo.BillingStatement -> {
      OrgId = msg.OrgId
      Event = "billing-statement"
      Email = null
      Data = {| |}
     }
   | EmailInfo.Purchase info -> {
      OrgId = msg.OrgId
      Event = "purchase"
      Email = info.Email
      Data = {|
         amount = $"${info.Amount}"
         merchant = info.Merchant
         cardNumberLast4 = info.CardNumberLast4
      |}
     }
   | EmailInfo.PurchaseFailed info -> {
      OrgId = msg.OrgId
      Event = "debit-declined"
      Email = info.Email
      Data = {| reason = info.Reason |}
     }
   | EmailInfo.InternalTransferBetweenOrgs info -> {
      OrgId = msg.OrgId
      Event = "internal-transfer-between-orgs"
      Email = null
      Data = {|
         sender = info.SenderAccountName
         recipient = info.RecipientBusinessName
         amount = $"${info.Amount}"
      |}
     }
   | EmailInfo.InternalTransferBetweenOrgsDeposited info -> {
      OrgId = msg.OrgId
      Event = "internal-transfer-between-orgs-deposited"
      Email = null
      Data = {|
         sender = info.SenderBusinessName
         recipient = info.RecipientAccountName
         amount = $"${info.Amount}"
      |}
     }
   | EmailInfo.PlatformPaymentRequested info -> {
      OrgId = msg.OrgId
      Event = "platform-payment-requested"
      Email = null
      Data = {|
         payee = info.PayeeBusinessName
         payer = info.PayerBusinessName
         amount = $"${info.Amount}"
      |}
     }
   | EmailInfo.PlatformPaymentReminder info -> {
      OrgId = msg.OrgId
      Event = "platform-payment-reminder"
      Email = null
      Data = {|
         payee = info.PayeeBusinessName
         payer = info.PayerBusinessName
         amount = $"${info.Amount}"
      |}
     }
   | EmailInfo.PlatformPaymentDeclined info -> {
      OrgId = msg.OrgId
      Event = "platform-payment-declined"
      Email = null
      Data = {|
         payee = info.PayeeBusinessName
         payer = info.PayerBusinessName
         amount = $"${info.Amount}"
      |}
     }
   | EmailInfo.ThirdPartyPaymentRequested info -> {
      OrgId = msg.OrgId
      Event = "third-party-payment-requested"
      Email = info.PayerEmail
      Data = {|
         payee = info.PayeeBusinessName
         amount = $"${info.Amount}"
         securePaymentFormUrl = info.SecurePaymentFormUrl
      |}
     }
   | EmailInfo.ThirdPartyPaymentReminder info -> {
      OrgId = msg.OrgId
      Event = "third-party-payment-reminder"
      Email = info.PayerEmail
      Data = {|
         payee = info.PayeeBusinessName
         amount = $"${info.Amount}"
         securePaymentFormUrl = info.SecurePaymentFormUrl
      |}
     }
   | EmailInfo.DomesticTransfer info -> {
      OrgId = msg.OrgId
      Event = "domestic-transfer"
      Email = null
      Data = {|
         senderAccountName = info.SenderAccountName
         recipientName = info.RecipientName
         amount = $"${info.Amount}"
      |}
     }
   | EmailInfo.ApplicationErrorRequiresSupport(errMsg) -> {
      OrgId = msg.OrgId
      Event = "application-error-requires-support"
      Email =
         match EnvNotifications.config.SupportEmail with
         | None -> null
         | Some email -> email
      Data = {| error = errMsg |}
     }
   | EmailInfo.EmployeeInvite info -> {
      OrgId = msg.OrgId
      Event = "employee-invite"
      Email = info.Email
      Data = {|
         name = info.Name
         // TODO: Domain not configured.
         inviteLink =
            $"localhost:8080{RoutePaths.UserSessionPath.AuthorizeInvite}?token={info.Token}"
      |}
     }
   | EmailInfo.EmployeeOnboardingFail info -> {
      OrgId = msg.OrgId
      Event = "employee-onboarding-fail"
      Email = null
      Data = {|
         name = info.Name
         reason = info.Reason
      |}
     }
   | EmailInfo.CardSetupSuccess info -> {
      OrgId = msg.OrgId
      Event = "card-setup-success"
      Email = info.EmployeeEmail
      Data = {| name = info.EmployeeName |}
     }
   | EmailInfo.CardSetupFail info -> {
      OrgId = msg.OrgId
      Event = "card-setup-fail"
      Email = null
      Data = {| name = info.EmployeeName |}
     }
   | EmailInfo.ScheduledTransferInsufficientBalanceWarning info -> {
      OrgId = msg.OrgId
      Event = "scheduled-transfer-low-balance-warning"
      Email = null
      Data = {|
         senderAccountName = info.SenderAccountName
         availableBalance = $"${info.AvailableBalance}"
         scheduledTransfersCount = info.ScheduledTransfersCount
         scheduledTransfersAmount = $"${info.ScheduledTransfersAmount}"
         imminentScheduledTransferDate = info.ImminentScheduledTransferDate
      |}
     }

let private sendEmail
   (bearerToken: string)
   (data: EmailRequest)
   : TaskResult<HttpResponseMessage, Err>
   =
   task {
      try
         let email =
            EnvNotifications.config.OverrideEmailRecipient
            |> Option.defaultValue data.Email

         let url = EnvNotifications.config.EmailServiceUri + "/track"

         let data = {|
            orgId = string data.OrgId
            event = data.Event
            email = string email
            data = data.Data
         |}

         let! response =
            url.WithOAuthBearerToken(bearerToken).PostJsonAsync data

         if not response.ResponseMessage.IsSuccessStatusCode then
            let! content = response.GetStringAsync()

            let errMsg =
               $"Error sending email: {response.StatusCode} - {content}"

            return Error(Err.UnexpectedError errMsg)
         else
            return Ok response.ResponseMessage
      with e ->
         return Error(Err.UnexpectedError e.Message)
   }

let private mockSendEmail _ =
   new HttpResponseMessage() |> Ok |> Task.FromResult

// Formulate an EmailRequest configured with the specified Email
// from the EmailMessage.
// If no specified Email then formulate an EmailRequest for each
// admin of the organization.
let private queueMessageToActionRequest
   (getTeamEmail: Actor<_> -> OrgId -> Task<Result<Email option, Err>>)
   (mailbox: Actor<_>)
   (msg: EmailMessage)
   : EmailRequest option Task
   =
   task {
      let info = emailPropsFromMessage msg
      let orgId = info.OrgId

      if not (isNull info.Email) then
         return Some info
      else
         let! email =
            getTeamEmail mailbox orgId
            |> TaskResultOption.map (fun email -> { info with Email = email })

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
   (sendEmail: EmailRequest -> TaskResult<HttpResponseMessage, Err>)
   : Props<obj>
   =
   let consumerQueueOpts
      : Lib.Queue.QueueConsumerOptions<
           EmailMessage,
           EmailRequest,
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

open OrganizationSqlMapper

let private getOrgTeamEmailFromDB (orgId: OrgId) =
   pgQuerySingle<Email>
      $"""
      SELECT {OrgFields.adminTeamEmail} FROM {table}
      WHERE {OrgFields.orgId} = @orgId
      """
      (Some [ "orgId", OrgSqlWriter.orgId orgId ])
      OrgSqlReader.adminTeamEmail

let private getTeamEmailFromCacheOrDB
   (orgSettingsCache: OrgSettingsCache)
   (mailbox: Actor<_>)
   (orgId: OrgId)
   =
   taskResult {
      let! foundInCache =
         orgSettingsCache.Get orgId |> AsyncResultOption.map _.AdminTeamEmail

      if foundInCache.IsSome then
         return foundInCache
      else
         logDebug mailbox $"Team email not found in cache. {orgId}"
         return! getOrgTeamEmailFromDB orgId
   }

// Will not consume email messages off of RabbitMq if no EmailBearerToken
// configured for interaction with third party email provider.
let private idleActor () =
   let handler ctx msg =
      match msg with
      | LifecycleEvent e ->
         match e with
         | PreStart ->
            logWarning ctx $"EmailBearerToken not set. Will not send email"

            ignored ()
         | _ -> ignored ()
      | msg ->
         logError ctx $"Unknown Message: {msg}"
         unhandled ()

   props (actorOf2 handler)

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
   let actorProps =
      actorProps
         queueConnection
         queueSettings
         streamRestartSettings
         breaker
         broadcaster
         (getTeamEmailFromCacheOrDB orgSettingsCache)
         registry

   match EnvNotifications.config.MockSendingEmail, bearerToken with
   | true, _ -> actorProps mockSendEmail
   | false, None -> idleActor ()
   | false, Some token -> actorProps (sendEmail token)
