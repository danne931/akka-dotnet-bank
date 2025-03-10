[<RequireQualifiedAccess>]
module EmailConsumerActor

open Akka.Actor
open Akka.Streams.Amqp.RabbitMq
open Akkling
open System
open System.Net.Http
open System.Net.Http.Json
open System.Threading.Tasks
open FsToolkit.ErrorHandling

open Lib.Types
open Lib.Postgres
open Lib.SharedTypes
open Bank.Employee.Domain
open SignalRBroadcast
open Email

let mutable cnt = 0

module private EmailRequest =
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
   match msg with
   | EmailMessage.AccountOpen(accountName, orgId) -> {
      OrgId = orgId
      Event = "account-opened"
      Email = None
      Data = {| name = accountName |}
     }
   | EmailMessage.AccountClose(accountName, orgId) -> {
      OrgId = orgId
      Event = "account-closed"
      Email = None
      Data = {| name = accountName |}
     }
   // TODO: Include link to view statement
   | EmailMessage.BillingStatement(accountName, orgId) -> {
      OrgId = orgId
      Event = "billing-statement"
      Email = None
      Data = {| name = accountName |}
     }
   | EmailMessage.PurchaseFailed info -> {
      OrgId = info.OrgId
      Event = "debit-declined"
      Email = Some(string info.Email)
      Data = {|
         reason = PurchaseFailReason.display info.Reason
      |}
     }
   | EmailMessage.InternalTransferBetweenOrgsDeposited info -> {
      OrgId = info.OrgId
      Event = "transfer-deposited"
      Email = None
      Data = {|
         name = info.AccountName
         amount = $"${info.Amount}"
         origin = info.SenderBusinessName
      |}
     }
   | EmailMessage.ApplicationErrorRequiresSupport(errMsg, orgId) -> {
      OrgId = orgId
      Event = "application-error-requires-support"
      Email = EnvNotifications.config.SupportEmail
      Data = {| error = errMsg |}
     }
   | EmailMessage.EmployeeInvite info -> {
      OrgId = info.OrgId
      Event = "employee-invite"
      Email = Some(string info.Email)
      Data = {|
         name = info.Name
         // TODO: Domain not configured.
         inviteLink =
            $"localhost:8080{RoutePaths.UserSessionPath.AuthorizeInvite}?token={info.Token.Token}"
      |}
     }

// NOTE
// Raise an exception instead of returning Result.Error to trip circuit breaker.
let private sendEmail (client: HttpClient) (data: EmailRequest.T) = task {
   use! response =
      client.PostAsJsonAsync(
         "track",
         {|
            orgId = data.OrgId
            event = data.Event
            email = data.Email
            data = data.Data
         |}
      )

   if not response.IsSuccessStatusCode then
      let! content = response.Content.ReadFromJsonAsync()

      return
         failwith $"Error sending email: {response.ReasonPhrase} - {content}"
   else
      return response
}

let private createClient (bearerToken: string) =
   let client =
      new HttpClient(BaseAddress = Uri(EnvNotifications.config.EmailServiceUri))

   client.DefaultRequestHeaders.Authorization <-
      Headers.AuthenticationHeaderValue("Bearer", bearerToken)

   client

let private queueMessageToActionRequest
   (getAdminEmailsForOrg: OrgId -> Task<Result<Email list option, Err>>)
   (mailbox: Actor<_>)
   (msg: EmailMessage)
   =
   task {
      let preliminaryInfo = emailPropsFromMessage msg

      match EmailRequest.create preliminaryInfo with
      | Some evt -> return Some [ evt ]
      | None ->
         let! emailsToSendToAdmins =
            getAdminEmailsForOrg preliminaryInfo.OrgId
            |> TaskResultOption.map (
               List.choose (fun email ->
                  EmailRequest.create {
                     preliminaryInfo with
                        Email = Some(string email)
                  })
            )

         match emailsToSendToAdmins with
         | Error err ->
            logError mailbox $"Error getting admin emails - {err}"
            return None
         | Ok None ->
            logError
               mailbox
               $"Could not retrieve admin emails for email message: {msg}"

            return None
         | Ok(Some emails) -> return Some emails
   }

let actorProps
   (queueConnection: AmqpConnectionDetails)
   (queueSettings: QueueSettings)
   (streamRestartSettings: Akka.Streams.RestartSettings)
   (breaker: Akka.Pattern.CircuitBreaker)
   (broadcaster: SignalRBroadcast)
   (client: HttpClient)
   (getAdminEmailsForOrg: OrgId -> Task<Result<Email list option, Err>>)
   : Props<obj>
   =
   let consumerQueueOpts
      : Lib.Queue.QueueConsumerOptions<EmailMessage, EmailRequest.T> = {
      Service = CircuitBreakerService.Email
      onCircuitBreakerEvent = broadcaster.circuitBreaker
      protectedAction =
         fun _ emailData ->
            cnt <- cnt + 1
            sendEmail client emailData |> Task.map ignore
      queueMessageToActionRequests =
         queueMessageToActionRequest getAdminEmailsForOrg
   }

   Lib.Queue.consumerActorProps
      queueConnection
      queueSettings
      streamRestartSettings
      breaker
      consumerQueueOpts

module Fields = EmployeeSqlMapper.EmployeeFields
module Reader = EmployeeSqlMapper.EmployeeSqlReader
module Writer = EmployeeSqlMapper.EmployeeSqlWriter

let getAdminEmailsForOrg (orgId: OrgId) =
   let roleTypecast = EmployeeSqlMapper.EmployeeTypeCast.role
   let statusTypecast = EmployeeSqlMapper.EmployeeTypeCast.status

   pgQuery<Email>
      $"""
      SELECT {Fields.email} FROM {EmployeeSqlMapper.table}
      WHERE
         {Fields.orgId} = @orgId
         AND {Fields.role} = @role::{roleTypecast}
         AND {Fields.status} = @status::{statusTypecast}
      """
      (Some [
         "orgId", Writer.orgId orgId
         "role", Writer.role Role.Admin
         "status", Writer.status EmployeeStatus.Active
      ])
      Reader.email

let initProps
   (breaker: Akka.Pattern.CircuitBreaker)
   (broadcaster: SignalRBroadcast)
   (queueConnection: AmqpConnectionDetails)
   (queueSettings: QueueSettings)
   (streamRestartSettings: Akka.Streams.RestartSettings)
   (bearerToken: string option)
   =
   let client = bearerToken |> Option.map createClient

   match client with
   | Some client ->
      actorProps
         queueConnection
         queueSettings
         streamRestartSettings
         breaker
         broadcaster
         client
         getAdminEmailsForOrg
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
