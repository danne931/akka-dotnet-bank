[<RequireQualifiedAccess>]
module EmailConsumerActor

open Akka.Actor
open Akka.Streams.Amqp.RabbitMq
open Akka.Streams.Amqp.RabbitMq.Dsl
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
open Bank.Employee.Domain
open SignalRBroadcast
open Email

type private CircuitBreakerMessage =
   | BreakerHalfOpen
   | BreakerClosed

module private TrackingEvent =
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
   : TrackingEvent.PreliminaryT
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

let mutable cnt = 0

// NOTE
// Raise an exception instead of returning Result.Error to trip circuit breaker.
let private sendEmail (client: HttpClient) (data: TrackingEvent.T) = task {
   if cnt > 1 then
      let res = new HttpResponseMessage(Net.HttpStatusCode.OK)
      printfn "res %A" res

      res.Content <-
         new StringContent(
            "DINO",
            System.Text.Encoding.UTF8,
            "application/json"
         )

      printfn "content %A" res.Content
      return res
   else
      failwith "fail then get back up"

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
         failwith $"Error sending email: {response.ReasonPhrase} - {content}"

      return response
}

let private createClient (bearerToken: string) =
   let client =
      new HttpClient(BaseAddress = Uri(EnvNotifications.config.EmailServiceUri))

   client.DefaultRequestHeaders.Authorization <-
      Headers.AuthenticationHeaderValue("Bearer", bearerToken)

   client

let deserializeFromRabbit
   (system: ActorSystem)
   (msg: CommittableIncomingMessage)
   =
   try
      let serializer =
         system.Serialization.FindSerializerForType(typeof<EmailMessage>)

      Ok <| serializer.FromBinary<EmailMessage>(msg.Message.Bytes.ToArray())
   with ex ->
      Error ex

let mutable attemptEmailCount = 0
let mutable goodEmailCount = 0

let initEmailSink
   (queueConnection: AmqpConnectionDetails)
   (queueSettings: RabbitQueueSettings)
   (breaker: Akka.Pattern.CircuitBreaker)
   (client: HttpClient option)
   (getAdminEmailsForOrg: OrgId -> Task<Result<Email list option, Err>>)
   (killSwitch: Akka.Streams.SharedKillSwitch)
   (mailbox: Actor<obj>)
   parallelism
   =
   let materializer = mailbox.System.Materializer()

   let sendEmailWithCircuitBreaker
      client
      (committable: CommittableIncomingMessage)
      emailData
      =
      breaker.WithCircuitBreaker(fun () -> task {
         try
            let! res = sendEmail client emailData
            do! committable.Ack()
            return res
         with e ->
            do! committable.Nack()
            return failwith e.Message
      })
      |> Async.AwaitTask

   let source =
      let settings =
         Lib.Rabbit.createSourceSettings queueSettings.Name queueConnection

      AmqpSource.CommittableSource(settings, bufferSize = parallelism)

   let sink =
      Sink.onComplete (function
         | None -> ()
         | Some err ->
            logError mailbox $"Notifications stream completed with error: {err}")

   source
   |> Source.viaMat (killSwitch.Flow()) Keep.none
   |> Source.asyncMapUnordered parallelism (fun committable -> async {
      let nack = committable.Nack >> Async.AwaitTask

      if breaker.IsOpen then
         logWarning mailbox "Circuit breaker open - will try again later"
         do! nack ()
         return None
      else
         match deserializeFromRabbit mailbox.System committable with
         | Error err ->
            logError
               mailbox
               $"Error deserializing EmailMessage from RabbitMq {err}"

            do! nack ()
            return None
         | Ok msg ->
            let preliminaryData = emailPropsFromMessage msg

            match TrackingEvent.create preliminaryData, client with
            | _, None ->
               logWarning
                  mailbox
                  "EmailBearerToken not set. Will not send email."

               do! nack ()
               return None
            | Some info, Some client ->
               return Some [ committable, client, info ]
            | None, Some client ->
               let! emailsToSendToAdmins =
                  getAdminEmailsForOrg preliminaryData.OrgId
                  |> Async.AwaitTask
                  |> AsyncResultOption.map (
                     List.choose (fun email ->
                        TrackingEvent.create {
                           preliminaryData with
                              Email = Some(string email)
                        })
                  )

               match emailsToSendToAdmins with
               | Error err ->
                  logError mailbox $"Error getting admin emails - {err}"
                  do! nack ()
                  return None
               | Ok None ->
                  logError
                     mailbox
                     $"Could not retrieve admin emails for email message: {msg}"

                  do! nack ()
                  return None
               | Ok(Some emails) ->
                  return
                     Some [ for info in emails -> committable, client, info ]
   })
   // Filter out items that were nacked due to circuit breaker being open,
   // errors getting admin emails, or no EmailBearerToken set, etc.
   |> Source.choose id
   // Flatten (EmailInfo list) list to EmailInfo list.
   |> Source.collect id
   // Send email with circuit breaker integration.  If the email fails to
   // send then the message will be Nacked and attempted later when the
   // the breaker transition to HalfOpen or Closed.
   |> Source.asyncMapUnordered parallelism (fun (committable, client, info) -> async {
      if breaker.IsOpen then
         logWarning mailbox "Circuit breaker open - will try again later"
         do! committable.Nack() |> Async.AwaitTask
      else
         try
            let! res = sendEmailWithCircuitBreaker client committable info
            goodEmailCount <- goodEmailCount + 1
            printfn "good %A" goodEmailCount
         with e ->
            printfn "bad %A" e

         printfn "%A ATTEMPT EMAIL COUNT" attemptEmailCount
         attemptEmailCount <- attemptEmailCount + 1

      return 100
   })
   |> Source.toMat sink Keep.none
   |> Graph.run materializer

let actorProps
   (breaker: Akka.Pattern.CircuitBreaker)
   (getAdminEmailsForOrg: OrgId -> Task<Result<Email list option, Err>>)
   (broadcaster: SignalRBroadcast)
   (rabbitConnection: AmqpConnectionDetails)
   (queueSettings: RabbitQueueSettings)
   (bearerToken: string option)
   =
   let client = Some(createClient "test-invalid-token")
   //let client = bearerToken |> Option.map createClient

   let createKillSwitch () =
      KillSwitch.shared "CircuitBreakerOpen.Email"

   let mutable killSwitch = createKillSwitch ()

   let initStream =
      initEmailSink
         rabbitConnection
         queueSettings
         breaker
         client
         getAdminEmailsForOrg

   let rec init (ctx: Actor<obj>) = actor {
      let! msg = ctx.Receive()
      printfn "MSG RECEIVED in init %A" msg

      match msg with
      | LifecycleEvent e ->
         match e with
         | PreStart ->
            logInfo ctx "Prestart - Initialize EmailActor Queue Source"

            breaker.OnHalfOpen(fun () ->
               cnt <- cnt + 1

               ctx.Self <! BreakerHalfOpen

               broadcaster.circuitBreaker {
                  Service = CircuitBreakerService.Email
                  Status = CircuitBreakerStatus.HalfOpen
                  Timestamp = DateTime.UtcNow
               })
            |> ignore

            breaker.OnClose(fun () ->
               ctx.Self <! BreakerClosed

               broadcaster.circuitBreaker {
                  Service = CircuitBreakerService.Email
                  Status = CircuitBreakerStatus.Closed
                  Timestamp = DateTime.UtcNow
               })
            |> ignore

            breaker.OnOpen(fun () ->
               logWarning ctx "Breaker open - pause processing"

               broadcaster.circuitBreaker {
                  Service = CircuitBreakerService.Email
                  Status = CircuitBreakerStatus.Open
                  Timestamp = DateTime.UtcNow
               })
            |> ignore

            return! processing ctx queueSettings.MaxParallelism
         | _ -> return ignored ()
      | msg ->
         logError ctx $"Unknown msg {msg}"
         return unhandled ()
   }

   and processing (ctx: Actor<obj>) (limit: int) =
      initStream killSwitch ctx limit

      actor {
         let! msg = ctx.Receive()

         match msg with
         | LifecycleEvent e ->
            printfn "lifecycle event down below %A" e

            match e with
            | PostStop ->
               printfn "POSTSTOP email consumer"
               //queue.Complete()
               return! init ctx
            | _ -> return ignored ()
         | :? CircuitBreakerMessage as msg ->
            match msg with
            | BreakerHalfOpen ->
               logWarning ctx "Breaker half open - try processing one"
               return! processing ctx 1
            | BreakerClosed ->
               logWarning ctx "Breaker closed - resume processing"
               return! processing ctx queueSettings.MaxParallelism
         | msg ->
            logError ctx $"Unknown msg {msg}"
            return unhandled ()
      }

   props init

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
   (queueSettings: RabbitQueueSettings)
   (bearerToken: string option)
   =
   actorProps
      breaker
      getAdminEmailsForOrg
      broadcaster
      queueConnection
      queueSettings
      bearerToken
