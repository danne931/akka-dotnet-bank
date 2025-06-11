[<RequireQualifiedAccess>]
module KnowYourCustomerServiceActor

open System.Text
open System.Text.Json
open System.Threading.Tasks
open Akkling
open Akkling.Cluster.Sharding
open Akkling.Streams
open Akka.Actor
open Akka.Streams.Amqp.RabbitMq
open FsToolkit.ErrorHandling

open Lib.SharedTypes
open Lib.CircuitBreaker
open Bank.Org.Domain
open SignalRBroadcast
open Lib.Types
open OrgOnboardingSaga

// Onboarding know-your-customer service requests & responses
// are based loosely on middesk.com API.
// TODO: Research & verify actual middesk API

type KYCServiceRequest = {
   name: string
   tax_id: string
   address: {|
      line1: string
      city: string
      state: string
      postal_code: string
   |}
}

type KYCServiceError = {
   code: string
   message: string
} with

   member x.AsDomainError =
      match x.code with
      | "tax_id_invalid" ->
         OrgOnboardingApplicationRequiresUpdateInfo.InvalidEIN
      | "business_name_invalid" ->
         OrgOnboardingApplicationRequiresUpdateInfo.InvalidBusinessName
      | "business_address_invalid" ->
         OrgOnboardingApplicationRequiresUpdateInfo.InvalidAddress
      | other -> OrgOnboardingApplicationRequiresUpdateInfo.Unknown other

type KYCServiceVerificationResponse = {
   registered: bool
   good_standing: bool
   errors: KYCServiceError list
}

let protectedAction
   (networkRequest:
      KYCMessage -> Task<Result<KYCServiceVerificationResponse, Err>>)
   (mailbox: Actor<_>)
   (msg: KYCMessage)
   : TaskResult<KYCServiceVerificationResponse, Err>
   =
   task {
      let! result = networkRequest msg
      return result
   }

let actorProps
   (queueConnection: AmqpConnectionDetails)
   (queueSettings: QueueSettings)
   (streamRestartSettings: Akka.Streams.RestartSettings)
   (breaker: Akka.Pattern.CircuitBreaker)
   (broadcaster: SignalRBroadcast)
   (networkRequest:
      KYCMessage -> Task<Result<KYCServiceVerificationResponse, Err>>)
   (getSagaRef: CorrelationId -> IEntityRef<AppSaga.AppSagaMessage>)
   : Props<obj>
   =
   let consumerQueueOpts
      : Lib.Queue.QueueConsumerOptions<
           KYCMessage,
           KYCMessage,
           KYCServiceVerificationResponse
         > = {
      Service = CircuitBreakerService.KnowYourCustomer
      onCircuitBreakerEvent = broadcaster.circuitBreaker
      protectedAction =
         fun mailbox msg -> protectedAction networkRequest mailbox msg
      queueMessageToActionRequest = fun _ msg -> Task.FromResult(Some msg)
      onSuccessFlow =
         Flow.map
            (fun (mailbox, (queueMessage: KYCMessage), response) ->
               match queueMessage with
               | KYCMessage.VerifyApplication submission ->
                  let corrId = submission.CorrelationId
                  let txnSagaRef = getSagaRef corrId

                  let msg =
                     OrgOnboardingSagaEvent.KYCResponse(Ok())
                     |> AppSaga.Message.orgOnboard submission.OrgId corrId

                  txnSagaRef <! msg

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

let private networkRequestToKYCService (msg: KYCMessage) = taskResult {
   match msg with
   | KYCMessage.VerifyApplication submission ->
      let application = submission.Application

      let request: KYCServiceRequest = {
         name = application.LegalBusinessName
         tax_id = application.EmployerIdentificationNumber
         address = {|
            line1 = "TODO"
            city = "TODO"
            state = "TODO"
            postal_code = "TODO"
         |}
      }

      do! Task.Delay(1300)

      let response: KYCServiceVerificationResponse = {
         registered = true
         good_standing = true
         errors = []
      }

      return response
}

let initProps
   (queueConnection: AmqpConnectionDetails)
   (queueSettings: QueueSettings)
   (streamRestartSettings: Akka.Streams.RestartSettings)
   (breaker: Akka.Pattern.CircuitBreaker)
   (broadcaster: SignalRBroadcast)
   (getSagaRef: CorrelationId -> IEntityRef<AppSaga.AppSagaMessage>)
   : Props<obj>
   =
   actorProps
      queueConnection
      queueSettings
      streamRestartSettings
      breaker
      broadcaster
      networkRequestToKYCService
      getSagaRef

let getProducer (system: ActorSystem) : IActorRef<KYCMessage> =
   Akka.Hosting.ActorRegistry
      .For(system)
      .Get<ActorUtil.ActorMetadata.KYCServiceProducerMarker>()
   |> typed
