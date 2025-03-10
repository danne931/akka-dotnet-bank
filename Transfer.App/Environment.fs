[<RequireQualifiedAccess>]
module EnvTransfer

open System
open System.Net
open FsConfig

open Lib.Types

let builder = Env.builder

type DomesticTransferRouter = { MaxInstancesPerNode: int }

type MockDomesticTransferProcessor = { Host: IPAddress; Port: int }

// NOTE:
// These settings apply to DomesticTransfer actors created
// by the round robin pool router on a given account node.
// A cluster singleton actor existing within the account cluster
// creates a circuit breaker instance and forwards the
// DomesticTransferMessage messages to the routees.
// The routees process transfer requests and progress checks
// by making TCP requests to a "MockDomesticTransferProcessor",
// a separate dotnet server which currently exists to mock
// processing of ACH transfers (& which will likely be replaced
// once Plaid is integrated)
type private TransferConfigInput = {
   MockDomesticTransferProcessor: {| Host: string option; Port: int |}
   DomesticTransferRouter: {| MaxInstancesPerNode: int option |}
   DomesticTransferCircuitBreaker: {|
      MaxFailures: int option
      CallTimeoutSeconds: int option
      ResetTimeoutSeconds: int option
   |}
   TransferProgressTrackingThrottle: Env.StreamThrottleInput
   TransferProgressLookbackMinutes: int option
   DomesticTransferQueue: {|
      Name: string option
      MaxParallelism: int option
   |}
}

type TransferConfig = {
   MockDomesticTransferProcessor: MockDomesticTransferProcessor
   DomesticTransferRouter: DomesticTransferRouter
   domesticTransferCircuitBreaker:
      Akka.Actor.ActorSystem -> Akka.Pattern.CircuitBreaker
   TransferProgressTrackingThrottle: StreamThrottle
   TransferProgressLookbackMinutes: int
   Queue: QueueSettings
}

let private getMockDomesticTransferProcessorHost (host: string option) =
   match host with
   | Some ip -> IPAddress.Parse ip
   | None ->
      try
         // Referencing container by name to resolve IP for
         // mock domestic transfer processor server.
         Dns.GetHostAddresses("mock-domestic-transfer-processor")[0]
      with _ ->
         if not Env.isDev then
            failwith
               """
               IP for mock domestic transfer processor doesn't exist.
               Misconfigured container name.
               """
         else
            printfn "Configuring localhost for domestic transfer."
            IPAddress.Loopback

let config =
   match AppConfig(builder.Configuration).Get<TransferConfigInput>() with
   | Ok input -> {
      MockDomesticTransferProcessor = {
         Host =
            getMockDomesticTransferProcessorHost
               input.MockDomesticTransferProcessor.Host
         Port = input.MockDomesticTransferProcessor.Port
      }
      DomesticTransferRouter = {
         MaxInstancesPerNode =
            Option.defaultValue
               10
               input.DomesticTransferRouter.MaxInstancesPerNode
      }
      domesticTransferCircuitBreaker =
         fun system ->
            Akka.Pattern.CircuitBreaker(
               system.Scheduler,
               input.DomesticTransferCircuitBreaker.MaxFailures
               |> Option.defaultValue 2,

               input.DomesticTransferCircuitBreaker.CallTimeoutSeconds
               |> Option.defaultValue 7
               |> TimeSpan.FromSeconds,

               input.DomesticTransferCircuitBreaker.ResetTimeoutSeconds
               |> Option.defaultValue 20
               |> TimeSpan.FromSeconds
            )
      TransferProgressLookbackMinutes =
         input.TransferProgressLookbackMinutes
         |> Option.defaultValue (if Env.isProd then 60 else 0)
      TransferProgressTrackingThrottle = {
         Count =
            input.TransferProgressTrackingThrottle.Count
            |> Option.defaultValue 100
         Burst =
            input.TransferProgressTrackingThrottle.Burst
            |> Option.defaultValue 100
         Duration =
            input.TransferProgressTrackingThrottle.Seconds
            |> Option.defaultValue 10
            |> TimeSpan.FromSeconds
      }
      Queue = {
         Name =
            input.DomesticTransferQueue.Name
            |> Option.defaultValue "domestic-transfer"
         MaxParallelism =
            input.DomesticTransferQueue.MaxParallelism |> Option.defaultValue 10
      }
     }
   | Error err ->
      match err with
      | NotFound key -> invalidArg key "Not found"
      | BadValue(key, value) -> invalidArg key $"{value} is invalid type for"
      | NotSupported key -> invalidArg key "Not supported"
