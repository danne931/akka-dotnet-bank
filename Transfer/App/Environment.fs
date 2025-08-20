[<RequireQualifiedAccess>]
module EnvTransfer

open System
open FsConfig

open Lib.Types

let builder = Env.builder

type private TransferConfigInput = {
   DomesticTransferCircuitBreaker: {|
      MaxFailures: int option
      CallTimeoutSeconds: float option
      ResetTimeoutSeconds: float option
   |}
   DomesticTransferQueue: {|
      Name: string option
      MaxParallelism: int option
   |}
   AutoTransferComputeThrottle: Env.StreamThrottleInput
}

type TransferConfig = {
   domesticTransferCircuitBreaker:
      Akka.Actor.ActorSystem -> Akka.Pattern.CircuitBreaker
   AutoTransferComputeThrottle: StreamThrottleEnvConfig
   Queue: QueueEnvConfig
}

let config =
   match AppConfig(builder.Configuration).Get<TransferConfigInput>() with
   | Ok input -> {
      domesticTransferCircuitBreaker =
         fun system ->
            Akka.Pattern.CircuitBreaker(
               system.Scheduler,
               input.DomesticTransferCircuitBreaker.MaxFailures
               |> Option.defaultValue 2,

               input.DomesticTransferCircuitBreaker.CallTimeoutSeconds
               |> Option.defaultValue 7.
               |> TimeSpan.FromSeconds,

               input.DomesticTransferCircuitBreaker.ResetTimeoutSeconds
               |> Option.defaultValue 20.
               |> TimeSpan.FromSeconds
            )
      AutoTransferComputeThrottle = {
         Count =
            input.AutoTransferComputeThrottle.Count |> Option.defaultValue 100
         Burst =
            input.AutoTransferComputeThrottle.Burst |> Option.defaultValue 100
         Duration =
            input.AutoTransferComputeThrottle.Seconds
            |> Option.defaultValue 10.
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
