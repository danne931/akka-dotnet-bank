[<RequireQualifiedAccess>]
module EnvTransfer

open System
open System.Net
open FsConfig

open Lib.Types

let builder = Env.builder

type MockPartnerBank = { Host: IPAddress; Port: int }

type private TransferConfigInput = {
   MockPartnerBank: {| Host: string option; Port: int |}
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
   MockPartnerBank: MockPartnerBank
   domesticTransferCircuitBreaker:
      Akka.Actor.ActorSystem -> Akka.Pattern.CircuitBreaker
   AutoTransferComputeThrottle: StreamThrottleEnvConfig
   Queue: QueueEnvConfig
}

let private getMockPartnerBankHost (host: string option) =
   match host with
   | Some ip -> IPAddress.Parse ip
   | None ->
      try
         // Referencing container by name to resolve IP for
         // mock partner bank server.
         Dns.GetHostAddresses("mock-partner-bank")[0]
      with _ ->
         if not Env.isDev then
            failwith
               """
               IP for mock partner bank doesn't exist.
               Misconfigured container name.
               """
         else
            printfn "Configuring localhost for domestic transfer."
            IPAddress.Loopback

let config =
   match AppConfig(builder.Configuration).Get<TransferConfigInput>() with
   | Ok input -> {
      MockPartnerBank = {
         Host = getMockPartnerBankHost input.MockPartnerBank.Host
         Port = input.MockPartnerBank.Port
      }
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
