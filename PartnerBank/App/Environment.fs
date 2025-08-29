[<RequireQualifiedAccess>]
module EnvPartnerBank

open System
open System.Net
open FsConfig

open Lib.Types

let builder = Env.builder

type MockPartnerBank = { Host: IPAddress; Port: int }

type private PartnerBankConfigInput = {
   MockPartnerBank: {|
      Host: string option
      Port: int option
   |}
   PartnerBankCircuitBreaker: {|
      MaxFailures: int option
      CallTimeoutSeconds: float option
      ResetTimeoutSeconds: float option
   |}
   PartnerBankQueue: {|
      Name: string option
      MaxParallelism: int option
   |}
}

type PartnerBankConfig = {
   MockPartnerBank: MockPartnerBank
   CircuitBreaker: Akka.Actor.ActorSystem -> Akka.Pattern.CircuitBreaker
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
            printfn "Configuring localhost for mock partner bank."
            IPAddress.Loopback

let config =
   match AppConfig(builder.Configuration).Get<PartnerBankConfigInput>() with
   | Ok input -> {
      MockPartnerBank = {
         Host = getMockPartnerBankHost input.MockPartnerBank.Host
         Port = input.MockPartnerBank.Port |> Option.defaultValue 5007
      }
      CircuitBreaker =
         fun system ->
            Akka.Pattern.CircuitBreaker(
               system.Scheduler,
               input.PartnerBankCircuitBreaker.MaxFailures
               |> Option.defaultValue 2,

               input.PartnerBankCircuitBreaker.CallTimeoutSeconds
               |> Option.defaultValue 7.
               |> TimeSpan.FromSeconds,

               input.PartnerBankCircuitBreaker.ResetTimeoutSeconds
               |> Option.defaultValue 20.
               |> TimeSpan.FromSeconds
            )
      Queue = {
         Name =
            input.PartnerBankQueue.Name |> Option.defaultValue "partner-bank"
         MaxParallelism =
            input.PartnerBankQueue.MaxParallelism |> Option.defaultValue 10
      }
     }
   | Error err ->
      match err with
      | NotFound key -> invalidArg key "Not found"
      | BadValue(key, value) -> invalidArg key $"{value} is invalid type for"
      | NotSupported key -> invalidArg key "Not supported"
