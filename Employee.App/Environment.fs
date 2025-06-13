[<RequireQualifiedAccess>]
module EnvEmployee

open System
open FsConfig

open Lib.Types

let builder = Env.builder

type private Input = {
   CardIssuerServiceCircuitBreaker: {|
      MaxFailures: int option
      CallTimeoutSeconds: int option
      ResetTimeoutSeconds: int option
   |}
   CardIssuerServiceQueue: {|
      Name: string option
      MaxParallelism: int option
   |}
}

type EmployeeConfig = {
   cardIssuerServiceCircuitBreaker:
      Akka.Actor.ActorSystem -> Akka.Pattern.CircuitBreaker
   CardIssuerServiceQueue: QueueSettings
}

let config =
   match AppConfig(builder.Configuration).Get<Input>() with
   | Ok input -> {
      cardIssuerServiceCircuitBreaker =
         fun system ->
            Akka.Pattern.CircuitBreaker(
               system.Scheduler,
               input.CardIssuerServiceCircuitBreaker.MaxFailures
               |> Option.defaultValue 2,
               input.CardIssuerServiceCircuitBreaker.CallTimeoutSeconds
               |> Option.defaultValue 7
               |> TimeSpan.FromSeconds,
               input.CardIssuerServiceCircuitBreaker.ResetTimeoutSeconds
               |> Option.defaultValue 20
               |> TimeSpan.FromSeconds
            )
      CardIssuerServiceQueue = {
         Name =
            input.CardIssuerServiceQueue.Name
            |> Option.defaultValue "card-issuer"
         MaxParallelism =
            input.CardIssuerServiceQueue.MaxParallelism
            |> Option.defaultValue 10
      }
     }
   | Error err ->
      match err with
      | NotFound key -> invalidArg key "Not found"
      | BadValue(key, value) -> invalidArg key $"{value} is invalid type for"
      | NotSupported key -> invalidArg key "Not supported"
