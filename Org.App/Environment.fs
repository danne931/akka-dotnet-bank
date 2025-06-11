[<RequireQualifiedAccess>]
module EnvOrg

open System
open FsConfig

open Lib.Types

let builder = Env.builder

type private Input = {
   KnowYourCustomerServiceCircuitBreaker: {|
      MaxFailures: int option
      CallTimeoutSeconds: int option
      ResetTimeoutSeconds: int option
   |}
   KnowYourCustomerServiceQueue: {|
      Name: string option
      MaxParallelism: int option
   |}
}

type KnowYourCustomerConfig = {
   KnowYourCustomerServiceCircuitBreaker:
      Akka.Actor.ActorSystem -> Akka.Pattern.CircuitBreaker
   KnowYourCustomerServiceQueue: QueueSettings
}

let config =
   match AppConfig(builder.Configuration).Get<Input>() with
   | Ok input -> {
      KnowYourCustomerServiceCircuitBreaker =
         fun system ->
            Akka.Pattern.CircuitBreaker(
               system.Scheduler,
               input.KnowYourCustomerServiceCircuitBreaker.MaxFailures
               |> Option.defaultValue 2,

               input.KnowYourCustomerServiceCircuitBreaker.CallTimeoutSeconds
               |> Option.defaultValue 7
               |> TimeSpan.FromSeconds,

               input.KnowYourCustomerServiceCircuitBreaker.ResetTimeoutSeconds
               |> Option.defaultValue 20
               |> TimeSpan.FromSeconds
            )
      KnowYourCustomerServiceQueue = {
         Name =
            input.KnowYourCustomerServiceQueue.Name
            |> Option.defaultValue "know-your-customer"
         MaxParallelism =
            input.KnowYourCustomerServiceQueue.MaxParallelism
            |> Option.defaultValue 10
      }
     }
   | Error err ->
      match err with
      | NotFound key -> invalidArg key "Not found"
      | BadValue(key, value) -> invalidArg key $"{value} is invalid type for"
      | NotSupported key -> invalidArg key "Not supported"
