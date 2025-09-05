[<RequireQualifiedAccess>]
module EnvEmployee

open System
open FsConfig

open Lib.Types

let builder = Env.builder

// NOTE:
// For local development, CardIssuerApiKey is set in ~/.microsoft/usersecrets.
// See https://learn.microsoft.com/en-us/aspnet/core/security/app-secrets?view=aspnetcore-7.0&tabs=linux#set-a-secret
// In Account.Service directory run:
// dotnet user-secrets set "CardIssuerApiKey" "YOUR_API_KEY"

type private Input = {
   CardIssuerServiceCircuitBreaker: {|
      MaxFailures: int option
      CallTimeoutSeconds: float option
      ResetTimeoutSeconds: float option
   |}
   CardIssuerServiceQueue: {|
      Name: string option
      MaxParallelism: int option
   |}
   // If true, will send mock responses from Lithic
   // for card setup saga & purchase flows
   CardIssuerMockRequests: bool option
   CardIssuerApiKey: string option
   CardIssuerURI: string option
}

type EmployeeConfig = {
   cardIssuerServiceCircuitBreaker:
      Akka.Actor.ActorSystem -> Akka.Pattern.CircuitBreaker
   CardIssuerServiceQueue: QueueEnvConfig
   CardIssuerURI: string
   CardIssuerMockRequests: bool
   CardIssuerApiKey: string option
}

let private errorMessage missing =
   $"""
   ========================================================

     Missing {missing} configuration setting(s)

   ========================================================
   """

let private throwInProductionIfMissingEnvVars (input: Input) =
   let missing =
      [
         "CardIssuerApiKey", Option.isNone input.CardIssuerApiKey
         "CardIssuerURI", Option.isNone input.CardIssuerURI
      ]
      |> List.fold (fun acc (key, isNone) -> if isNone then key :: acc else acc) []

   if not missing.IsEmpty then
      let errMsg = errorMessage missing

      if Env.isProd then failwith errMsg else printfn "%A" errMsg

let config =
   match AppConfig(builder.Configuration).Get<Input>() with
   | Ok input ->
      throwInProductionIfMissingEnvVars input

      {
         cardIssuerServiceCircuitBreaker =
            fun system ->
               Akka.Pattern.CircuitBreaker(
                  system.Scheduler,
                  input.CardIssuerServiceCircuitBreaker.MaxFailures
                  |> Option.defaultValue 2,
                  input.CardIssuerServiceCircuitBreaker.CallTimeoutSeconds
                  |> Option.defaultValue 7.
                  |> TimeSpan.FromSeconds,
                  input.CardIssuerServiceCircuitBreaker.ResetTimeoutSeconds
                  |> Option.defaultValue 20.
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
         CardIssuerApiKey = input.CardIssuerApiKey
         CardIssuerMockRequests =
            input.CardIssuerMockRequests
            |> Option.defaultValue (Env.isDev || Env.isStaging)
         CardIssuerURI =
            input.CardIssuerURI
            |> Option.defaultValue "https://sandbox.lithic.com/v1"
      }
   | Error err ->
      match err with
      | NotFound key -> invalidArg key "Not found"
      | BadValue(key, value) -> invalidArg key $"{value} is invalid type for"
      | NotSupported key -> invalidArg key "Not supported"
