[<RequireQualifiedAccess>]
module EnvNotifications

open System
open FsConfig

open Lib.Types
open Email

let builder = Env.builder

// NOTE:
// For local development, EmailBearerToken, SupportEmail & OverrideEmailRecipient are set in ~/.microsoft/usersecrets.
// See https://learn.microsoft.com/en-us/aspnet/core/security/app-secrets?view=aspnetcore-7.0&tabs=linux#set-a-secret
// In Account.Service directory run:
// dotnet user-secrets set "EmailBearerToken" "YOUR_API_TOKEN"

type NotificationsInput = {
   EmailServiceUri: string
   EmailBearerToken: string option
   SupportEmail: string option
   CircuitBreaker: {|
      MaxFailures: int option
      CallTimeoutSeconds: float option
      ResetTimeoutSeconds: float option
   |}
   EmailQueue: {|
      Name: string option
      MaxParallelism: int option
   |}
   MockSendingEmail: bool option
   OverrideEmailRecipient: string option
}

type NotificationsConfig = {
   EmailServiceUri: string
   EmailBearerToken: string option
   SupportEmail: Email option
   circuitBreaker: Akka.Actor.ActorSystem -> Akka.Pattern.CircuitBreaker
   Queue: QueueEnvConfig
   // If true, will pretend to send emails rather than hitting the email
   // third party API.
   MockSendingEmail: bool
   // Will redirect all emails to this.  Useful in development.
   OverrideEmailRecipient: Email option
}

let private errorMessage missing =
   $"""
   ========================================================

     Missing {missing} configuration setting(s)

   ========================================================
   """

let private throwInProductionIfMissingEnvVars (input: NotificationsInput) =
   let missing =
      [
         "EmailBearerToken", Option.isNone input.EmailBearerToken
         "SupportEmail", Option.isNone input.SupportEmail
      ]
      |> List.fold (fun acc (key, isNone) -> if isNone then key :: acc else acc) []

   if not missing.IsEmpty then
      let errMsg = errorMessage missing

      if Env.isProd then failwith errMsg else printfn "%A" errMsg

let config =
   match AppConfig(builder.Configuration).Get<NotificationsInput>() with
   | Ok input ->
      throwInProductionIfMissingEnvVars input

      let mockEmail = input.MockSendingEmail |> Option.defaultValue false

      {
         MockSendingEmail = not Env.isProd && mockEmail
         OverrideEmailRecipient =
            input.OverrideEmailRecipient
            |> Option.bind (fun email ->
               if not Env.isProd then
                  Some(Email.deserialize email)
               else
                  None)
         EmailServiceUri = input.EmailServiceUri
         EmailBearerToken = input.EmailBearerToken
         SupportEmail = input.SupportEmail |> Option.map Email.deserialize
         Queue = {
            Name = input.EmailQueue.Name |> Option.defaultValue "email"
            MaxParallelism =
               input.EmailQueue.MaxParallelism |> Option.defaultValue 10
         }
         circuitBreaker =
            fun system ->
               Akka.Pattern.CircuitBreaker(
                  system.Scheduler,

                  input.CircuitBreaker.MaxFailures |> Option.defaultValue 2,

                  input.CircuitBreaker.CallTimeoutSeconds
                  |> Option.defaultValue 7.
                  |> TimeSpan.FromSeconds,

                  input.CircuitBreaker.ResetTimeoutSeconds
                  |> Option.defaultValue 30.
                  |> TimeSpan.FromSeconds
               )
      }
   | Error err ->
      match err with
      | NotFound key -> invalidArg key "Not found"
      | BadValue(key, value) -> invalidArg key $"{value} is invalid type for"
      | NotSupported key -> invalidArg key "Not supported"
