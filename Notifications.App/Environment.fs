[<RequireQualifiedAccess>]
module EnvNotifications

open System
open FsConfig

open Lib.Types

let builder = Env.builder

// NOTE:
// For local development, EmailBearerToken & SupportEmail are set in ~/.microsoft/usersecrets.
// See https://learn.microsoft.com/en-us/aspnet/core/security/app-secrets?view=aspnetcore-7.0&tabs=linux#set-a-secret

type NotificationsInput = {
   EmailServiceUri: string
   EmailBearerToken: string option
   SupportEmail: string option
   CircuitBreaker: {|
      MaxFailures: int option
      CallTimeoutSeconds: int option
      ResetTimeoutSeconds: int option
   |}
   RabbitConnection: {|
      Host: string option
      Port: int option
      VirtualHost: string option
      Username: string option
      Password: string option
   |}
   RabbitQueue: {|
      Name: string option
      MaxParallelism: int option
   |}
}

type NotificationsConfig = {
   EmailServiceUri: string
   EmailBearerToken: string option
   SupportEmail: string option
   circuitBreaker: Akka.Actor.ActorSystem -> Akka.Pattern.CircuitBreaker
   RabbitConnection: RabbitConnectionSettings
   RabbitQueue: RabbitQueueSettings
}


let private errorMessage missing =
   $"""
   ========================================================

     Missing {missing} configuration setting(s)

   ========================================================
   """

let config =
   match AppConfig(builder.Configuration).Get<NotificationsInput>() with
   | Ok input ->
      let missing =
         [
            "EmailBearerToken", Option.isNone input.EmailBearerToken
            "SupportEmail", Option.isNone input.SupportEmail
         ]
         |> List.fold
            (fun acc (key, isNone) -> if isNone then key :: acc else acc)
            []

      if not missing.IsEmpty then
         let errMsg = errorMessage missing

         if Env.isProd then failwith errMsg else printfn "%A" errMsg

      {
         EmailServiceUri = input.EmailServiceUri
         EmailBearerToken = input.EmailBearerToken
         SupportEmail = input.SupportEmail
         RabbitConnection = {
            Host =
               input.RabbitConnection.Host |> Option.defaultValue "localhost"
            Port = input.RabbitConnection.Port |> Option.defaultValue 5672
            VirtualHost =
               input.RabbitConnection.VirtualHost |> Option.defaultValue "/"
            Username =
               input.RabbitConnection.Username |> Option.defaultValue "guest"
            Password =
               input.RabbitConnection.Password |> Option.defaultValue "guest"
         }
         RabbitQueue = {
            Name = input.RabbitQueue.Name |> Option.defaultValue "email"
            MaxParallelism =
               input.RabbitQueue.MaxParallelism |> Option.defaultValue 10
         }
         circuitBreaker =
            fun system ->
               Akka.Pattern.CircuitBreaker(
                  system.Scheduler,

                  input.CircuitBreaker.MaxFailures |> Option.defaultValue 2,

                  input.CircuitBreaker.CallTimeoutSeconds
                  |> Option.defaultValue 7
                  |> TimeSpan.FromSeconds,

                  input.CircuitBreaker.ResetTimeoutSeconds
                  |> Option.defaultValue 30
                  |> TimeSpan.FromSeconds
               )
      }
   | Error err ->
      match err with
      | NotFound key -> invalidArg key "Not found"
      | BadValue(key, value) -> invalidArg key $"{value} is invalid type for"
      | NotSupported key -> invalidArg key "Not supported"
