module Bank.Transfer.Routes

open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open Microsoft.FSharp.Core
open EventStore.Client

open System
open System.Threading.Tasks

open BankTypes
open Bank.Transfer.Domain
open Bank.Account.Api
open Bank.Transfer.Api

module private Path =
   let Base = "/transfers"
   let TransferRecipient = Base + "/register-recipient"

let startTransferRoutes (app: WebApplication) (es: EventStoreClient) =
   app.MapPost(
      Path.TransferRecipient,
      Func<AccountActor.AccountRegistry, RegisterTransferRecipientCommand, Task<IResult>>(
         (fun registry command ->
            processCommand
               command
               registry
               (Validators.registerTransferRecipient (es, recipientExists))
            |> RouteUtil.UnwrapValidation)
      )
   )
   |> ignore

   app.MapPost(
      Path.Base,
      Func<AccountActor.AccountRegistry, TransferCommand, Task<IResult>>
         (fun registry command ->
            processCommand command registry (Validators.transfer ())
            |> RouteUtil.UnwrapValidation)
   )
   |> ignore
