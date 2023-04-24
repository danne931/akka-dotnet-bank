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

module private Path =
   let Base = "/transfers"
   let TransferRecipient = Base + "/register-recipient"

//let private registerTransferRecipient (es: EventStoreClient) (options: JsonSerializerOptions) cmd  =
let private registerTransferRecipient (es: EventStoreClient) cmd =
   cmd
   |> RegisterTransferRecipientCommand.create
   |> RegisterInternalTransferRecipientEvent.create
   |> fun evt -> saveAndPublish es (Envelope.unwrap evt)
   |> RouteUtil.Unwrap

let private transfer cmd = cmd |> Results.Ok //|> TransferCommand.create |> Results.Ok

let startTransferRoutes (app: WebApplication) (es: EventStoreClient) =
   app.MapPost(Path.Base, Func<TransferCommand, IResult>(transfer)) |> ignore

   app.MapPost(
      Path.TransferRecipient,
      Func<RegisterTransferRecipientCommand, Task<IResult>>(
         registerTransferRecipient es
      )
   )
   |> ignore
