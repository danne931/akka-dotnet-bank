module Bank.Account.Routes

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open EventStore.Client

open Bank.Account.Domain
open Bank.Account.Api

module private Path =
   let Base = "/accounts"
   let Account = Base + "/{id}"
   let Diagnostic = "/diagnostic"
   let AccountEvents = Diagnostic + "/events/{id}"
   let Deposit = Base + "/deposit"
   let Debit = Base + "/debit"
   let DailyDebitLimit = Base + "/daily-debit-limit"
   let LockCard = Base + "/lock"
   let UnlockCard = Base + "/unlock"

let startAccountRoutes (app: WebApplication) (esClient: EventStoreClient) =
   app.MapGet(
      Path.Account,
      Func<Guid, Task<IResult>>(fun id ->
         getAccount (getAccountEvents esClient) id |> RouteUtil.UnwrapOption)
   )
   |> ignore

   app.MapGet(
      Path.AccountEvents,
      Func<Guid, Task<IResult>>(fun id ->
         getAccountEvents esClient id |> RouteUtil.UnwrapOption)
   )
   |> ignore

   app.MapPost(
      Path.Base,
      Func<CreateAccountCommand, Task<IResult>>(fun command ->
         createAccount
            esClient
            (Validators.accountCreate ())
            (CreateAccountCommand.create command)
         |> RouteUtil.UnwrapValidation)
   )
   |> ignore

   app.MapDelete(
      Path.AccountEvents,
      Func<Guid, Task<IResult>>(fun id ->
         softDeleteEvents esClient id |> RouteUtil.Unwrap)
   )
   |> ignore
