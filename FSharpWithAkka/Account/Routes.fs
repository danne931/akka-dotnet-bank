module Bank.Account.Routes

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open EventStore.Client

open BankTypes
open Lib.Types
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
      Path.Base,
      Func<Task<IResult>>(fun _ ->
         getAccountCreationEvents esClient |> RouteUtil.UnwrapOption)
   )
   |> ignore

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
      Func<AccountActor.AccountRegistry, CreateAccountCommand, Task<IResult>>
         (fun registry command ->
            createAccount
               esClient
               registry
               (Validators.accountCreate ())
               command
            |> RouteUtil.UnwrapValidation)
   )
   |> ignore

   app.MapDelete(
      Path.AccountEvents,
      Func<Guid, Task<IResult>>(fun id ->
         softDeleteEvents esClient id |> RouteUtil.Unwrap)
   )
   |> ignore

   app.MapPost(
      Path.Deposit,
      Func<AccountActor.AccountRegistry, DepositCashCommand, Task<IResult>>
         (fun registry command ->
            processCommand command registry (Validators.deposit ())
            |> RouteUtil.UnwrapValidation)
   )
   |> ignore

   app.MapPost(
      Path.Debit,
      Func<AccountActor.AccountRegistry, DebitCommand, Task<IResult>>
         (fun registry command ->
            processCommand command registry (Validators.debit ())
            |> RouteUtil.UnwrapValidation)
   )
   |> ignore

   app.MapPost(
      Path.DailyDebitLimit,
      Func<AccountActor.AccountRegistry, LimitDailyDebitsCommand, Task<IResult>>
         (fun registry command ->
            processCommand command registry (Validators.dailyDebitLimit ())
            |> RouteUtil.UnwrapValidation)
   )
   |> ignore

   app.MapPost(
      Path.LockCard,
      Func<AccountActor.AccountRegistry, LockCardCommand, Task<IResult>>
         (fun registry command ->
            processCommand command registry (PassValidation())
            |> RouteUtil.UnwrapValidation)
   )
   |> ignore

   app.MapPost(
      Path.UnlockCard,
      Func<AccountActor.AccountRegistry, UnlockCardCommand, Task<IResult>>
         (fun registry command ->
            processCommand command registry (PassValidation())
            |> RouteUtil.UnwrapValidation)
   )
   |> ignore
