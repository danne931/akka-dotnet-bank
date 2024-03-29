namespace Bank.Hubs

open Microsoft.AspNetCore.SignalR
open System.Threading.Tasks

open BankTypes

type IAccountClient =
   abstract member ReceiveMessage:
      {|
         event: AccountEvent
         newState: AccountState
      |} ->
         Task

   abstract member ReceiveError: string -> Task

   abstract member ReceiveCircuitBreakerMessage: CircuitBreakerMessage -> Task

type AccountHub() =
   inherit Hub<IAccountClient>()

   member x.RemoveFromConnectionGroup(accountId: string) =
      x.Groups.RemoveFromGroupAsync(x.Context.ConnectionId, accountId)

   member x.AddToConnectionGroup(accountId: string) =
      x.Groups.AddToGroupAsync(x.Context.ConnectionId, accountId)
