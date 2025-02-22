namespace Bank.Hubs

open Microsoft.AspNetCore.SignalR
open System.Threading.Tasks

type IAccountClient =
   abstract member AccountEventPersistenceConfirmation: string -> Task
   abstract member EmployeeEventPersistenceConfirmation: string -> Task
   abstract member OrgEventPersistenceConfirmation: string -> Task
   abstract member EventProcessingError: string -> Task
   abstract member CircuitBreakerMessage: string -> Task

type AccountHub() =
   inherit Hub<IAccountClient>()

   member x.RemoveFromConnectionGroup(orgId: string) =
      x.Groups.RemoveFromGroupAsync(x.Context.ConnectionId, orgId)

   member x.AddToConnectionGroup(orgId: string) =
      x.Groups.AddToGroupAsync(x.Context.ConnectionId, orgId)
