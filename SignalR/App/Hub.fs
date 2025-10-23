namespace Bank.Hubs

open Microsoft.AspNetCore.SignalR
open System.Threading.Tasks

type IBankClient =
   abstract member ParentAccountEventPersistenceConfirmation: string -> Task
   abstract member AccountEventPersistenceConfirmation: string -> Task
   abstract member EmployeeEventPersistenceConfirmation: string -> Task
   abstract member OrgEventPersistenceConfirmation: string -> Task
   abstract member EventProcessingError: string -> Task
   abstract member CircuitBreakerMessage: string -> Task
   abstract member SagaUpdated: string -> Task

type BankHub() =
   inherit Hub<IBankClient>()

   member x.RemoveFromConnectionGroup(orgId: string) =
      x.Groups.RemoveFromGroupAsync(x.Context.ConnectionId, orgId)

   member x.AddToConnectionGroup(orgId: string) =
      x.Groups.AddToGroupAsync(x.Context.ConnectionId, orgId)
