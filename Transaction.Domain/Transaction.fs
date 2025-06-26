module Transaction

open System

open Bank.Org.Domain
open Bank.Account.Domain
open Bank.Employee.Domain
open Lib.SharedTypes
open CommandApproval

type OrgHistory = {
   InitiatedByName: string
   Event: OrgEvent
}

type AccountHistory = {
   InitiatedByName: string
   Event: AccountEvent
}

type ParentAccountHistory = {
   InitiatedByName: string
   Event: ParentAccountEvent
}

type EmployeeHistory = {
   InitiatedByName: string
   EmployeeName: string
   Event: EmployeeEvent
}

[<RequireQualifiedAccess>]
type History =
   | Org of OrgHistory
   | Account of AccountHistory
   | ParentAccount of ParentAccountHistory
   | Employee of EmployeeHistory

   member x.Envelope =
      match x with
      | Org h -> OrgEnvelope.unwrap h.Event |> snd
      | Account h -> AccountEnvelope.unwrap h.Event |> snd
      | ParentAccount h ->
         h.Event |> AccountEvent.ParentAccount |> AccountEnvelope.unwrap |> snd
      | Employee h -> EmployeeEnvelope.unwrap h.Event |> snd

[<RequireQualifiedAccess>]
type TransactionStatus =
   | Scheduled
   | InProgress
   | Complete
   | Failed

   member x.Display =
      match x with
      | TransactionStatus.Scheduled -> "Scheduled"
      | TransactionStatus.Complete -> "Complete"
      | TransactionStatus.InProgress -> "In Progress"
      | TransactionStatus.Failed -> "Failed"

[<RequireQualifiedAccess>]
type TransactionType =
   | Payment
   | DomesticTransfer
   | InternalTransferBetweenOrgs
   | InternalTransferWithinOrg
   | InternalAutomatedTransfer
   | Purchase
   | Deposit

type Transaction = {
   Type: TransactionType
   Status: TransactionStatus
   History: History list
   Timestamp: DateTime
   Amount: decimal
   Id: TransactionId
   OrgId: OrgId
   InitiatedBy: Initiator
}

type TransactionCategory = { Id: int; Name: string }

type TransactionWithAncillaryInfo = {
   Id: TransactionId
   Transaction: Transaction
   Category: TransactionCategory option
   Note: string option
}

let private transactionInfoFromApprovableCommand =
   function
   | ApprovableCommandType.ApprovableAmountBased c ->
      match c with
      | ApprovableAmountBased.DomesticTransferCommandType ->
         Some(TransactionType.DomesticTransfer, TransactionStatus.InProgress)
      | ApprovableAmountBased.InternalTransferBetweenOrgsCommandType ->
         Some(
            TransactionType.InternalTransferBetweenOrgs,
            TransactionStatus.InProgress
         )
      | ApprovableAmountBased.FulfillPlatformPaymentCommandType ->
         Some(TransactionType.Payment, TransactionStatus.InProgress)
   | _ -> None

let transactionInfoFromHistory
   (history: History)
   : (TransactionType * TransactionStatus * decimal) option
   =
   match history with
   | History.ParentAccount _ -> None
   | History.Org orgHistory ->
      let info =
         match orgHistory.Event with
         | OrgEvent.CommandApprovalRequested e ->
            Some(e.Data.Command.CommandType, e.Data.Command.Amount)
         | OrgEvent.CommandApprovalAcquired e ->
            Some(e.Data.Command.CommandType, e.Data.Command.Amount)
         | OrgEvent.CommandApprovalProcessCompleted e ->
            Some(e.Data.Command.CommandType, e.Data.Command.Amount)
         | OrgEvent.CommandApprovalDeclined e ->
            Some(e.Data.Command.CommandType, e.Data.Command.Amount)
         | OrgEvent.CommandApprovalTerminated e ->
            Some(e.Data.Command.CommandType, e.Data.Command.Amount)
         | _ -> None

      info
      |> Option.bind (fun (cmdType, amount) ->
         transactionInfoFromApprovableCommand cmdType
         |> Option.map (fun (txnType, txnStatus) -> txnType, txnStatus, amount))
   | History.Employee employeeHistory ->
      match employeeHistory.Event with
      | EmployeeEvent.PurchaseSettled e ->
         Some(
            TransactionType.Purchase,
            TransactionStatus.InProgress,
            e.Data.Info.Amount
         )
      | EmployeeEvent.PurchaseRefunded e ->
         Some(
            TransactionType.Purchase,
            TransactionStatus.Failed,
            e.Data.Info.Amount
         )
      | _ -> None
   | History.Account accountHistory ->
      match accountHistory.Event with
      | AccountEvent.InternalTransferWithinOrgDeducted e ->
         Some(
            TransactionType.InternalTransferWithinOrg,
            TransactionStatus.InProgress,
            e.Data.BaseInfo.Amount
         )
      | AccountEvent.InternalTransferWithinOrgDeposited e ->
         Some(
            TransactionType.InternalTransferWithinOrg,
            TransactionStatus.Complete,
            e.Data.BaseInfo.Amount
         )
      | AccountEvent.InternalTransferBetweenOrgsScheduled e ->
         Some(
            TransactionType.InternalTransferBetweenOrgs,
            TransactionStatus.Scheduled,
            e.Data.BaseInfo.Amount
         )
      | AccountEvent.InternalTransferBetweenOrgsPending e ->
         Some(
            TransactionType.InternalTransferBetweenOrgs,
            TransactionStatus.InProgress,
            e.Data.BaseInfo.Amount
         )
      | AccountEvent.InternalTransferBetweenOrgsDeposited e ->
         Some(
            TransactionType.InternalTransferBetweenOrgs,
            TransactionStatus.InProgress,
            e.Data.BaseInfo.Amount
         )
      | AccountEvent.InternalTransferBetweenOrgsSettled e ->
         Some(
            TransactionType.InternalTransferBetweenOrgs,
            TransactionStatus.Complete,
            e.Data.BaseInfo.Amount
         )
      | AccountEvent.InternalTransferBetweenOrgsFailed e ->
         Some(
            TransactionType.InternalTransferBetweenOrgs,
            TransactionStatus.Failed,
            e.Data.BaseInfo.Amount
         )
      | AccountEvent.InternalAutomatedTransferDeducted e ->
         Some(
            TransactionType.InternalAutomatedTransfer,
            TransactionStatus.Complete,
            e.Data.BaseInfo.Amount
         )
      | AccountEvent.InternalAutomatedTransferDeposited e ->
         Some(
            TransactionType.InternalAutomatedTransfer,
            TransactionStatus.Complete,
            e.Data.BaseInfo.Amount
         )
      | AccountEvent.DepositedCash e ->
         Some(
            TransactionType.Deposit,
            TransactionStatus.Complete,
            e.Data.Amount
         )
      | AccountEvent.DebitPending e ->
         Some(
            TransactionType.Purchase,
            TransactionStatus.InProgress,
            e.Data.Amount
         )
      | AccountEvent.DebitSettled e ->
         Some(
            TransactionType.Purchase,
            TransactionStatus.Complete,
            e.Data.Amount
         )
      | AccountEvent.DebitRefunded e ->
         Some(
            TransactionType.Purchase,
            TransactionStatus.Complete,
            e.Data.Amount
         )
      | AccountEvent.DebitFailed e ->
         Some(TransactionType.Purchase, TransactionStatus.Failed, e.Data.Amount)
      | AccountEvent.DomesticTransferScheduled e ->
         Some(
            TransactionType.DomesticTransfer,
            TransactionStatus.Scheduled,
            e.Data.BaseInfo.Amount
         )
      | AccountEvent.DomesticTransferPending e ->
         Some(
            TransactionType.DomesticTransfer,
            TransactionStatus.InProgress,
            e.Data.BaseInfo.Amount
         )
      | AccountEvent.DomesticTransferProgress e ->
         Some(
            TransactionType.DomesticTransfer,
            TransactionStatus.InProgress,
            e.Data.BaseInfo.Amount
         )
      | AccountEvent.DomesticTransferSettled e ->
         Some(
            TransactionType.DomesticTransfer,
            TransactionStatus.Complete,
            e.Data.BaseInfo.Amount
         )
      | AccountEvent.DomesticTransferFailed e ->
         Some(
            TransactionType.DomesticTransfer,
            TransactionStatus.Failed,
            e.Data.BaseInfo.Amount
         )
      | AccountEvent.PlatformPaymentRequested e ->
         Some(
            TransactionType.Payment,
            TransactionStatus.InProgress,
            e.Data.BaseInfo.Amount
         )
      | AccountEvent.PlatformPaymentDeclined e ->
         Some(
            TransactionType.Payment,
            TransactionStatus.Failed,
            e.Data.BaseInfo.Amount
         )
      | AccountEvent.PlatformPaymentCancelled e ->
         Some(
            TransactionType.Payment,
            TransactionStatus.Failed,
            e.Data.BaseInfo.Amount
         )
      | AccountEvent.PlatformPaymentPending e ->
         Some(
            TransactionType.Payment,
            TransactionStatus.InProgress,
            e.Data.BaseInfo.Amount
         )
      | AccountEvent.PlatformPaymentDeposited e ->
         Some(
            TransactionType.Payment,
            TransactionStatus.InProgress,
            e.Data.BaseInfo.Amount
         )
      | AccountEvent.PlatformPaymentFailed e ->
         Some(
            TransactionType.Payment,
            TransactionStatus.Failed,
            e.Data.BaseInfo.Amount
         )
      | AccountEvent.PlatformPaymentSettled e ->
         Some(
            TransactionType.Payment,
            TransactionStatus.Complete,
            e.Data.BaseInfo.Amount
         )
      | _ -> None

let applyHistory
   (txns: Map<TransactionId, Transaction>)
   (history: History)
   : Map<TransactionId, Transaction>
   =
   match transactionInfoFromHistory history with
   | Some(txnType, status, amount) ->
      let envelope = history.Envelope
      let txnId = TransactionId envelope.CorrelationId

      match txns.TryFind txnId with
      | Some _ ->
         Map.change
            txnId
            (Option.map (fun txn ->
               let txn = {
                  txn with
                     Status = status
                     History = history :: txn.History
               }

               // For most transactions the timestamp shown will be the
               // timestamp of the start of a transaction's lifecycle.
               // (ex: The timestamp of a domestic transfer initiating
               //      processing rather than when it finished potentially
               //      days later.)
               // An exception will be made for payments received.  We will
               // show the timestamp of when an incoming payment was received,
               // rather than their respective lifecycle start "PaymentRequested"
               // timestamps.
               match history with
               | History.Account accountHistory ->
                  match accountHistory.Event with
                  | AccountEvent.PlatformPaymentDeposited e -> {
                     txn with
                        Timestamp = e.Timestamp
                    }
                  | _ -> txn
               | _ -> txn))
            txns
      | None ->
         txns
         |> Map.add txnId {
            Type = txnType
            Status = status
            History = [ history ]
            Timestamp = envelope.Timestamp
            Amount = amount
            Id = TransactionId envelope.CorrelationId
            OrgId = envelope.OrgId
            InitiatedBy = envelope.InitiatedBy
         }
   | None -> txns

let fromHistory (history: History list) : Transaction list =
   history
   |> List.fold applyHistory Map.empty
   |> _.Values
   |> Seq.toList
   |> List.sortByDescending (fun txn -> txn.Timestamp, txn.Id)
