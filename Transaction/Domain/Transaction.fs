module Transaction

open System

open Bank.Org.Domain
open Bank.Account.Domain
open Bank.Employee.Domain
open Bank.Transfer.Domain
open Bank.Purchase.Domain
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

let private txnTypePaymentOrTransfer (paymentId: PaymentRequestId option) =
   if paymentId.IsSome then
      TransactionType.Payment
   else
      TransactionType.InternalTransferBetweenOrgs

let private transactionInfoFromApprovableCommand =
   function
   | ApprovableCommand.AmountBased c ->
      match c with
      | ApprovableCommandAmountBased.DomesticTransfer _ ->
         Some(TransactionType.DomesticTransfer, TransactionStatus.InProgress)
      | ApprovableCommandAmountBased.InternalTransferBetweenOrgs e ->
         Some(
            txnTypePaymentOrTransfer e.Data.OriginatedFromPaymentRequest,
            TransactionStatus.InProgress
         )
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
            Some(e.Data.Command, e.Data.Command.Amount)
         | OrgEvent.CommandApprovalAcquired e ->
            Some(e.Data.Command, e.Data.Command.Amount)
         | OrgEvent.CommandApprovalProcessCompleted e ->
            Some(e.Data.Command, e.Data.Command.Amount)
         | OrgEvent.CommandApprovalDeclined e ->
            Some(e.Data.Command, e.Data.Command.Amount)
         | OrgEvent.CommandApprovalTerminated e ->
            Some(e.Data.Command, e.Data.Command.Amount)
         | _ -> None

      info
      |> Option.bind (fun (cmd, amount) ->
         transactionInfoFromApprovableCommand cmd
         |> Option.map (fun (txnType, txnStatus) -> txnType, txnStatus, amount))
   | History.Employee employeeHistory ->
      match employeeHistory.Event with
      | EmployeeEvent.PurchasePending e ->
         Some(
            TransactionType.Purchase,
            TransactionStatus.InProgress,
            e.Data.Info.Amount
         )
      | EmployeeEvent.PurchaseSettled e ->
         Some(
            TransactionType.Purchase,
            TransactionStatus.Complete,
            e.Data.Info.Amount
         )
      | EmployeeEvent.PurchaseFailed e ->
         Some(
            TransactionType.Purchase,
            TransactionStatus.Failed,
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
            txnTypePaymentOrTransfer e.Data.BaseInfo.FromPaymentRequest,
            TransactionStatus.Scheduled,
            e.Data.BaseInfo.Amount
         )
      | AccountEvent.InternalTransferBetweenOrgsPending e ->
         Some(
            txnTypePaymentOrTransfer e.Data.BaseInfo.FromPaymentRequest,
            TransactionStatus.InProgress,
            e.Data.BaseInfo.Amount
         )
      | AccountEvent.InternalTransferBetweenOrgsDeposited e ->
         Some(
            txnTypePaymentOrTransfer e.Data.BaseInfo.FromPaymentRequest,
            TransactionStatus.InProgress,
            e.Data.BaseInfo.Amount
         )
      | AccountEvent.InternalTransferBetweenOrgsSettled e ->
         Some(
            txnTypePaymentOrTransfer e.Data.BaseInfo.FromPaymentRequest,
            TransactionStatus.Complete,
            e.Data.BaseInfo.Amount
         )
      | AccountEvent.InternalTransferBetweenOrgsFailed e ->
         Some(
            txnTypePaymentOrTransfer e.Data.BaseInfo.FromPaymentRequest,
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
      | AccountEvent.PaymentRequested e ->
         Some(
            TransactionType.Payment,
            TransactionStatus.InProgress,
            e.Data.SharedDetails.Amount
         )
      | AccountEvent.PaymentRequestDeclined e ->
         Some(
            TransactionType.Payment,
            TransactionStatus.Failed,
            e.Data.SharedDetails.Amount
         )
      | AccountEvent.PaymentRequestCancelled e ->
         Some(
            TransactionType.Payment,
            TransactionStatus.Failed,
            e.Data.SharedDetails.Amount
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
            (Option.map (fun txn -> {
               txn with
                  Status = status
                  Amount = amount
                  History = history :: txn.History
            }))
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
   |> List.sortBy _.Envelope.Timestamp
   |> List.fold applyHistory Map.empty
   |> Map.map (fun _ txn -> {
      txn with
         History = txn.History |> List.sortByDescending _.Envelope.Timestamp
   })
   |> _.Values
   |> Seq.toList
   |> List.sortByDescending (fun txn -> txn.Timestamp, txn.Id)
