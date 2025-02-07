[<RequireQualifiedAccess>]
module InternalTransferRecipientActor

open Akkling
open Akkling.Cluster.Sharding

open ActorUtil
open Bank.Account.Domain
open Bank.Transfer.Domain
open Lib.SharedTypes

type private FailReason = InternalTransferFailReason

[<RequireQualifiedAccess>]
type InternalTransferMessage =
   | AutomatedTransferRequest of BankEvent<InternalAutomatedTransferPending>
   | TransferRequestWithinOrg of BankEvent<InternalTransferWithinOrgPending>
   | TransferRequestBetweenOrgs of BankEvent<InternalTransferBetweenOrgsPending>

let private failTransferWithinOrgMsg
   (evt: BankEvent<InternalTransferWithinOrgPending>)
   (reason: FailReason)
   =
   let info = evt.Data.BaseInfo

   FailInternalTransferWithinOrgCommand.create
      (info.Sender.AccountId, info.Sender.OrgId)
      evt.CorrelationId
      evt.InitiatedById
      { BaseInfo = info; Reason = reason }
   |> AccountCommand.FailInternalTransfer
   |> AccountMessage.StateChange

let private failTransferBetweenOrgsMsg
   (evt: BankEvent<InternalTransferBetweenOrgsPending>)
   (reason: FailReason)
   =
   let info = evt.Data.BaseInfo

   FailInternalTransferBetweenOrgsCommand.create
      (info.Sender.AccountId, evt.OrgId)
      evt.CorrelationId
      evt.InitiatedById
      { BaseInfo = info; Reason = reason }
   |> AccountCommand.FailInternalTransferBetweenOrgs
   |> AccountMessage.StateChange

let private failAutoTransferMsg
   (evt: BankEvent<InternalAutomatedTransferPending>)
   (reason: FailReason)
   =
   let info = evt.Data.BaseInfo

   FailInternalAutoTransferCommand.create
      (info.Sender.AccountId, info.Sender.OrgId)
      evt.CorrelationId
      evt.InitiatedById
      {
         Rule = evt.Data.Rule
         BaseInfo = info
         Reason = reason
      }
   |> AccountCommand.FailInternalAutoTransfer
   |> AccountMessage.StateChange

let private completeTransferWithinOrgMsg
   (evt: BankEvent<InternalTransferWithinOrgPending>)
   =
   let info = evt.Data.BaseInfo

   CompleteInternalTransferWithinOrgCommand.create
      (info.Sender.AccountId, evt.OrgId)
      evt.CorrelationId
      evt.InitiatedById
      { BaseInfo = info }
   |> AccountCommand.CompleteInternalTransfer
   |> AccountMessage.StateChange

let private completeTransferBetweenOrgsMsg
   (evt: BankEvent<InternalTransferBetweenOrgsPending>)
   =
   let info = evt.Data.BaseInfo

   CompleteInternalTransferBetweenOrgsCommand.create
      (info.Sender.AccountId, evt.OrgId)
      evt.CorrelationId
      evt.InitiatedById
      { BaseInfo = info }
   |> AccountCommand.CompleteInternalTransferBetweenOrgs
   |> AccountMessage.StateChange

let private completeAutoTransferMsg
   (evt: BankEvent<InternalAutomatedTransferPending>)
   =
   let info = evt.Data.BaseInfo

   CompleteInternalAutoTransferCommand.create
      (info.Sender.AccountId, evt.OrgId)
      evt.CorrelationId
      evt.InitiatedById
      {
         Rule = evt.Data.Rule
         BaseInfo = info
      }
   |> AccountCommand.CompleteInternalAutoTransfer
   |> AccountMessage.StateChange

let actorProps
   (getAccountRef: AccountId -> IEntityRef<AccountMessage>)
   : Props<InternalTransferMessage>
   =
   let handler (ctx: Actor<_>) (msg: InternalTransferMessage) = actor {
      let logWarning = logWarning ctx

      let failTransferMsg =
         match msg with
         | InternalTransferMessage.TransferRequestWithinOrg e ->
            failTransferWithinOrgMsg e
         | InternalTransferMessage.TransferRequestBetweenOrgs e ->
            failTransferBetweenOrgsMsg e
         | InternalTransferMessage.AutomatedTransferRequest e ->
            failAutoTransferMsg e

      let completeTransferMsg () =
         match msg with
         | InternalTransferMessage.TransferRequestWithinOrg e ->
            completeTransferWithinOrgMsg e
         | InternalTransferMessage.TransferRequestBetweenOrgs e ->
            completeTransferBetweenOrgsMsg e
         | InternalTransferMessage.AutomatedTransferRequest e ->
            completeAutoTransferMsg e

      let info, meta =
         match msg with
         | InternalTransferMessage.TransferRequestWithinOrg e ->
            e.Data.BaseInfo,
            {|
               CorrId = e.CorrelationId
               InitiatedBy = e.InitiatedById
            |}
         | InternalTransferMessage.TransferRequestBetweenOrgs e ->
            e.Data.BaseInfo,
            {|
               CorrId = e.CorrelationId
               InitiatedBy = e.InitiatedById
            |}
         | InternalTransferMessage.AutomatedTransferRequest e ->
            e.Data.BaseInfo,
            {|
               CorrId = e.CorrelationId
               InitiatedBy = e.InitiatedById
            |}

      let recipientId = info.Recipient.AccountId
      let senderId = info.Sender.AccountId
      let recipientAccountRef = getAccountRef recipientId
      let senderAccountRef = getAccountRef senderId

      let! (recipientAccountOpt: Account option) =
         recipientAccountRef <? AccountMessage.GetAccount

      match recipientAccountOpt with
      | None ->
         logWarning $"Transfer recipient not found {recipientId}"

         senderAccountRef <! failTransferMsg FailReason.InvalidAccountInfo
      | Some recipientAccount ->
         if recipientAccount.Status = AccountStatus.Closed then
            logWarning $"Transfer recipient account closed"

            senderAccountRef <! failTransferMsg FailReason.AccountClosed
         else
            senderAccountRef <! completeTransferMsg ()

            // CorrelationId (here as txn.TransactionId) from sender
            // transfer request traced to receiver's deposit.
            let msg =
               match msg with
               | InternalTransferMessage.TransferRequestWithinOrg _ ->
                  DepositInternalTransferWithinOrgCommand.create
                     recipientAccount.CompositeId
                     meta.CorrId
                     meta.InitiatedBy
                     { BaseInfo = info }
                  |> AccountCommand.DepositTransferWithinOrg
                  |> AccountMessage.StateChange
               | InternalTransferMessage.TransferRequestBetweenOrgs _ ->
                  DepositInternalTransferBetweenOrgsCommand.create
                     recipientAccount.CompositeId
                     meta.CorrId
                     meta.InitiatedBy
                     { BaseInfo = info }
                  |> AccountCommand.DepositTransferBetweenOrgs
                  |> AccountMessage.StateChange
               | InternalTransferMessage.AutomatedTransferRequest evt ->
                  DepositInternalAutoTransferCommand.create
                     recipientAccount.CompositeId
                     meta.CorrId
                     meta.InitiatedBy
                     {
                        BaseInfo = info
                        Rule = evt.Data.Rule
                     }
                  |> AccountCommand.DepositInternalAutoTransfer
                  |> AccountMessage.StateChange

            recipientAccountRef <! msg
   }

   props <| actorOf2 handler

let getOrStart
   mailbox
   (getAccountRef: AccountId -> IEntityRef<AccountMessage>)
   =
   ActorMetadata.internalTransfer.Name
   |> getChildActorRef<_, InternalTransferMessage> mailbox
   |> Option.defaultWith (fun _ ->
      spawn mailbox ActorMetadata.internalTransfer.Name
      <| actorProps getAccountRef)
