module AccountActorTests

open System
open Expecto
open Akkling
open Akka.Actor
open Akka.Persistence.Extras

open Util
open ActorUtil
open Bank.Account.Domain
open Bank.Transfer.Domain
open BillingStatement

type private InternalTransferMsg =
   InternalTransferRecipientActor.InternalTransferMessage

type private DomesticTransferMsg =
   DomesticTransferRecipientActor.DomesticTransferMessage

// NOTE: Change default snapshot store from local file system
//       to in memory.
let config =
   Configuration.parse
      """
      akka.persistence.snapshot-store.plugin = "akka.persistence.snapshot-store.inmem"
      """

let initialDepositCommand amount =
   AccountCommand.DepositCash <| Stub.command.depositCash amount

let accountPersistence: AccountPersistence = {
   getEvents = fun _ -> Stub.accountEvents |> async.Return
}

let accountPersistenceNoEvents: AccountPersistence = {
   getEvents = fun _ -> async.Return []
}

// Mock PersistenceSupervisor message wrapping for command
// intended to be persisted
let envelope msg =
   ConfirmableMessageEnvelope(Int64.MinValue, "", msg)

// Mock PersistenceSupervisor forwards messages to AccountActor
// & wraps StateChange messages intended to be persisted.
let mockPersistenceSupervisorProps
   (spawnChild: Actor<AccountMessage> -> IActorRef<obj>)
   =
   let init (ctx: Actor<AccountMessage>) =
      let child = spawnChild ctx

      actor {
         let! msg = ctx.Receive()

         if AccountActor.isPersistableMessage msg then
            child <<! envelope msg
         else
            child <<! msg

         return ignored ()
      }

   props init

let init (tck: TestKit.Tck) (accountPersistence: AccountPersistence) =
   let internalTransferProbe = tck.CreateTestProbe()
   let domesticTransferProbe = tck.CreateTestProbe()
   let emailProbe = tck.CreateTestProbe()
   let accountClosureProbe = tck.CreateTestProbe()
   let billingProbe = tck.CreateTestProbe()

   let getOrStartInternalTransferActor (_: Actor<_>) =
      typed internalTransferProbe :> IActorRef<InternalTransferMsg>

   let getDomesticTransferActor (_: ActorSystem) =
      typed domesticTransferProbe :> IActorRef<DomesticTransferMsg>

   let getEmailActor (_: ActorSystem) =
      (typed emailProbe :> IActorRef<EmailActor.EmailMessage>)

   let getAccountClosureActor (_: ActorSystem) =
      typed accountClosureProbe :> IActorRef<AccountClosureMessage>

   let getBillingStatementActor (_: ActorSystem) =
      typed billingProbe :> IActorRef<BillingStatementMessage>

   let prop =
      mockPersistenceSupervisorProps (fun ctx ->
         spawn ctx ActorMetadata.account.Name
         <| AccountActor.actorProps
               accountPersistence
               Stub.accountBroadcast
               getOrStartInternalTransferActor
               getDomesticTransferActor
               getEmailActor
               getAccountClosureActor
               getBillingStatementActor)

   let accountActor = spawn tck ActorMetadata.account.Name prop

   {|
      accountActor = accountActor
      internalTransferProbe = internalTransferProbe
      domesticTransferProbe = domesticTransferProbe
      emailProbe = emailProbe
      accountClosureProbe = accountClosureProbe
      billingProbe = billingProbe
   |}

[<Tests>]
let tests =
   testList "Account Actor" [
      akkaTest
         "Init account should save the account, create a user, & send a welcome email"
      <| Some config
      <| fun tck ->
         let o = init tck accountPersistence
         let cmd = AccountCommand.CreateAccount Stub.command.createAccount
         o.accountActor <! AccountMessage.StateChange cmd
         o.accountActor <! AccountMessage.GetAccount

         let state = tck.ExpectMsg<Option<Account>>()

         Expect.equal
            (Stub.accountStateOmitEvents state)
            (Stub.accountStateAfterCreate |> Some |> Stub.accountStateOmitEvents)
            "Account state after CreateAccountCommand should be initialized"

         let msg = o.emailProbe.ExpectMsg<EmailActor.EmailMessage>()

         match msg with
         | EmailActor.EmailMessage.AccountOpen account ->
            Expect.equal
               (account |> Some |> Stub.accountStateOmitEvents)
               (Stub.accountStateAfterCreate
                |> Some
                |> Stub.accountStateOmitEvents)
               "EmailActor should receive AccountOpen message with created
               account"
         | msg ->
            Expect.isTrue
               false
               $"Expected AccountOpen EmailMessage. Received {msg}"

      akkaTest "Close account should interact with the AccountClosureActor"
      <| Some config
      <| fun tck ->
         let o = init tck accountPersistence
         let cmd = AccountCommand.CreateAccount Stub.command.createAccount
         o.accountActor <! AccountMessage.StateChange cmd

         let cmd = AccountCommand.CloseAccount Stub.command.closeAccount
         o.accountActor <! AccountMessage.StateChange cmd
         o.accountActor <! AccountMessage.GetAccount

         let expectedState = {
            Stub.accountStateAfterCreate with
               Status = AccountStatus.Closed
         }

         let state = tck.ExpectMsg<Option<Account>>()

         Expect.equal
            (Stub.accountStateOmitEvents state)
            (expectedState |> Some |> Stub.accountStateOmitEvents)
            "Account state should be closed"

         let msg = o.accountClosureProbe.ExpectMsg<AccountClosureMessage>()

         match msg with
         | AccountClosureMessage.Register account ->
            Expect.equal
               (account |> Some |> Stub.accountStateOmitEvents)
               (expectedState |> Some |> Stub.accountStateOmitEvents)
               "AccountClosure actor should receive a Register message with an
                account intended to close"
         | msg ->
            Expect.isTrue
               false
               $"Expected Register AccountClosureMessage. Received {msg}"

      akkaTest
         "A failed debit due to insufficient balance should notify the EmailActor"
      <| Some config
      <| fun tck ->
         let o = init tck accountPersistence
         let cmd = AccountCommand.CreateAccount Stub.command.createAccount
         o.accountActor <! AccountMessage.StateChange cmd

         o.accountActor
         <! AccountMessage.StateChange(initialDepositCommand 2000m)

         let debit = Stub.command.debit 2001m

         o.accountActor
         <! AccountMessage.StateChange(AccountCommand.Debit debit)

         o.accountActor <! AccountMessage.GetAccount

         let state = tck.ExpectMsg<Option<Account>>()
         let account = Expect.wantSome state ""

         Expect.equal
            account.Balance
            2000m
            "Account state should be unchanged after failed debit validation due
            to insufficient balance"

         // account create email
         o.emailProbe.ExpectMsg<EmailActor.EmailMessage>() |> ignore

         let msg = o.emailProbe.ExpectMsg<EmailActor.EmailMessage>()

         match msg with
         | EmailActor.EmailMessage.DebitDeclined(errMsg, account) ->
            Expect.stringContains
               errMsg
               "InsufficientBalance"
               "DebitDeclined message sent due to InsufficientBalance"
         | msg ->
            Expect.isTrue
               false
               $"Expected DebitDeclined EmailMessage. Received {msg}"

      akkaTest
         "A failed debit due to exceeding daily debit allowance should notify the EmailActor"
      <| Some config
      <| fun tck ->
         let o = init tck accountPersistence
         let cmd = AccountCommand.CreateAccount Stub.command.createAccount
         o.accountActor <! AccountMessage.StateChange cmd

         o.accountActor
         <! AccountMessage.StateChange(initialDepositCommand 2000m)

         let cmd =
            AccountCommand.LimitDailyDebits
            <| Stub.command.limitDailyDebits 100m

         o.accountActor <! AccountMessage.StateChange cmd
         o.accountActor <! AccountMessage.GetAccount

         let state = tck.ExpectMsg<Option<Account>>()
         let account = Expect.wantSome state ""

         Expect.equal
            account.DailyDebitLimit
            100m
            "Account state should be configured with the daily debit limit"

         let debit1 = AccountCommand.Debit <| Stub.command.debit 98m
         let debit2 = AccountCommand.Debit <| Stub.command.debit 33m
         o.accountActor <! AccountMessage.StateChange debit1
         o.accountActor <! AccountMessage.StateChange debit2
         o.accountActor <! AccountMessage.GetAccount
         let state = tck.ExpectMsg<Option<Account>>()
         let account = Expect.wantSome state ""

         Expect.equal
            account.Balance
            (2000m - 98m)
            "Account balance should reflect debits over daily accrued amount being rejected"

         o.emailProbe.ExpectMsg<EmailActor.EmailMessage>() |> ignore
         let msg = o.emailProbe.ExpectMsg<EmailActor.EmailMessage>()

         match msg with
         | EmailActor.EmailMessage.DebitDeclined(errMsg, account) ->
            Expect.stringContains
               errMsg
               "ExceededDailyDebit"
               "DebitDeclined message sent due to ExceededDailyDebit"
         | msg ->
            Expect.isTrue
               false
               $"Expected DebitDeclined EmailMessage. Received {msg}"

      akkaTest
         "An internal transfer should message the InternalTransferRecipientActor"
      <| Some config
      <| fun tck ->
         let o = init tck accountPersistence
         let cmd = AccountCommand.CreateAccount Stub.command.createAccount
         o.accountActor <! AccountMessage.StateChange cmd

         o.accountActor
         <! AccountMessage.StateChange(initialDepositCommand 2000m)

         let cmd =
            AccountCommand.RegisterInternalTransferRecipient
               Stub.command.registerInternalRecipient

         o.accountActor <! AccountMessage.StateChange cmd

         let transfer = Stub.command.internalTransfer 33m
         let cmd = AccountCommand.InternalTransfer transfer
         o.accountActor <! AccountMessage.StateChange cmd
         o.accountActor <! AccountMessage.GetAccount

         let state = tck.ExpectMsg<Option<Account>>()

         Expect.isTrue state.IsSome "account state is not None"

         Expect.equal
            state.Value.Balance
            (2000m - transfer.Data.Amount)
            "Account state should reflect a transfer debit"

         o.internalTransferProbe.ExpectMsg<InternalTransferMsg>() |> ignore

      akkaTest
         "A domestic transfer should message the DomesticTransferRecipientActor"
      <| Some config
      <| fun tck ->
         let o = init tck accountPersistence
         let cmd = AccountCommand.CreateAccount Stub.command.createAccount
         o.accountActor <! AccountMessage.StateChange cmd

         o.accountActor
         <! AccountMessage.StateChange(initialDepositCommand 2000m)

         let msg =
            Stub.command.registerDomesticRecipient
            |> AccountCommand.RegisterDomesticTransferRecipient
            |> AccountMessage.StateChange

         o.accountActor <! msg

         o.accountActor <! AccountMessage.GetAccount
         let state = tck.ExpectMsg<Option<Account>>()
         let account = Expect.wantSome state ""

         let recipientStubIdOverride =
            account.DomesticTransferRecipients.Head().Key

         let transfer = Stub.command.domesticTransfer 31m

         let transfer = {
            transfer with
               Data.Recipient.AccountId = recipientStubIdOverride
         }

         let cmd = AccountCommand.DomesticTransfer transfer

         o.accountActor <! AccountMessage.StateChange cmd
         o.accountActor <! AccountMessage.GetAccount

         let state = tck.ExpectMsg<Option<Account>>()

         Expect.isTrue state.IsSome "account state is not None"

         Expect.equal
            state.Value.Balance
            (2000m - transfer.Data.Amount)
            "Account state should reflect a transfer debit"

         let msg = o.domesticTransferProbe.ExpectMsg<DomesticTransferMsg>()

         match msg with
         | DomesticTransferMsg.TransferRequest(action, evt) ->
            Expect.isTrue true ""
         | msg ->
            Expect.isTrue
               false
               $"Expected TransferPending DomesticTransfer Message. Received {msg}"

      akkaTest "Receiving a transfer deposit should notify the EmailActor"
      <| Some config
      <| fun tck ->
         let o = init tck accountPersistence
         let cmd = AccountCommand.CreateAccount Stub.command.createAccount
         o.accountActor <! AccountMessage.StateChange cmd

         o.accountActor
         <! AccountMessage.StateChange(initialDepositCommand 2000m)

         let deposit = Stub.command.depositTransfer 101m

         o.accountActor
         <! AccountMessage.StateChange(AccountCommand.DepositTransfer deposit)

         o.accountActor <! AccountMessage.GetAccount

         let state = tck.ExpectMsg<Option<Account>>()
         let account = Expect.wantSome state ""

         Expect.equal
            account.Balance
            2101m
            "Account state should reflect a received transfer"

         // account create email
         o.emailProbe.ExpectMsg<EmailActor.EmailMessage>() |> ignore

         match o.emailProbe.ExpectMsg<EmailActor.EmailMessage>() with
         | EmailActor.EmailMessage.TransferDeposited(evt, account) ->
            Expect.equal
               (Some account)
               state
               "EmailActor should receive a TransferDeposited message"
         | msg ->
            Expect.isTrue
               false
               $"Expected TransferDeposited EmailMessage. Received {msg}"

      akkaTest
         "AccountActor interacts with BillingStatementActor & EmailActor
          when account transactions for a billing cycle found."
      <| Some config
      <| fun tck ->
         let o = init tck accountPersistence

         for command in Stub.commands do
            o.accountActor <! AccountMessage.StateChange command

         // Drop balance below maintenance fee threshold
         let cmd = AccountCommand.Debit <| Stub.command.debit 1500m
         o.accountActor <! AccountMessage.StateChange cmd

         o.accountActor <! AccountMessage.GetAccount
         let initAccount = tck.ExpectMsg<Option<Account>>().Value

         let msg =
            StartBillingCycleCommand.create initAccount.CompositeId {
               Reference = None
            }
            |> AccountCommand.StartBillingCycle
            |> AccountMessage.StateChange

         o.accountActor <! msg

         let (RegisterBillingStatement statement) =
            o.billingProbe.ExpectMsg<BillingStatementMessage>()

         Expect.equal
            statement.Balance
            initAccount.Balance
            "Billing statement balance should = account balance"

         Expect.equal
            statement.AccountId
            initAccount.AccountId
            "Billing statement AccountId should = account AccountId"

         let getEventName =
            BillingTransaction.value
            >> AccountEnvelope.unwrap
            >> snd
            >> _.EventName

         Expect.sequenceEqual
            (statement.Transactions |> List.map getEventName)
            ("DebitedAccount"
             :: (Stub.billingTransactions |> List.map getEventName |> List.rev))
            "RegisterBillingStatements msg should send transactions equivalent
             to those associated with the account events"

         // account create email
         o.emailProbe.ExpectMsg<EmailActor.EmailMessage>() |> ignore

         match o.emailProbe.ExpectMsg<EmailActor.EmailMessage>() with
         | EmailActor.BillingStatement account ->
            Expect.equal
               account
               initAccount
               "EmailActor BillingStatement message should contain
                an account record equivalent to the current account state"
         | msg ->
            Expect.isTrue
               false
               $"EmailActor expects BillingStatement message.
                 Received message: {msg}"

         o.accountActor <! AccountMessage.GetAccount

         let accountAfterBillingCycle = tck.ExpectMsg<Option<Account>>().Value

         Expect.equal
            accountAfterBillingCycle.Balance
            (initAccount.Balance - MaintenanceFee.RecurringDebitAmount)
            "Account balance after billing cycle should be decremented by
             maintenance fee"

      akkaTest
         "Maintenance fee should be skipped when maintenance fee criteria met."
      <| Some config
      <| fun tck ->
         let o = init tck accountPersistence

         for command in Stub.commands do
            o.accountActor <! AccountMessage.StateChange command

         o.accountActor <! AccountMessage.GetAccount

         let initAccount = tck.ExpectMsg<Option<Account>>().Value

         let msg =
            StartBillingCycleCommand.create initAccount.CompositeId {
               Reference = None
            }
            |> AccountCommand.StartBillingCycle
            |> AccountMessage.StateChange

         o.accountActor <! msg

         o.accountActor <! AccountMessage.GetAccount

         let accountAfterBillingCycle = tck.ExpectMsg<Option<Account>>().Value

         Expect.equal
            accountAfterBillingCycle.Balance
            initAccount.Balance
            "Account balance after billing cycle should not change since
             the maintenance fee balance threshold is met."
   ]
