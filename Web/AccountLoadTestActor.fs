[<RequireQualifiedAccess>]
module AccountLoadTestActor

open System
open Akka.Hosting
open Akka.Actor
open Akkling
open Akkling.Cluster.Sharding

open ActorUtil
open Lib.SharedTypes
open Bank.Account.Domain
open AccountLoadTestTypes

// NOTE:
// Dev/staging facility to flood the system with
// CreateAccount & Deposit commands.
// Run to verify that AccountActors, ReadModelSyncActor,
// BillingCycleActor, BillingStatementActors, & AccountClosureActors
// can handle increased throughput.
//
// TODO:
// Measure requests per second from time of DepositCash command to
// time DepositedCash event is successfully persisted.

type UnexpectedBalance = { Expected: decimal; Actual: decimal }

type AccountTest = {
   AccountId: AccountId
   UnexpectedBalance: UnexpectedBalance option
   DepositsRemaining: int
}

type ProgressIndicator =
   | Idle
   | InProgress
   | Finished

type AccountLoadTestStateMessage = {
   Progress: ProgressIndicator
   ProcessedCount: int
   RemainingCount: int
   UnexpectedBalances: Map<AccountId, UnexpectedBalance>
}

type AccountLoadTestState = {
   Progress: ProgressIndicator
   AccountTests: Map<AccountId, AccountTest>
   ProcessedCount: int
   RemainingCount: int
   UnexpectedBalances: Map<AccountId, UnexpectedBalance>
} with

   member x.asMessage() = {
      Progress = x.Progress
      ProcessedCount = x.ProcessedCount
      RemainingCount = x.RemainingCount
      UnexpectedBalances = x.UnexpectedBalances
   }

module private Stub =
   let depositAmount = 3m
   let balanceAfter3Deposits = depositAmount * 3m
   let orgId = Guid.NewGuid() |> OrgId
   let parentAccountId = Guid.NewGuid() |> ParentAccountId

   let initiatedBy: Initiator = {
      Id = InitiatedById Constants.LOGGED_IN_EMPLOYEE_ID_REMOVE_SOON
      Name = "Daniel Eisenbarger"
   }

   let createAccountMessage accountId =
      AccountMessage.StateChange << AccountCommand.CreateVirtualAccount
      <| CreateVirtualAccountCommand.create {
         Name = "Operations"
         Currency = Currency.EUR
         Depository = AccountDepository.Checking
         AccountNumber = AccountNumber.generate () |> string
         ParentAccountId = parentAccountId
         AccountId = accountId
         OrgId = orgId
         InitiatedBy = initiatedBy
      }

   let depositMessage accountId =
      AccountMessage.StateChange << AccountCommand.DepositCash
      <| DepositCashCommand.create (parentAccountId, orgId) initiatedBy {
         AccountId = accountId
         Amount = depositAmount
         Origin = Some "load-test"
      }

   let closeAccountMessage accountId =
      AccountMessage.StateChange << AccountCommand.CloseAccount
      <| CloseAccountCommand.create (parentAccountId, orgId) initiatedBy {
         AccountId = accountId
         Reference = Some "load test - clean up"
      }

let private config = {|
   NumberOfAccountTests = 900
   RequestsPerAccountTest = 3
|}

let private prepareTestData () = {
   Progress = Idle
   ProcessedCount = 0
   RemainingCount = config.NumberOfAccountTests
   AccountTests =
      [ 1 .. config.NumberOfAccountTests ]
      |> List.map (fun _ ->
         let accountId = Guid.NewGuid() |> AccountId

         accountId,
         {
            AccountId = accountId
            UnexpectedBalance = None
            DepositsRemaining = config.RequestsPerAccountTest
         })
      |> Map.ofList
   UnexpectedBalances = Map.empty
}

let actorProps (getAccountRef: ParentAccountId -> IEntityRef<AccountMessage>) =
   let handler (mailbox: Actor<AccountLoadTestMessage>) =
      PubSub.subscribePointToPoint (PubSub.get mailbox.System) mailbox.Self

      let logInfo, logError = logInfo mailbox, logError mailbox

      let rec loop (state: AccountLoadTestState) = actor {
         let! msg = mailbox.Receive()

         return!
            match msg with
            | Lookup ->
               mailbox.Sender() <! state.asMessage ()
               ignored ()
            | CheckProgress _ when state.Progress <> InProgress -> ignored ()
            | CheckProgress check ->
               logInfo $"Progress check: {check}"
               let time = TimeSpan.FromSeconds 30.

               let maxChecks = 3

               if check.NumberOfProgressChecks = maxChecks then
                  logInfo
                     $"RemainingCount has remained same for
                            {maxChecks} progress checks. "

                  mailbox.Self <! Finish
               else if state.RemainingCount = 0 then
                  mailbox.Self <! Finish
               else
                  let check = {
                     check with
                        RemainingAccountTests = state.RemainingCount
                        NumberOfProgressChecks =
                           check.NumberOfProgressChecks + 1
                  }

                  mailbox.Schedule time mailbox.Self (CheckProgress check)
                  |> ignore

               ignored ()
            | StartLoadTest when state.Progress = InProgress ->
               logError "Load test already in progress. Ignoring start message."

               ignored ()
            | StartLoadTest when state.Progress = Finished ->
               logInfo "Reset load test."
               mailbox.Self <! StartLoadTest
               loop <| prepareTestData ()
            | StartLoadTest ->
               for test in state.AccountTests.Values do
                  let accountId = test.AccountId
                  let ref = getAccountRef Stub.parentAccountId
                  ref <! Stub.createAccountMessage accountId
                  ref <! Stub.depositMessage accountId
                  ref <! Stub.depositMessage accountId
                  ref <! Stub.depositMessage accountId

               mailbox.Schedule (TimeSpan.FromMinutes 1.) mailbox.Self
               <| CheckProgress {
                  RemainingAccountTests = state.RemainingCount
                  NumberOfProgressChecks = 0
               }
               |> ignore

               loop { state with Progress = InProgress }
            | Finish ->
               let newState = { state with Progress = Finished }

               logInfo $"Finished load test. Result: {newState}"

               loop newState
            | Teardown ->
               logInfo "Commence teardown."

               for test in state.AccountTests.Values do
                  let ref = getAccountRef Stub.parentAccountId
                  ref <! Stub.closeAccountMessage test.AccountId

               ignored ()
            | AccountEventPersisted _ when state.Progress <> InProgress ->
               ignored ()
            | AccountEventPersisted(msg) ->
               match Map.tryFind msg.AccountId state.AccountTests with
               | None -> ignored ()
               | Some test ->
                  let test =
                     match msg.Event with
                     | CreatedVirtualAccount _ -> test
                     | DepositedCash _ -> {
                        test with
                           DepositsRemaining = test.DepositsRemaining - 1
                       }
                     | _ -> test

                  if test.DepositsRemaining <> 0 then
                     loop {
                        state with
                           AccountTests =
                              Map.change
                                 test.AccountId
                                 (fun _ -> Some test)
                                 state.AccountTests
                     }
                  else
                     let expectedBalance = Stub.balanceAfter3Deposits

                     let test =
                        if msg.AccountBalance = expectedBalance then
                           test
                        else
                           {
                              test with
                                 UnexpectedBalance =
                                    Some {
                                       Expected = expectedBalance
                                       Actual = msg.AccountBalance
                                    }
                           }

                     let newState = {
                        state with
                           AccountTests =
                              Map.change
                                 test.AccountId
                                 (fun _ -> Some test)
                                 state.AccountTests
                           ProcessedCount = state.ProcessedCount + 1
                           RemainingCount = state.RemainingCount - 1
                           UnexpectedBalances =
                              match test.UnexpectedBalance with
                              | None -> state.UnexpectedBalances
                              | Some unexpected ->
                                 Map.add
                                    test.AccountId
                                    unexpected
                                    state.UnexpectedBalances
                     }

                     logInfo
                        $"Account test finished.  Remaining: {newState.RemainingCount}"

                     if test.UnexpectedBalance.IsSome then
                        logError
                           $"Unexpected balance found {test.AccountId}: {test.UnexpectedBalance.Value.Actual}"

                     if newState.RemainingCount = 0 then
                        mailbox.Self <! Finish

                     loop newState
      }

      loop <| prepareTestData ()

   props handler

let get (system: ActorSystem) : IActorRef<AccountLoadTestMessage> =
   typed <| ActorRegistry.For(system).Get<ActorMetadata.AccountLoadTestMarker>()
