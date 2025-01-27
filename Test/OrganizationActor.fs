module OrganizationActorTests

open System
open Expecto
open Akkling
open Akkling.Cluster.Sharding
open Akka.Actor
open Akka.Persistence.Extras

open Util
open ActorUtil
open Bank.Org.Domain
open Bank.Account.Domain
open Bank.Employee.Domain
open CommandApprovalRule
open CommandApprovalProgress
open Lib.SharedTypes

module Stub = OrganizationStub

// NOTE: Change default snapshot store from local file system
//       to in memory.
let config =
   Configuration.parse
      """
      akka.persistence.snapshot-store.plugin = "akka.persistence.snapshot-store.inmem"
      """

// Mock PersistenceSupervisor message wrapping for command
// intended to be persisted
let envelope msg =
   ConfirmableMessageEnvelope(Int64.MinValue, "", msg)

let initMockEmployeeActor (tck: TestKit.Tck) =
   let handler (ctx: Actor<_>) (msg: obj) =
      match msg with
      | :? ShardEnvelope as envelope ->
         match envelope.Message with
         | :? EmployeeMessage as msg ->
            match msg with
            | EmployeeMessage.StateChange cmd ->
               cmd |> EmployeeMessage.StateChange |> tck.TestActor.Tell

               ignored ()
            | msg -> unhandled msg
         | msg -> unhandled msg
      | msg -> unhandled msg

   spawn tck "employee-mock" <| props (actorOf2 handler)

let initMockAccountActor (tck: TestKit.Tck) =
   let handler (ctx: Actor<_>) (msg: obj) =
      match msg with
      | :? ShardEnvelope as envelope ->
         match envelope.Message with
         | :? AccountMessage as msg ->
            match msg with
            | AccountMessage.StateChange cmd ->
               cmd |> AccountMessage.StateChange |> tck.TestActor.Tell

               ignored ()
            | msg -> unhandled msg
         | msg -> unhandled msg
      | msg -> unhandled msg

   spawn tck "account-mock" <| props (actorOf2 handler)

// Mock PersistenceSupervisor forwards messages to OrgActor
// & wraps StateChange messages intended to be persisted.
let mockPersistenceSupervisorProps
   (spawnChild: Actor<OrgMessage> -> IActorRef<obj>)
   =
   let init (ctx: Actor<OrgMessage>) =
      let child = spawnChild ctx

      actor {
         let! msg = ctx.Receive()

         if OrgActor.isPersistableMessage msg then
            child <<! envelope msg
         else
            child <<! msg

         return ignored ()
      }

   props init

let init (tck: TestKit.Tck) =
   let getEmployeeRef = getEmployeeEntityRef (initMockEmployeeActor tck)

   let getAccountRef = getAccountEntityRef (initMockAccountActor tck)

   let prop =
      mockPersistenceSupervisorProps (fun ctx ->
         spawn ctx ActorMetadata.org.Name
         <| OrgActor.actorProps getEmployeeRef getAccountRef)

   let orgActor = spawn tck ActorMetadata.employee.Name prop

   {| orgActor = orgActor |}

let setupOrg (tck: TestKit.Tck) (orgActor: IActorRef<OrgMessage>) =
   let cmd = Stub.createOrgCommand |> OrgCommand.CreateOrg
   let msg = OrgMessage.StateChange cmd
   orgActor <! msg

   tck.AwaitAssert(
      fun () ->
         orgActor <! OrgMessage.GetOrg
         let orgOpt = tck.ExpectMsg<Option<Org>>()
         let org = Expect.wantSome orgOpt ""

         Expect.equal
            org.Status
            OrgStatus.Active
            "org status should be active after onboarding finalized"

      , duration = TimeSpan.FromSeconds 5
      , interval = TimeSpan.FromSeconds 1
   )

let setupApprovalRequest
   (tck: TestKit.Tck)
   (orgActor: IActorRef<OrgMessage>)
   (cmdType: ApprovableCommandType)
   : ConfigureApprovalRuleCommand *
     CommandApprovalRule.T *
     CommandApprovalProgress.T
   =
   let rule = {
      RuleId = Stub.ruleId ()
      OrgId = Stub.orgId
      CommandType = cmdType
      Criteria = Criteria.PerCommand
      Approvers = [ Approver.AnyAdmin; Approver.AnyAdmin ]
   }

   let configureApprovalRuleCmd =
      ConfigureApprovalRuleCommand.create
         rule.OrgId
         (Guid.NewGuid() |> EmployeeId |> InitiatedById)
         rule

   let msg =
      configureApprovalRuleCmd
      |> OrgCommand.ConfigureApprovalRule
      |> OrgMessage.StateChange

   orgActor <! msg

   let cmd =
      match cmdType with
      | ApprovableCommandType.ApprovablePerCommand UpdateEmployeeRoleCommandType ->
         Stub.updateRoleCommand
      | ApprovableCommandType.ApprovablePerCommand InviteEmployeeCommandType ->
         Stub.inviteEmployeeCommand
      | _ -> failwith $"Unhandled command type in setupApprovalRequest"

   orgActor <! OrgMessage.ApprovableRequest cmd

   let mutable progress = None

   tck.AwaitAssert(
      fun () ->
         orgActor <! OrgMessage.GetOrg
         let orgOpt = tck.ExpectMsg<Option<Org>>()
         let org = Expect.wantSome orgOpt ""
         progress <- Seq.tryHead org.CommandApprovalProgress.Values

         Expect.isSome
            progress
            "progress should be in the CommandApprovalProgress Map after sending
            an OrgMessage.ApprovableRequest for a command which has a command
            approval rule configured"

      , duration = TimeSpan.FromSeconds 5
      , interval = TimeSpan.FromSeconds 1
   )

   configureApprovalRuleCmd, rule, progress.Value

[<Tests>]
let tests =
   testList "Organization Actor" [
      akkaTest
         "Org Actor should forward commands to employee actor if no
          approval required"
      <| Some config
      <| fun tck ->
         let o = init tck
         setupOrg tck o.orgActor

         let cmd = Stub.updateRoleCommand
         let msg = OrgMessage.ApprovableRequest cmd

         o.orgActor <! msg

         let msg = tck.ExpectMsg<EmployeeMessage>()

         match msg with
         | EmployeeMessage.StateChange(EmployeeCommand.UpdateRole _) ->
            Expect.isTrue true ""
         | _ ->
            Expect.isTrue
               false
               "Expected the employee actor to receive an UpdateRole command
               immediately following the OrgMessage.ApprovableRequest since
               there are no rules requiring approval for this command."

      akkaTest
         "Org Actor should forward commands to account actor if no
          approval required"
      <| Some config
      <| fun tck ->
         let o = init tck
         setupOrg tck o.orgActor

         let cmd =
            AccountStub.command.domesticTransfer 100m
            |> DomesticTransfer
            |> ApprovableCommand.AmountBased

         let msg = OrgMessage.ApprovableRequest cmd

         o.orgActor <! msg

         let msg = tck.ExpectMsg<AccountMessage>()

         match msg with
         | AccountMessage.StateChange(AccountCommand.DomesticTransfer _) ->
            Expect.isTrue true ""
         | _ ->
            Expect.isTrue
               false
               "Expected the account actor to receive a DomesticTransfer command
               immediately following the OrgMessage.ApprovableRequest since
               there are no rules requiring approval for this command."

      akkaTest
         "Org Actor should initiate an approval request if it receives a
         command for which a command approval rule is configured"
      <| Some config
      <| fun tck ->
         let o = init tck
         setupOrg tck o.orgActor

         // The Expect side effects within this setup will determine the outcome
         // of this test.
         setupApprovalRequest tck o.orgActor |> ignore

      akkaTest
         "Org Actor should forward commands with Pending approval progress if
          their associated rules are deleted."
      <| Some config
      <| fun tck ->
         let o = init tck
         setupOrg tck o.orgActor

         let configureApprovalRuleCmd, rule, _ =
            setupApprovalRequest
               tck
               o.orgActor
               (ApprovableCommandType.ApprovablePerCommand
                  UpdateEmployeeRoleCommandType)

         let deleteRuleCmd =
            DeleteApprovalRuleCommand.create {
               RuleId = rule.RuleId
               OrgId = rule.OrgId
               CommandType = rule.CommandType
               DeletedBy = {
                  EmployeeId =
                     InitiatedById.toEmployeeId
                        configureApprovalRuleCmd.InitiatedBy
                  EmployeeName = ""
               }
            }
            |> OrgCommand.DeleteApprovalRule

         o.orgActor <! OrgMessage.StateChange deleteRuleCmd

         let msg = tck.ExpectMsg<EmployeeMessage>()

         match msg with
         | EmployeeMessage.StateChange(EmployeeCommand.UpdateRole _) ->
            Expect.isTrue true ""
         | _ ->
            Expect.isTrue
               false
               "Expected the employee actor to receive an UpdateRole command
                associated with an in-progress approval item following the deletion
                of the rule pertaining to UpdateRole commands."

      akkaTest
         "Org Actor should forward commands with Pending approval progress if
          the rule's approvers are edited such that no more approvals are needed."
      <| Some config
      <| fun tck ->
         let o = init tck
         setupOrg tck o.orgActor

         let configureApprovalRuleCmd, _, _ =
            setupApprovalRequest
               tck
               o.orgActor
               (ApprovableCommandType.ApprovablePerCommand
                  UpdateEmployeeRoleCommandType)

         let cmd =
            {
               configureApprovalRuleCmd with
                  Data.Rule.Approvers = [ Approver.AnyAdmin ]
            }
            |> OrgCommand.ConfigureApprovalRule

         o.orgActor <! OrgMessage.StateChange cmd

         let msg = tck.ExpectMsg<EmployeeMessage>()

         match msg with
         | EmployeeMessage.StateChange(EmployeeCommand.UpdateRole _) ->
            Expect.isTrue true ""
         | _ ->
            Expect.isTrue
               false
               "Expected the employee actor to receive an UpdateRole command
                associated with an in-progress approval item after editing the
                rules list of approvers such that no more approvals are needed."

      akkaTest
         "Org Actor should forward commands with Pending approval progress if
          when enough approvals have been acquired."
      <| Some config
      <| fun tck ->
         let o = init tck
         setupOrg tck o.orgActor

         let _, rule, progress =
            setupApprovalRequest
               tck
               o.orgActor
               (ApprovableCommandType.ApprovablePerCommand
                  UpdateEmployeeRoleCommandType)

         // NOTE:
         // The rule is configured with 2 approvers (AnyAdmin; AnyAdmin).
         // In such a case the initial request for command approval will itself
         // acquire an approval (the requesting admin).  Thus, 1 approval
         // remains to complete the approval progress & initiate the command.
         let cmd =
            AcquireCommandApproval.create rule.OrgId {
               RuleId = rule.RuleId
               ApprovedBy = {
                  EmployeeId = Guid.NewGuid() |> EmployeeId
                  EmployeeName = "B"
               }
               ProgressId = progress.ProgressId
               Command = progress.CommandToInitiateOnApproval
            }
            |> OrgCommand.AcquireCommandApproval

         o.orgActor <! OrgMessage.StateChange cmd

         let msg = tck.ExpectMsg<EmployeeMessage>()

         match msg with
         | EmployeeMessage.StateChange(EmployeeCommand.UpdateRole _) ->
            Expect.isTrue true ""
         | _ ->
            Expect.isTrue
               false
               "Expected the employee actor to receive an UpdateRole command
                associated with an in-progress approval item after enough
                approvals have been acquired."

      akkaTest
         "Org Actor should send a CancelInvitationCommand to Employee Actor if
          an InviteEmployee approvable progress is declined."
      <| Some config
      <| fun tck ->
         let o = init tck
         setupOrg tck o.orgActor

         let _, rule, progress =
            setupApprovalRequest
               tck
               o.orgActor
               (ApprovableCommandType.ApprovablePerCommand
                  InviteEmployeeCommandType)

         let cmd =
            DeclineCommandApproval.create rule.OrgId {
               RuleId = rule.RuleId
               DeclinedBy = {
                  EmployeeId = Guid.NewGuid() |> EmployeeId
                  EmployeeName = "B"
               }
               ProgressId = progress.ProgressId
               Command = progress.CommandToInitiateOnApproval
            }
            |> OrgCommand.DeclineCommandApproval

         o.orgActor <! OrgMessage.StateChange cmd

         let msg = tck.ExpectMsg<EmployeeMessage>()

         match msg with
         | EmployeeMessage.StateChange(EmployeeCommand.CancelInvitation _) ->
            Expect.isTrue true ""
         | _ ->
            Expect.isTrue
               false
               "Expected the employee actor to receive an CancelInvitation command
                associated with an in-progress approval item after the approval
                is declined."
   ]
