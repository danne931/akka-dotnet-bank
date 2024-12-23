namespace Bank.Employee.Domain

open System
open System.Threading.Tasks
open Validus

open Lib.SharedTypes
open Bank.Transfer.Domain

type ApprovableCommandEnvelope = {
   EntityId: EntityId
   InitiatedBy: InitiatedById
   OrgId: OrgId
}

[<RequireQualifiedAccess>]
type ApprovableCommandType =
   | InviteEmployee
   | UpdateEmployeeRole
   | FulfillPlatformPayment
   | InternalTransferBetweenOrgs
   | DomesticTransfer

   override x.ToString() =
      match x with
      | InviteEmployee -> "InviteEmployee"
      | UpdateEmployeeRole -> "UpdateEmployeeRole"
      | FulfillPlatformPayment -> "SendPayment"
      | InternalTransferBetweenOrgs -> "SendInternalTransferBetweenOrgs"
      | DomesticTransfer -> "SendDomesticTransfer"

   member x.Display =
      match x with
      | InviteEmployee -> "Invite Employee"
      | UpdateEmployeeRole -> "Update Employee Role"
      | FulfillPlatformPayment -> "Fulfill Platform Payment"
      | InternalTransferBetweenOrgs -> "Internal Transfer Between Orgs"
      | DomesticTransfer -> "Domestic Transfer"

   static member fromString(o: string) : ApprovableCommandType option =
      if String.IsNullOrWhiteSpace o then
         None
      else
         match o with
         | "InviteEmployee" -> Some InviteEmployee
         | "UpdateEmployeeRole" -> Some UpdateEmployeeRole
         | "SendPayment" -> Some FulfillPlatformPayment
         | "SendInternalTransferBetweenOrgs" -> Some InternalTransferBetweenOrgs
         | "SendDomesticTransfer" -> Some DomesticTransfer
         | _ -> None

   static member fromStringUnsafe(o: string) : ApprovableCommandType =
      match ApprovableCommandType.fromString o with
      | None ->
         failwith "Error attempting to cast string to ApprovableCommandType"
      | Some o -> o

[<RequireQualifiedAccess>]
type ApprovableCommand =
   | InviteEmployee of ApproveAccessCommand
   | UpdateEmployeeRole of UpdateRoleCommand
   | FulfillPlatformPayment of FulfillPlatformPaymentCommand
   | InternalTransferBetweenOrgs of InternalTransferBetweenOrgsCommand
   | DomesticTransfer of RequestDomesticTransferCommand

   static member envelope(cmd: ApprovableCommand) : CommandEnvelope =
      match cmd with
      | InviteEmployee o -> Command.envelope o
      | UpdateEmployeeRole o -> Command.envelope o
      | FulfillPlatformPayment o -> Command.envelope o
      | InternalTransferBetweenOrgs o -> Command.envelope o
      | DomesticTransfer o -> Command.envelope o

   member x.InitiatedBy = ApprovableCommand.envelope x |> _.InitiatedById

   member x.EntityId = ApprovableCommand.envelope x |> _.EntityId

   member x.OrgId = ApprovableCommand.envelope x |> _.OrgId

   member x.CorrelationId = ApprovableCommand.envelope x |> _.CorrelationId

   member x.Amount =
      match x with
      | InviteEmployee _ -> 0m
      | UpdateEmployeeRole _ -> 0m
      | FulfillPlatformPayment o -> o.Data.RequestedPayment.BaseInfo.Amount
      | InternalTransferBetweenOrgs o -> o.Data.Amount
      | DomesticTransfer o -> o.Data.Amount

   member x.CommandType =
      match x with
      | InviteEmployee _ -> ApprovableCommandType.InviteEmployee
      | UpdateEmployeeRole _ -> ApprovableCommandType.UpdateEmployeeRole
      | FulfillPlatformPayment _ -> ApprovableCommandType.FulfillPlatformPayment
      | InternalTransferBetweenOrgs _ ->
         ApprovableCommandType.InternalTransferBetweenOrgs
      | DomesticTransfer _ -> ApprovableCommandType.DomesticTransfer

   member x.Display = x.CommandType.Display

module CommandApprovalRule =
   type RequiresApprovalForCommand =
      ApprovableCommand
         -> EmployeeDailyAccrual
         -> Task<Result<CommandApprovalRuleId option, Err>>

   type AmountPerCommandRange = {
      LowerBound: decimal option
      UpperBound: decimal option
   }

   [<RequireQualifiedAccess>]
   type Criteria =
      | AmountDailyLimit of limit: decimal
      | AmountPerCommand of AmountPerCommandRange
      | PerCommand

      override x.ToString() =
         match x with
         | AmountDailyLimit _ -> "AmountDailyLimit"
         | AmountPerCommand _ -> "AmountPerCommand"
         | PerCommand -> "PerCommand"

   type Approver = { Name: string; EmployeeId: EmployeeId }

   type T = {
      RuleId: CommandApprovalRuleId
      OrgId: OrgId
      CommandType: ApprovableCommandType
      Criteria: Criteria
      Approvers: Approver list
   }

   type ConfigureApprovalRuleCommand = Command<T>

   module ConfigureApprovalRuleCommand =
      let create
         (employeeId: EmployeeId, orgId: OrgId)
         (initiatedBy: InitiatedById)
         (data: ConfigureApprovalRuleCommand)
         =
         Command.create
            (EmployeeId.toEntityId employeeId)
            orgId
            (CorrelationId.create ())
            initiatedBy
            data

      let toEvent
         (cmd: ConfigureApprovalRuleCommand)
         : ValidationResult<BankEvent<T>>
         =
         Ok <| BankEvent.create<T> cmd


module CommandApprovalProgress =
   type Requester = { Name: string; Id: InitiatedById }

   [<RequireQualifiedAccess>]
   type Status =
      | Pending
      | Approved
      | Declined

      override x.ToString() =
         match x with
         | Pending -> "Pending"
         | Approved -> "Approved"
         | Declined -> "Declined"

      static member fromString(status: string) : Status option =
         if String.IsNullOrWhiteSpace status then
            None
         else
            match status with
            | "Pending" -> Some Pending
            | "Approved" -> Some Approved
            | "Declined" -> Some Declined
            | _ -> None

      static member fromStringUnsafe(status: string) : Status =
         match Status.fromString status with
         | None ->
            failwith
               "Error attempting to cast string to CommandApprovalProgress.Status"
         | Some status -> status

   type T = {
      CommandId: CommandApprovalProgressId
      RuleId: CommandApprovalRuleId
      OrgId: OrgId
      Status: Status
      ApprovedBy: CommandApprovalRule.Approver list
      DeclinedBy: CommandApprovalRule.Approver option
      CommandToInitiateOnApproval: ApprovableCommand
   }

   type CommandApprovalRequested = {
      RuleId: CommandApprovalRuleId
      Command: ApprovableCommand
   }

   type CommandApprovalAcquired = {
      ApprovedBy: InitiatedById
      CommandId: CommandApprovalProgressId
      CommandType: ApprovableCommandType
   }

   type CommandApprovalDeclined = {
      DeclinedBy: InitiatedById
      CommandId: CommandApprovalProgressId
      CommandType: ApprovableCommandType
   }

   type RequestCommandApproval = Command<CommandApprovalRequested>

   module RequestCommandApproval =
      let create
         (employeeId: EmployeeId, orgId: OrgId)
         (initiatedBy: InitiatedById)
         (correlationId: CorrelationId)
         (data: CommandApprovalRequested)
         =
         Command.create
            (EmployeeId.toEntityId employeeId)
            orgId
            correlationId
            initiatedBy
            data

      let toEvent
         (cmd: RequestCommandApproval)
         : ValidationResult<BankEvent<CommandApprovalRequested>>
         =
         Ok <| BankEvent.create<CommandApprovalRequested> cmd

   type AcquireCommandApproval = Command<CommandApprovalAcquired>

   module AcquireCommandApproval =
      let create
         (employeeId: EmployeeId, orgId: OrgId)
         (data: CommandApprovalAcquired)
         =
         let (CommandApprovalProgressId correlationId) = data.CommandId

         Command.create
            (EmployeeId.toEntityId employeeId)
            orgId
            correlationId
            data.ApprovedBy
            data

      let toEvent
         (cmd: AcquireCommandApproval)
         : ValidationResult<BankEvent<CommandApprovalAcquired>>
         =
         Ok <| BankEvent.create<CommandApprovalAcquired> cmd

   type DeclineCommandApproval = Command<CommandApprovalDeclined>

   module DeclineCommandApproval =
      let create
         (employeeId: EmployeeId, orgId: OrgId)
         (data: CommandApprovalDeclined)
         =
         let (CommandApprovalProgressId correlationId) = data.CommandId

         Command.create
            (EmployeeId.toEntityId employeeId)
            orgId
            correlationId
            data.DeclinedBy
            data

      let toEvent
         (cmd: DeclineCommandApproval)
         : ValidationResult<BankEvent<CommandApprovalDeclined>>
         =
         Ok <| BankEvent.create<CommandApprovalDeclined> cmd

type CommandApprovalProgressWithRule = {
   RuleId: CommandApprovalRuleId
   CommandProgressId: CommandApprovalProgressId
   Command: ApprovableCommand
   Criteria: CommandApprovalRule.Criteria
   PermittedApprovers: CommandApprovalRule.Approver list
   ApprovedBy: CommandApprovalRule.Approver list option
   DeclinedBy: CommandApprovalRule.Approver option
   RequestedBy: CommandApprovalProgress.Requester
   Status: CommandApprovalProgress.Status
   LastUpdate: DateTime
}

module CommandApprovalProgressWithRule =
   /// Employee may approve or deny the command if they are a PermittedApprover
   /// for a given command type & they haven't already approved the command.
   let mayApproveOrDeny
      (progress: CommandApprovalProgressWithRule)
      (eId: EmployeeId)
      =
      progress.Status = CommandApprovalProgress.Status.Pending
      && progress.PermittedApprovers
         |> List.exists (fun o -> o.EmployeeId = eId)
      && (progress.ApprovedBy
          |> Option.map (List.exists (fun o -> o.EmployeeId <> eId))
          |> Option.defaultValue true)
