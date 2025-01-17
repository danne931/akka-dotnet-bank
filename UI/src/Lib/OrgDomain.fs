module UIDomain.Org

open Bank.Org.Domain
open Lib.SharedTypes

type OrgCommandReceipt = {
   PendingCommand: OrgCommand
   PendingEvent: OrgEvent
   PendingState: Org
   Envelope: Envelope
}

type CommandApprovalProgressWithRuleMaybe =
   Result<
      Map<CommandApprovalProgressId, CommandApprovalProgressWithRule> option,
      Err
    >
