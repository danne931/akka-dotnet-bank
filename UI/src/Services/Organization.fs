[<RequireQualifiedAccess>]
module OrgService

open Fable.SimpleHttp
open FsToolkit.ErrorHandling

open Bank.Org.Domain
open UIDomain.Org
open Lib.SharedTypes
open RoutePaths

let private serviceName = "OrgService"

let private notImplemented (cmd: OrgCommand) =
   let msg = $"{serviceName}: Not implemented command: {cmd}"
   Log.error msg
   failwith msg

let private postJson (command: OrgCommand) =
   let serialized, url =
      match command with
      | OrgCommand.ConfigureApprovalRule cmd ->
         Serialization.serialize cmd, OrgPath.ConfigureCommandApprovalRule
      | OrgCommand.DeleteApprovalRule cmd ->
         Serialization.serialize cmd, OrgPath.DeleteCommandApprovalRule
      | OrgCommand.AcquireCommandApproval cmd ->
         Serialization.serialize cmd, OrgPath.AcquireCommandApproval
      | OrgCommand.DeclineCommandApproval cmd ->
         Serialization.serialize cmd, OrgPath.DeclineCommandApproval
      | OrgCommand.RegisterDomesticTransferRecipient cmd ->
         Serialization.serialize cmd, TransferPath.DomesticTransferRecipient
      | OrgCommand.EditDomesticTransferRecipient cmd ->
         Serialization.serialize cmd, TransferPath.DomesticTransferRecipientEdit
      | OrgCommand.NicknameDomesticTransferRecipient cmd ->
         Serialization.serialize cmd, TransferPath.NicknameRecipient
      | other -> notImplemented other

   Http.postJson url serialized

let submitCommand
   (org: Org)
   (command: OrgCommand)
   : Async<Result<OrgCommandReceipt, Err>>
   =
   asyncResult {
      // Pre-network request validation checks the command itself
      // (Err.ValidationError) and the Result of applying the command
      // to the current state (Err.OrgStateTransitionError).
      // This same validation occurs on the server when an actor is
      // processing a command.
      let! evt, newState =
         Org.stateTransition
            {
               Info = org
               Events = []
               AccrualMetrics = Map.empty
            }
            command

      let! res = postJson command
      let code = res.statusCode

      if code <> 200 then
         return! Error <| Err.InvalidStatusCodeError(serviceName, code)
      else
         let! envelope = Serialization.deserialize<Envelope> res.responseText

         return {
            Envelope = envelope
            PendingState = newState.Info
            PendingEvent = evt
            PendingCommand = command
         }
   }

let getOrgAndAccountProfiles
   (orgId: OrgId)
   : Async<Result<OrgWithAccountProfiles option, Err>>
   =
   async {
      let path = OrgPath.get orgId

      let! (code, responseText) = Http.get path

      if code = 404 then
         return Ok None
      elif code <> 200 then
         return Error <| Err.InvalidStatusCodeError(serviceName, code)
      else
         return
            responseText
            |> Serialization.deserialize<OrgWithAccountProfiles>
            |> Result.map Some
   }

let searchOrgTransferSocialDiscovery
   (orgId: OrgId)
   (nameQuery: string)
   : Async<Result<Org list option, Err>>
   =
   async {
      let path = OrgPath.search orgId nameQuery

      let! (code, responseText) = Http.get path

      if code = 404 then
         return Ok None
      elif code <> 200 then
         return Error <| Err.InvalidStatusCodeError(serviceName, code)
      else
         return
            responseText
            |> Serialization.deserialize<Org list>
            |> Result.map Some
   }

let getMerchants (orgId: OrgId) : Async<Result<Map<string, Merchant>, Err>> = async {
   let! (code, responseText) = Http.get <| OrgPath.merchants orgId

   if code = 404 then
      return Ok Map.empty
   elif code <> 200 then
      return Error <| Err.InvalidStatusCodeError(serviceName, code)
   else
      return
         responseText
         |> Serialization.deserialize<Merchant list>
         |> Result.map (List.map (fun o -> o.Name, o) >> Map.ofList)
}

let updateMerchant (merchant: Merchant) : Async<Result<int, Err>> = async {
   let! res =
      Http.postJson
         (OrgPath.merchants merchant.OrgId)
         (Serialization.serialize merchant)

   let code = res.statusCode

   if code <> 200 then
      return Error <| Err.InvalidStatusCodeError(serviceName, code)
   else
      return Serialization.deserialize<int> res.responseText
}

let getCommandApprovalRules
   (orgId: OrgId)
   : Async<Result<CommandApprovalRule.T list option, Err>>
   =
   async {
      let path = OrgPath.getCommandApprovalRules orgId

      let! (code, responseText) = Http.get path

      if code = 404 then
         return Ok None
      elif code <> 200 then
         return Error <| Err.InvalidStatusCodeError(serviceName, code)
      else
         return
            responseText
            |> Serialization.deserialize<CommandApprovalRule.T list>
            |> Result.map Some
   }

let getCommandApprovals (orgId: OrgId) : Async<CommandApprovalProgressMaybe> = async {
   let path = OrgPath.getCommandApprovals orgId

   let! (code, responseText) = Http.get path

   if code = 404 then
      return Ok None
   elif code <> 200 then
      return Error <| Err.InvalidStatusCodeError(serviceName, code)
   else
      return
         responseText
         |> Serialization.deserialize<CommandApprovalProgress.T list>
         |> Result.map (fun progress ->
            progress
            |> List.map (fun p -> p.ProgressId, p)
            |> Map.ofList
            |> Some)
}

/// Provides employee accrual metrics before a user submits a domestic
/// transfer, internal transfer between orgs, or payment.
/// The CommandApprovalDailyAccrual metrics are used in
/// CommandApprovalRule.requiresCommandApproval to determine if an employee
/// is attempting to submit a command above the configured daily limit for that
/// command (as specified in an org's configured command approval rules).
let getTodaysAccrualMetricsByInitiatedBy
   (orgId: OrgId)
   (initiatedById: InitiatedById)
   : Async<Result<CommandApprovalDailyAccrual, Err>>
   =
   async {
      let! (code, responseText) =
         OrgPath.commandApprovalDailyAccrual orgId initiatedById |> Http.get

      if code <> 200 then
         return Error <| Err.InvalidStatusCodeError(serviceName, code)
      else
         return
            responseText
            |> Serialization.deserialize<CommandApprovalDailyAccrual>
   }
