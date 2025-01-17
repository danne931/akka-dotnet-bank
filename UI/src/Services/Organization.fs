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
      | OrgCommand.AcquireCommandApproval cmd ->
         Serialization.serialize cmd, OrgPath.AcquireCommandApproval
      | OrgCommand.DeclineCommandApproval cmd ->
         Serialization.serialize cmd, OrgPath.DeclineCommandApproval
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
         Org.stateTransition { Info = org; Events = [] } command

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

let orgRequiresCommandApproval
   (orgId: OrgId)
   (commandType: ApprovableCommandType)
   : Async<Result<CommandApprovalRuleId option, Err>>
   =
   async {
      let path =
         OrgPath.getCommandApprovalRuleByCommandType orgId (string commandType)

      let! (code, responseText) = Http.get path

      if code <> 200 then
         return Error <| Err.InvalidStatusCodeError(serviceName, code)
      else
         return
            responseText
            |> Serialization.deserialize<CommandApprovalRuleId option>
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

let getCommandApprovalProgressWithRule
   (progressId: CommandApprovalProgressId)
   : Async<Result<CommandApprovalProgressWithRule option, Err>>
   =
   async {
      let path = OrgPath.getCommandApprovalProgressWithRule progressId

      let! (code, responseText) = Http.get path

      if code = 404 then
         return Ok None
      elif code <> 200 then
         return Error <| Err.InvalidStatusCodeError(serviceName, code)
      else
         return
            responseText
            |> Serialization.deserialize<CommandApprovalProgressWithRule>
            |> Result.map Some
   }

let getCommandApprovals
   (orgId: OrgId)
   : Async<CommandApprovalProgressWithRuleMaybe>
   =
   async {
      let path = OrgPath.getCommandApprovals orgId

      let! (code, responseText) = Http.get path

      if code = 404 then
         return Ok None
      elif code <> 200 then
         return Error <| Err.InvalidStatusCodeError(serviceName, code)
      else
         return
            responseText
            |> Serialization.deserialize<CommandApprovalProgressWithRule list>
            |> Result.map (fun progress ->
               progress
               |> List.map (fun p -> p.CommandProgressId, p)
               |> Map.ofList
               |> Some)
   }
