[<RequireQualifiedAccess>]
module Org

open Validus

open Bank.Org.Domain
open Lib.SharedTypes

let applyEvent (state: OrgWithEvents) (evt: OrgEvent) =
   let org = state.Info

   let updatedOrg =
      match evt with
      | OrgCreated e -> {
         OrgId = e.OrgId
         Name = e.Data.Name
         Status = OrgStatus.PendingOnboardingTasksFulfilled
         FeatureFlags = {
            SocialTransferDiscoveryPrimaryAccountId = None
         }
        }
      | OrgOnboardingFinished _ -> { org with Status = OrgStatus.Active }
      | FeatureFlagConfigured e -> {
         org with
            FeatureFlags = e.Data.Config
        }

   {
      state with
         Info = updatedOrg
         Events = evt :: state.Events
   }

module private StateTransition =
   let transitionErr (err: OrgStateTransitionError) =
      Error <| OrgStateTransitionError err

   let map
      (eventTransform: BankEvent<'t> -> OrgEvent)
      (state: OrgWithEvents)
      (eventValidation: ValidationResult<BankEvent<'t>>)
      =
      eventValidation
      |> Result.mapError ValidationError
      |> Result.map (fun evt ->
         let evt = eventTransform evt
         (evt, applyEvent state evt))

   let create (state: OrgWithEvents) (cmd: CreateOrgCommand) =
      if state.Info.Status <> OrgStatus.InitialEmptyState then
         transitionErr OrgNotReadyToStartOnboarding
      else
         map OrgCreated state (CreateOrgCommand.toEvent cmd)

   let finalizeOnboarding
      (state: OrgWithEvents)
      (cmd: FinalizeOrgOnboardingCommand)
      =
      if state.Info.Status <> OrgStatus.PendingOnboardingTasksFulfilled then
         transitionErr OrgNotReadyToActivate
      else
         map
            OrgOnboardingFinished
            state
            (FinalizeOrgOnboardingCommand.toEvent cmd)

   let configureFeatureFlag
      (state: OrgWithEvents)
      (cmd: ConfigureFeatureFlagCommand)
      =
      if state.Info.Status <> OrgStatus.Active then
         transitionErr OrgNotActive
      else
         map
            FeatureFlagConfigured
            state
            (ConfigureFeatureFlagCommand.toEvent cmd)

let stateTransition (state: OrgWithEvents) (command: OrgCommand) =
   match command with
   | CreateOrg cmd -> StateTransition.create state cmd
   | FinalizeOrgOnboarding cmd -> StateTransition.finalizeOnboarding state cmd
   | ConfigureFeatureFlag cmd -> StateTransition.configureFeatureFlag state cmd

let empty: Org = {
   OrgId = OrgId System.Guid.Empty
   Name = ""
   Status = OrgStatus.InitialEmptyState
   FeatureFlags = {
      SocialTransferDiscoveryPrimaryAccountId = None
   }
}
