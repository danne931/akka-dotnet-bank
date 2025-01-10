namespace Bank.Org.Domain

open Lib.SharedTypes
open Bank.Account.Domain

type OrgCommand =
   | CreateOrg of CreateOrgCommand
   | FinalizeOrgOnboarding of FinalizeOrgOnboardingCommand
   | ConfigureFeatureFlag of ConfigureFeatureFlagCommand

type OrgEvent =
   | OrgCreated of BankEvent<OrgCreated>
   | OrgOnboardingFinished of BankEvent<OrgOnboardingFinished>
   | FeatureFlagConfigured of BankEvent<FeatureFlagConfigured>

type OpenEventEnvelope = OrgEvent * Envelope

[<RequireQualifiedAccess>]
module OrgEnvelope =
   let get (evt: BankEvent<'E>) : Envelope = {
      Id = evt.Id
      EntityId = evt.EntityId
      OrgId = evt.OrgId
      CorrelationId = evt.CorrelationId
      InitiatedById = evt.InitiatedById
      Timestamp = evt.Timestamp
      EventName = evt.EventName
   }

   let wrap (o: BankEvent<_>) : OrgEvent =
      match box o with
      | :? BankEvent<OrgCreated> as evt -> OrgCreated evt
      | :? BankEvent<OrgOnboardingFinished> as evt -> OrgOnboardingFinished evt
      | :? BankEvent<FeatureFlagConfigured> as evt -> FeatureFlagConfigured evt
      | _ -> failwith "Missing definition for OrgEvent message"

   let unwrap (o: OrgEvent) : OpenEventEnvelope =
      match o with
      | OrgCreated evt -> wrap evt, get evt
      | OrgOnboardingFinished evt -> wrap evt, get evt
      | FeatureFlagConfigured evt -> wrap evt, get evt

type Org = {
   OrgId: OrgId
   Name: string
   Status: OrgStatus
   FeatureFlags: FeatureFlagOrgSettings
}

type OrgWithEvents = { Info: Org; Events: OrgEvent list }

type OrgWithAccountProfiles = {
   Org: Org
   AccountProfiles: Map<AccountId, AccountProfile>
   Balance: decimal
} with

   member x.Accounts: Map<AccountId, Account> =
      x.AccountProfiles |> Map.map (fun _ profile -> profile.Account)

type OrgMessage =
   | GetOrg
   | StateChange of OrgCommand
   | Event of OrgEvent
