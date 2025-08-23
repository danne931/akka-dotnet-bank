module CachedOrgSettings

open Lib.SharedTypes

type CachedOrgSettings = {
   AdminTeamEmail: Email.Email
   ParentAccountId: ParentAccountId
   PartnerBankInternalAccountLink:
      Bank.Account.Domain.PartnerBankInternalAccountLink
}

/// CRDT interface for accessing org settings across the cluster.
type OrgSettingsCache = {
   Get: OrgId -> Async<Result<CachedOrgSettings option, Err>>
   Update: OrgId -> CachedOrgSettings -> Async<Result<unit, Err>>
}
