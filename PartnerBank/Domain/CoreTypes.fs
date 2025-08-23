namespace PartnerBank.Service.Domain

open Lib.SharedTypes

type PartnerBankSagaMetadata = {
   OrgId: OrgId
   CorrelationId: CorrelationId
}

type PartnerBankLegalEntityId =
   | PartnerBankLegalEntityId of string

   override x.ToString() = string x.Value

   member x.Value = let (PartnerBankLegalEntityId id) = x in id
