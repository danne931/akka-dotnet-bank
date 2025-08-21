namespace PartnerBank.Service.Domain

open Lib.SharedTypes

type PartnerBankSagaMetadata = {
   OrgId: OrgId
   CorrelationId: CorrelationId
}
