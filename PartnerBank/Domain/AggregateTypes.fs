namespace PartnerBank.Service.Domain

[<RequireQualifiedAccess>]
type PartnerBankServiceMessage =
   | CreateLegalEntity of LegalBusinessEntityCreateRequest
   | CreateInternalAccount of InternalAccountCreateRequest
   | TransferDomestic of PartnerBankDomesticTransferRequest
   | TransferBetweenOrganizations of PartnerBankSyncTransferBetweenOrgs
   | Purchase of SyncPurchase

   member x.SagaMetadata =
      match x with
      | CreateLegalEntity req -> req.SagaMetadata
      | CreateInternalAccount req -> req.SagaMetadata
      | TransferBetweenOrganizations req -> req.SagaMetadata
      | TransferDomestic req -> req.SagaMetadata
      | Purchase req -> req.SagaMetadata

[<RequireQualifiedAccess>]
type PartnerBankResponse =
   | CreateLegalEntity of LegalBusinessEntityCreateResponse
   | CreateInternalAccount of InternalAccountCreateResponse
   | TransferBetweenOrganizations of PartnerBankSyncTransferBetweenOrgsResponse
   | TransferDomestic of PartnerBankDomesticTransferResponse
   | Purchase of SyncPurchaseResponse
