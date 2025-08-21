namespace PartnerBank.Service.Domain

[<RequireQualifiedAccess>]
type PartnerBankServiceMessage =
   | LinkAccount of PartnerBankAccountLinking
   | TransferDomestic of PartnerBankDomesticTransfer
   | TransferBetweenOrganizations of PartnerBankSyncTransferBetweenOrgs
   | Purchase of PartnerBankSyncPurchase

   member x.SagaMetadata =
      match x with
      | LinkAccount req -> req.SagaMetadata
      | TransferBetweenOrganizations req -> req.SagaMetadata
      | TransferDomestic req -> req.SagaMetadata
      | Purchase req -> req.SagaMetadata

[<RequireQualifiedAccess>]
type PartnerBankResponse =
   | LinkAccount of AccountLinkResponse
   | TransferBetweenOrganizations of PartnerBankSyncTransferBetweenOrgsResponse
   | TransferDomestic of PartnerBankDomesticTransferResponse
   | Purchase of PartnerBankSyncPurchaseResponse
