namespace PartnerBank.Service.Domain

open System
open FsToolkit.ErrorHandling

open Bank.Org.Domain
open Bank.Account.Domain
open Lib.SharedTypes

module BusinessTypeDTO =
   let toEntity =
      function
      | "limited-partnership" -> Ok BusinessType.LimitedPartnership
      | "trust" -> Ok BusinessType.Trust
      | "sole-proprietorship" -> Ok BusinessType.SoleProprietorship
      | "corporation" -> Ok BusinessType.Corporation
      | "llc" -> Ok BusinessType.LLC
      | "general-partnership" -> Ok BusinessType.GeneralPartnership
      | "professional-association" -> Ok BusinessType.ProfessionalAssociation
      | "government" -> Ok BusinessType.Government
      | "non-profit" -> Ok BusinessType.NonProfit
      | "other" -> Ok BusinessType.Other
      | _ -> Error(Err.SerializationError "Invalid BusinessType")

   let fromEntity =
      function
      | BusinessType.LimitedPartnership -> "limited-partnership"
      | BusinessType.Trust -> "trust"
      | BusinessType.SoleProprietorship -> "sole-proprietorship"
      | BusinessType.Corporation -> "corporation"
      | BusinessType.LLC -> "llc"
      | BusinessType.GeneralPartnership -> "general-partnership"
      | BusinessType.ProfessionalAssociation -> "professional-association"
      | BusinessType.Government -> "government"
      | BusinessType.NonProfit -> "non-profit"
      | BusinessType.Other -> "other"

type BusinessDetailsDTO = {
   address: {|
      city: string
      country_code: string
      line_1: string
      line_2: string
      postal_code: string
      state: string
   |}
   business_name: string
   description: string
   ein: string
   legal_type: string
   registration_id: {|
      country_code: string
      number: string
   |}
   website: string
}

module BusinessDetailsDTO =
   let fromEntity (bd: BusinessDetails) = {
      address = {|
         city = bd.Address.City
         country_code = bd.Address.CountryCode
         line_1 = bd.Address.Line1
         line_2 = bd.Address.Line2
         postal_code = bd.Address.PostalCode
         state = bd.Address.State
      |}
      business_name = bd.BusinessName
      description = bd.Description
      ein = bd.EmployerIdentificationNumber
      legal_type = BusinessTypeDTO.fromEntity bd.LegalType
      registration_id = {|
         country_code = bd.Address.CountryCode
         number = bd.EmployerIdentificationNumber
      |}
      website =
         match bd.Website with
         | Some site -> site
         | None -> null
   }

   let toEntity (bd: BusinessDetailsDTO) =
      let legalType = BusinessTypeDTO.toEntity bd.legal_type

      legalType
      |> Result.map (fun legalType -> {
         Address = {
            City = bd.address.city
            CountryCode = bd.address.country_code
            Line1 = bd.address.line_1
            Line2 = bd.address.line_2
            PostalCode = bd.address.postal_code
            State = bd.address.state
         }
         BusinessName = bd.business_name
         Description = bd.description
         EmployerIdentificationNumber = bd.ein
         LegalType = legalType
         Website =
            if String.IsNullOrWhiteSpace bd.website then
               None
            else
               Some bd.website
      })

[<RequireQualifiedAccess>]
type PartnerBankBusinessVerificationStatus =
   | Unverified
   | Pending
   | ManualReview
   | Verified
   | Denied

module PartnerBankBusinessVerificationStatus =
   let fromString =
      function
      | "UNVERIFIED" -> Ok PartnerBankBusinessVerificationStatus.Unverified
      | "PENDING" -> Ok PartnerBankBusinessVerificationStatus.Pending
      | "MANUAL_REVIEW" -> Ok PartnerBankBusinessVerificationStatus.ManualReview
      | "VERIFIED" -> Ok PartnerBankBusinessVerificationStatus.Verified
      | "DENIED" -> Ok PartnerBankBusinessVerificationStatus.Denied
      | _ ->
         "Invalid PartnerBankBusinessVerificationStatus"
         |> Err.SerializationError
         |> Error

type LegalBusinessEntity = {
   Id: PartnerBankLegalEntityId
   BusinessDetails: BusinessDetails
   VerificationStatus: PartnerBankBusinessVerificationStatus
   ReviewReasons: string list
}

type LegalBusinessEntityDTO = {
   id: string
   business_details: BusinessDetailsDTO
   verification_status: string
   review_reasons: string list
}

module LegalBusinessEntityDTO =
   let fromEntity (o: LegalBusinessEntity) = {
      id = o.Id.ToString()
      business_details = BusinessDetailsDTO.fromEntity o.BusinessDetails
      verification_status = string o.VerificationStatus
      review_reasons = o.ReviewReasons
   }

   let toEntity (x: LegalBusinessEntityDTO) = result {
      let! verificationStatus =
         PartnerBankBusinessVerificationStatus.fromString x.verification_status

      let! businessDetails = BusinessDetailsDTO.toEntity x.business_details

      return {
         Id = PartnerBankLegalEntityId x.id
         BusinessDetails = businessDetails
         VerificationStatus = verificationStatus
         ReviewReasons = x.review_reasons
      }
   }

type LegalBusinessEntityCreateRequest = {
   Detail: BusinessDetails
   SagaMetadata: PartnerBankSagaMetadata
} with

   member x.AsDTO = BusinessDetailsDTO.fromEntity x.Detail

type LegalBusinessEntityCreateResponseDTO = LegalBusinessEntityDTO
type LegalBusinessEntityCreateResponse = LegalBusinessEntity
