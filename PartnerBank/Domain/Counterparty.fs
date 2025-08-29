namespace PartnerBank.Service.Domain

open Lib.SharedTypes
open Bank.Transfer.Domain

type PartnerBankCounterpartyRequest = {
   Name: string
   AccountNumber: AccountNumber
   RoutingNumber: RoutingNumber
   Depository: CounterpartyAccountDepository
   Address: Address
} with

   member x.AsDTO = {|
      action = "CreateCounterparty"
      data = {|
         name = x.Name
         account_number = string x.AccountNumber
         routing_number = string x.RoutingNumber
         depository =
            match x.Depository with
            | CounterpartyAccountDepository.Checking -> "checking"
            | CounterpartyAccountDepository.Savings -> "savings"
         address = AddressDTO.fromAddress x.Address
      |}
   |}

type PartnerBankCreateCounterpartyResponse = {
   PartnerBankCounterpartyId: PartnerBankCounterpartyId
}

type PartnerBankCreateCounterpartyResponseDTO = {
   id: string
} with

   member x.AsEntity = {
      PartnerBankCounterpartyId = PartnerBankCounterpartyId x.id
   }
