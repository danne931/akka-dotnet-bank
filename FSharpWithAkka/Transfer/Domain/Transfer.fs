namespace Bank.Transfer.Domain

type RecipientAccountEnvironment =
   | Internal = 0
   | Domestic = 1
   | International = 2

type RecipientAccountIdentificationStrategy =
   | AccountId = 0
   | SwiftBIC = 1
   | IBAN = 2
   | NationalID = 3

type TransferRecipient = {
   LastName: string
   FirstName: string
   Identification: string
   AccountEnvironment: RecipientAccountEnvironment
   IdentificationStrategy: RecipientAccountIdentificationStrategy
   RoutingNumber: string option
   Currency: string
}
