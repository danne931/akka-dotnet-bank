module CardIssuerMappingSqlMapper

open Lib.SharedTypes
open Bank.Employee.Domain
open CardIssuer.Service.Domain

let table = "card_issuer_mapping"

module TypeCast =
   let issuerName = "card_issuer_name"
   let closedReason = "card_issuer_closed_card_reason"

module Fields =
   let id = "id"
   let internalCardId = "internal_card_id"
   let issuerCardId = "issuer_card_id"
   let issuerName = "issuer_name"
   let closedReason = "closed_reason"

   let employeeId = "employee_id"

module Reader =
   let id (read: RowReader) = read.int Fields.id

   let internalCardId (read: RowReader) =
      read.uuid Fields.internalCardId |> CardId

   let issuerCardId (read: RowReader) =
      read.uuid Fields.issuerCardId |> CardIssuerCardId

   let issuerName (read: RowReader) =
      read.string Fields.issuerName |> CardIssuerName.fromStringUnsafe

   let closedReason (read: RowReader) =
      read.textOrNone Fields.closedReason
      |> Option.map Serialization.deserializeUnsafe<CardClosedReason>

   let employeeId (read: RowReader) =
      read.uuid Fields.employeeId |> EmployeeId

module Writer =
   let id (id: int) = Sql.int id
   let internalCardId (CardId id) = Sql.uuid id
   let issuerCardId (CardIssuerCardId id) = Sql.uuid id
   let issuerName (name: CardIssuerName) = Sql.string (string name)

   let closedReason (reason: CardClosedReason option) =
      reason |> Option.map string |> Sql.stringOrNone

   let closedReasonDetail (reason: CardClosedReason option) =
      reason |> Option.map Serialization.serialize |> Sql.jsonbOrNone

   let employeeId (EmployeeId id) = Sql.uuid id
