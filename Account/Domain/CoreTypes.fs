namespace Bank.Account.Domain

open System
open Validus
open Validus.Operators

open Lib.SharedTypes

[<RequireQualifiedAccess>]
type ParentAccountStatus =
   | InitialEmptyState
   | Active
   | Closed
   /// Freezes the parent account, probably due to suspected fraud detected,
   /// disabling activity for all virtual accounts.
   | Frozen

   override x.ToString() =
      match x with
      | ParentAccountStatus.InitialEmptyState -> "InitialEmptyState"
      | ParentAccountStatus.Active -> "Active"
      | ParentAccountStatus.Frozen -> "Frozen"
      | ParentAccountStatus.Closed -> "Closed"

   static member fromString(status: string) : ParentAccountStatus option =
      if String.IsNullOrEmpty status then
         None
      else
         match status.ToLower() with
         | "active" -> Some ParentAccountStatus.Active
         | "closed" -> Some ParentAccountStatus.Closed
         | "frozen" -> Some ParentAccountStatus.Frozen
         | _ -> None

   static member fromStringUnsafe(status: string) : ParentAccountStatus =
      match ParentAccountStatus.fromString status with
      | None ->
         failwith "Error attempting to cast string to ParentAccountStatus"
      | Some status -> status

[<RequireQualifiedAccess>]
type AccountStatus =
   | InitialEmptyState
   | Active
   | Closed

   override x.ToString() =
      match x with
      | AccountStatus.InitialEmptyState -> "InitialEmptyState"
      | AccountStatus.Active -> "Active"
      | AccountStatus.Closed -> "Closed"

   static member fromString(status: string) : AccountStatus option =
      if String.IsNullOrEmpty status then
         None
      else
         match status.ToLower() with
         | "active" -> Some AccountStatus.Active
         | "closed" -> Some AccountStatus.Closed
         | _ -> None

   static member fromStringUnsafe(status: string) : AccountStatus =
      match AccountStatus.fromString status with
      | None -> failwith "Error attempting to cast string to AccountStatus"
      | Some status -> status

[<RequireQualifiedAccess>]
type AccountDepository =
   | Checking
   | Savings

   override x.ToString() =
      match x with
      | AccountDepository.Checking -> "Checking"
      | AccountDepository.Savings -> "Savings"

   static member fromString(dep: string) : AccountDepository option =
      if String.IsNullOrEmpty dep then
         None
      else
         match dep.ToLower() with
         | "checking" -> Some AccountDepository.Checking
         | "savings" -> Some AccountDepository.Savings
         | _ -> None

   static member fromStringUnsafe(dep: string) : AccountDepository =
      match AccountDepository.fromString dep with
      | None -> failwith "Error attempting to cast string to AccountDepository"
      | Some dep -> dep

type BillingPeriod = { Month: int; Year: int }

type PartnerBankRoutingNumber = PartnerBankRoutingNumber of RoutingNumber
type PartnerBankAccountNumber = PartnerBankAccountNumber of AccountNumber
type PartnerBankAccountId = PartnerBankAccountId of string
type PartnerBankLegalEntityId = PartnerBankLegalEntityId of string

type PartnerBankInternalAccountLink = {
   AccountNumber: PartnerBankAccountNumber
   RoutingNumber: PartnerBankRoutingNumber
   PartnerBankAccountId: PartnerBankAccountId
   PartnerBankLegalEntityId: PartnerBankLegalEntityId
}
