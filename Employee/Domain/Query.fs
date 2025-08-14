namespace Bank.Employee.Domain

open System

open Lib.SharedTypes
open Lib.NetworkQuery

[<RequireQualifiedAccess>]
type EmployeeEventGroupFilter =
   | Invitation
   | CreatedCard
   | Purchase
   | PurchaseLimitUpdated
   | CardFrozenUnfrozen
   | UpdatedRole
   | AccessRestored

   member x.Display =
      match x with
      | EmployeeEventGroupFilter.Invitation -> "Invitations"
      | EmployeeEventGroupFilter.CreatedCard -> "Cards Issued"
      | EmployeeEventGroupFilter.Purchase -> "Purchases"
      | EmployeeEventGroupFilter.PurchaseLimitUpdated ->
         "Purchase Limit Applied"
      | EmployeeEventGroupFilter.CardFrozenUnfrozen -> "Card Frozen/Unfrozen"
      | EmployeeEventGroupFilter.UpdatedRole -> "Employee Role Altered"
      | EmployeeEventGroupFilter.AccessRestored -> "Employee Access Restored"

   static member All = [
      EmployeeEventGroupFilter.Invitation
      EmployeeEventGroupFilter.Purchase
      EmployeeEventGroupFilter.CreatedCard
      EmployeeEventGroupFilter.UpdatedRole
      EmployeeEventGroupFilter.CardFrozenUnfrozen
      EmployeeEventGroupFilter.PurchaseLimitUpdated
      EmployeeEventGroupFilter.AccessRestored
   ]

module EmployeeEventGroupFilter =
   let fromString =
      function
      | "Invitation" -> Some EmployeeEventGroupFilter.Invitation
      | "CreatedCard" -> Some EmployeeEventGroupFilter.CreatedCard
      | "Purchase" -> Some EmployeeEventGroupFilter.Purchase
      | "PurchaseLimitUpdated" ->
         Some EmployeeEventGroupFilter.PurchaseLimitUpdated
      | "CardFrozenUnfrozen" -> Some EmployeeEventGroupFilter.CardFrozenUnfrozen
      | "UpdatedRole" -> Some EmployeeEventGroupFilter.UpdatedRole
      | "AccessRestored" -> Some EmployeeEventGroupFilter.AccessRestored
      | _ -> None

   let fromQueryString: string -> EmployeeEventGroupFilter list option =
      listFromQueryString fromString

   let listToDisplay (items: EmployeeEventGroupFilter list) =
      List.fold
         (fun acc (filter: EmployeeEventGroupFilter) ->
            if acc = "" then
               filter.Display
            else
               $"{acc}, {filter.Display}")
         ""
         items

type EmployeeQuery = {
   EmployeeIds: (EmployeeId list) option
   Roles: (Role list) option
   Status: EmployeeStatus option
}

module EmployeeQuery =
   let rolesFromQueryString = listFromQueryString Role.fromString

   let rolesToDisplay =
      List.fold
         (fun acc (filter: Role) ->
            if acc = "" then
               filter.Display
            else
               $"{acc}, {filter.Display}")
         ""

   let employeeIdsFromQueryString: string -> EmployeeId list option =
      listFromQueryString (Guid.parseOptional >> Option.map EmployeeId)

type CardQuery = {
   AccountIds: (AccountId list) option
   EmployeeIds: (EmployeeId list) option
   CreatedAtDateRange: (DateTime * DateTime) option
   Amount: AmountFilter option
}

module CardQuery =
   let employeeIdsFromQueryString = EmployeeQuery.employeeIdsFromQueryString

   let accountIdsFromQueryString =
      listFromQueryString (Guid.parseOptional >> Option.map AccountId)
