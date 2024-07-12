module Bank.Employee.Forms.AccountProfileForm

open Fable.Form.Simple
open System

open Bank.Account.Domain
open Lib.SharedTypes

type Values = { LinkedAccountId: string }

let accountProfileSelect (accountProfiles: Map<AccountId, AccountProfile>) =
   Form.selectField {
      Parser = Guid.Parse >> AccountId >> Ok
      Value = fun values -> values.LinkedAccountId
      Update =
         fun newValue values -> {
            values with
               LinkedAccountId = newValue
         }
      Error = fun _ -> None
      Attributes = {
         Label = "Select an account to link the card to:"
         Placeholder = "No account selected"
         Options = [
            for profile in accountProfiles.Values ->
               string profile.AccountId, profile.Name
         ]
      }
   }
