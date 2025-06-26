module Bank.Employee.Forms.AccountProfileForm

open Fable.Form.Simple
open System

open Bank.Account.Domain
open Lib.SharedTypes

type Values = { AccountId: string }

let accountSelect
   (label: string option)
   (accounts: Map<AccountId, Account>)
   : Form.Form<Values, AccountId, _>
   =
   Form.selectField {
      Parser = Guid.Parse >> AccountId >> Ok
      Value = fun values -> values.AccountId
      Update = fun newValue values -> { values with AccountId = newValue }
      Error = fun _ -> None
      Attributes = {
         Label = label |> Option.defaultValue "Select an account:"
         Placeholder = "No account selected"
         Options =
            [
               for a in accounts.Values ->
                  string a.AccountId,
                  $"{a.Name} ({Money.format a.AvailableBalance})"
            ]
            |> List.sortBy snd
      }
   }
