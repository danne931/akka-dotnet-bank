module Form.Util

open Validus
open System

open Bank.Account.Domain
open Lib.SharedTypes
open Lib.Validators

/// Get either the account id to update as a form 'Values string input
/// or the first account with no auto-transfer rule configured.
let defaultTargetAccountId
   (accountIdToUpdate: AccountId option)
   (accounts: Map<AccountId, Account>)
   =
   accountIdToUpdate
   |> Option.orElse (
      accounts.Values
      |> Seq.tryFind _.AutoTransferRule.IsNone
      |> Option.map _.AccountId
   )
   |> Option.map string
   |> Option.defaultValue ""

let accountSelectOptions filter (accounts: Map<AccountId, Account>) =
   accounts
   |> Map.toList
   |> List.filter (snd >> filter)
   |> List.map (fun (acctId, profile) ->
      string acctId,
      $"{profile.Name} ({Money.format profile.AvailableBalance})")
   |> List.sortBy snd

let accountParser
   (accounts: Map<AccountId, Account>)
   (field: string)
   (idInput: string)
   : Result<Account, ValidationErrors>
   =
   Guid.parseOptional idInput
   |> Option.bind (AccountId >> accounts.TryFind)
   |> function
      | Some account -> Ok account
      | None ->
         Error(ValidationErrors.create field [ "has an invalid account id" ])

let positiveAmountParser
   (field: string)
   (amt: string)
   : Result<PositiveAmount, ValidationErrors>
   =
   amountValidatorFromString field amt
   |> Result.bind (
      PositiveAmount.create
      >> Result.mapError (fun _ ->
         ValidationErrors.create field [ "amount should be greater than 0" ])
   )

let rec trimLeadingZeros (input: string) : string =
   if input <> "0" && input.StartsWith "0" then
      trimLeadingZeros (input.Substring 1)
   else
      input

let trimNegativeSign (input: string) : string =
   if input.StartsWith "-" then input.Substring 1 else input

let formattedPositiveDecimal (input: string) : string option =
   let input = trimMoneySymbols input

   if input = "0" || String.IsNullOrWhiteSpace input then
      Some "0"
   else
      try
         let _ = decimal input
         input |> trimNegativeSign |> trimLeadingZeros |> Some
      with _ ->
         None

let formattedPercent (input: string) : string option =
   formattedPositiveDecimal input |> Option.map (fun str -> str + "%")

let formattedMoney (input: string) : string option =
   formattedPositiveDecimal input |> Option.map ((+) "$")
