module Lib.Validators

open System
open Validus
open Validus.Operators

/// Trim money symbols such as ($, %) typically present in forms.
let trimMoneySymbols (input: string) = input.Replace("%", "").Replace("$", "")

let parseGuid: Validator<string, Guid> =
   fun field input ->
      try
         let guid = Guid.Parse input
         Check.Guid.notEmpty field guid
      with _ ->
         Error <| ValidationErrors.create field [ $"Invalid {field}" ]

let parseInt: Validator<string, int> =
   fun field input ->
      try
         int32 input |> Ok
      with _ ->
         Error <| ValidationErrors.create field [ $"Invalid {field}" ]

let parseInt64: Validator<string, int64> =
   fun field input ->
      try
         int64 input |> Ok
      with _ ->
         Error <| ValidationErrors.create field [ $"Invalid {field}" ]

let parseDecimal: Validator<string, decimal> =
   fun field input ->
      try
         input |> trimMoneySymbols |> decimal |> Ok
      with _ ->
         Error <| ValidationErrors.create field [ $"Invalid {field}" ]

let parseDate: Validator<string, DateTime> =
   fun field input ->
      match Lib.Time.DateTime.parseOptional input with
      | None -> Error <| ValidationErrors.create field [ $"Invalid {field}" ]
      | Some date -> Ok date

let amountValidator = Check.Decimal.greaterThan 0m

let amountValidatorFromString = parseDecimal >=> amountValidator

let accountNameValidator = Check.String.betweenLen 2 50 "Account name"
let firstNameValidator = Check.String.betweenLen 2 50 "First name"
let lastNameValidator = Check.String.betweenLen 2 50 "Last name"

let dateNotDefaultValidator propName =
   let msg = sprintf "%s should not be missing"
   Check.WithMessage.DateTime.notEquals DateTime.MinValue msg propName

let dateNotDefaultValidatorFromString = parseDate >=> dateNotDefaultValidator

let dateInFutureValidator propName (date: DateTime) =
   let msg = sprintf "%s should be in the future"

   Check.WithMessage.DateTime.greaterThan
      (DateTime.Today.ToUniversalTime())
      msg
      propName
      (date.ToUniversalTime())

let dateInFutureValidatorFromString = parseDate >=> dateInFutureValidator

let datePresentOrFutureValidator propName (date: DateTime) =
   let msg = sprintf "%s should be today or the future"

   Check.WithMessage.DateTime.greaterThanOrEqualTo
      (DateTime.Today.ToUniversalTime())
      msg
      propName
      (date.ToUniversalTime())

let datePresentOrFutureValidatorFromString =
   parseDate >=> datePresentOrFutureValidator

let transferRecipientIdValidator senderId =
   let msg = sprintf "%s should not equal sender id"
   Check.WithMessage.String.notEquals senderId msg "Recipient Id"

let merchantValidator = Check.String.greaterThanLen 2 "Merchant"
