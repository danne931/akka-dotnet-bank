namespace Bank.Account.Domain

open System

type private Currencies =
   | USD = 0
   | EUR = 1
   | THB = 2
   | VND = 3

module Validators =
   let accountCreate () =
      (fun (cmd: CreateAccountCommand) ->
         // TODO: handle varying currency codes
         if cmd.Balance < 100 then
            Error $"InvalidStartBalance {cmd.Balance}"
         elif not (Enum.IsDefined(typeof<Currencies>, cmd.Currency.Value)) then
            Error $"Invalid currency {cmd.Currency}"
         else
            Ok cmd)

   let DailyDebitLimitValidation () =
      (fun (cmd: LimitDailyDebitsCommand) ->
         if cmd.DebitLimit < 0 then
            Error "InvalidDailyDebitLimit"
         else
            Ok cmd)
