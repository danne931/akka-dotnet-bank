[<AutoOpen>]
module Money

open System

[<RequireQualifiedAccess>]
type Currency =
   | USD
   | EUR
   | THB
   | VND

   static member create(currency: string) : Result<Currency, string> =
      if String.IsNullOrEmpty currency then
         Error "Empty currency"
      else
         match currency.ToUpper() with
         | "USD" -> Ok USD
         | "EUR" -> Ok EUR
         | "THB" -> Ok THB
         | "VND" -> Ok VND
         | _ -> Error "Unsupported currency"

type PositiveAmount =
   private
   | PositiveAmount of decimal

   member x.Value = let (PositiveAmount v) = x in v

   static member create(value: decimal) =
      if value > 0m then
         Ok(PositiveAmount value)
      else
         Error "Non-Positive Amount"

   /// Have yet to refactor the codebase to use PositiveAmount more often.
   /// When a PositiveAmount interacts with a decimal we should
   /// use this tryMap to ensure the result of the transform is still a valid
   /// PositiveAmount.
   static member tryMap
      (transform: decimal -> decimal)
      (PositiveAmount amount)
      =
      PositiveAmount.create (transform amount)

   static member (-)(PositiveAmount amt1, PositiveAmount amt2) =
      PositiveAmount.create (amt1 - amt2)

   static member (+)(PositiveAmount amt1, PositiveAmount amt2) =
      PositiveAmount(amt1 + amt2)

   static member (/)(PositiveAmount amt1, PositiveAmount amt2) =
      PositiveAmount(amt1 / amt2)

   static member (*)(PositiveAmount amt1, PositiveAmount amt2) =
      PositiveAmount(amt1 * amt2)

[<RequireQualifiedAccess>]
type MoneyFlow =
   | In
   | Out

module MoneyFlow =
   let fromString (flow: string) : MoneyFlow option =
      if String.IsNullOrEmpty flow then
         None
      else
         match flow.ToLower() with
         | "in" -> Some MoneyFlow.In
         | "out" -> Some MoneyFlow.Out
         | _ -> None

// TODO:
// - Use instead of decimal in several places throughout the app
// - Add fromCents and toCents functions
type Money = {
   Amount: decimal
   Flow: MoneyFlow
} with

   static member create(amt: decimal) = {
      Flow = if amt < 0m then MoneyFlow.Out else MoneyFlow.In
      Amount = abs amt
   }

   static member amountSigned(m: Money) =
      match m.Flow with
      | MoneyFlow.Out -> -m.Amount
      | MoneyFlow.In -> m.Amount

   static member (+)(m: Money, m2: Money) =
      let sum = (Money.amountSigned m) + (Money.amountSigned m2)
      Money.create sum

type PendingFund = { Amount: decimal; Flow: MoneyFlow }

type PendingFunds = Map<Guid, PendingFund>

type PendingFundsTally = { In: decimal; Out: decimal }

module PendingFunds =
   let zero: PendingFunds = Map.empty

   let add (txnId: Guid) (fund: PendingFund) (funds: PendingFunds) =
      Map.add txnId fund funds

   let remove (txnId: Guid) (funds: PendingFunds) = Map.remove txnId funds

   let updateTransactionAmount
      (txnId: Guid)
      (fund: PendingFund)
      (funds: PendingFunds)
      =
      Map.change txnId (Option.map (fun _ -> fund)) funds

   let amount (funds: PendingFunds) =
      Seq.fold
         (fun acc fund ->
            match fund.Flow with
            | MoneyFlow.In -> { acc with In = acc.In + fund.Amount }
            | MoneyFlow.Out -> { acc with Out = acc.Out + fund.Amount })
         { In = 0m; Out = 0m }
         funds.Values
