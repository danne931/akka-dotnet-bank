[<AutoOpen>]
module Money

open System

[<RequireQualifiedAccess>]
type Currency =
   | USD
   | EUR
   | THB
   | VND

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

type PendingFunds = {
   Count: int
   Money: decimal
} with

   static member Zero = { Count = 0; Money = 0m }

   member x.Add(amount: decimal) = {
      Count = x.Count + 1
      Money = x.Money + amount
   }

   member x.Remove(amount: decimal) = {
      Count = max (x.Count - 1) 0
      Money = max (x.Money - amount) 0m
   }
