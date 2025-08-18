namespace Bank.Employee.Domain

open System
open Validus

open Lib.SharedTypes

type ThirdPartyProviderCardId =
   | ThirdPartyProviderCardId of Guid

   override x.ToString() = string x.Value

   member x.Value = let (ThirdPartyProviderCardId id) = x in id

[<RequireQualifiedAccess>]
type CardType =
   | Credit
   | Debit

   static member fromString(status: string) : CardType option =
      if String.IsNullOrEmpty status then
         None
      else
         match status.ToLower() with
         | "credit" -> Some Credit
         | "debit" -> Some Debit
         | _ -> None

   static member fromStringUnsafe(cardType: string) : CardType =
      match CardType.fromString cardType with
      | None -> failwith "Error attempting to cast string to CardType"
      | Some status -> status

[<RequireQualifiedAccess>]
type CardFrozenReason =
   | UserRequested
   | SuspectedFraud

[<RequireQualifiedAccess>]
type CardStatus =
   | Pending
   | Active
   | Frozen of CardFrozenReason
   | Closed

   override x.ToString() =
      match x with
      | Pending -> "Pending"
      | Active -> "Active"
      | Frozen _ -> "Frozen"
      | Closed -> "Closed"

type CardExpiration = {
   Month: int
   Year: int
} with

   static member create() : CardExpiration =
      let exp = DateTime.Now.AddYears 3
      { Month = exp.Month; Year = exp.Year }

type Card = {
   CardType: CardType
   CardNumberLast4: string
   DailyPurchaseLimit: decimal
   MonthlyPurchaseLimit: decimal
   Virtual: bool // virtual vs. physical card
   Status: CardStatus
   CardNickname: string option
   LastPurchaseAt: DateTime option
   Expiration: CardExpiration
   CardId: CardId
   AccountId: AccountId
   ThirdPartyProviderCardId: ThirdPartyProviderCardId option
} with

   member x.IsExpired() =
      DateTime(x.Expiration.Year, x.Expiration.Month, 1) <= DateTime.UtcNow

   member x.IsPending =
      match x.Status with
      | CardStatus.Pending -> true
      | _ -> false

   member x.IsFrozen =
      match x.Status with
      | CardStatus.Frozen reason -> Some reason
      | _ -> None

   member x.Display =
      $"""
      {x.CardNickname |> Option.defaultValue ""}
      **{x.CardNumberLast4}
      """

   static member dailyPurchaseLimitValidator =
      Check.Decimal.between 0m Constants.DAILY_PURCHASE_LIMIT_DEFAULT

   static member monthlyPurchaseLimitValidator =
      Check.Decimal.between 0m Constants.MONTHLY_PURCHASE_LIMIT_DEFAULT
