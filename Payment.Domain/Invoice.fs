namespace Bank.Payment.Domain

open System

type InvoiceId =
   | InvoiceId of Guid

   member x.Value = let (InvoiceId id) = x in id

type InvoiceLineItem = {
   Name: string
   Quantity: int
   UnitPrice: decimal
}

type Invoice = {
   Id: InvoiceId
   //UserFriendlyId: int
   LineItems: InvoiceLineItem list
   TaxPercent: decimal
} with

   member x.SubTotal =
      x.LineItems
      |> List.sumBy (fun item -> decimal item.Quantity * item.UnitPrice)

   member x.TaxAsMoney =
      if x.TaxPercent = 0m then
         0m
      else
         x.SubTotal * (x.TaxPercent / 100m)

   member x.Total = x.SubTotal + x.TaxAsMoney
