module Bank.Account.Forms.InvoiceForm

open Feliz
open Fable.Form.Simple
open Fable.Form.Simple.Pico
open System

open Bank.Payment.Domain
open Lib.Validators
open Lib.SharedTypes

type LineItemValues = {
   Name: string
   Quantity: string
   UnitPrice: string
}

type InvoiceValues = {
   LineItems: LineItemValues list
   TaxPercent: string
   SubTotal: string
   Total: string
}

let private moneyParser fieldName =
   amountValidatorFromString fieldName >> validationErrorsHumanFriendly

let private taxParser = parseDecimal "Tax" >> validationErrorsHumanFriendly

let private quantityParser =
   parseInt "Quantity" >> validationErrorsHumanFriendly

let private subtotalFromFormValues (values: InvoiceValues) =
   values.LineItems
   |> List.choose (fun a ->
      let qty = quantityParser a.Quantity |> Result.toOption
      let unitPrice = moneyParser "Unit price" a.UnitPrice |> Result.toOption

      Option.map2 (fun qty price -> qty, price) qty unitPrice)
   |> List.sumBy (fun (qty, price) -> decimal qty * price)

let private totalFromFormValues (subTotal: decimal) (tax: decimal) =
   if tax = 0m then
      subTotal
   else
      subTotal + subTotal * (tax / 100m)

let parsedInvoiceFromInvoiceValues (values: InvoiceValues) =
   let subtotal = subtotalFromFormValues values
   let tax = taxParser values.TaxPercent |> Result.defaultValue 0m
   let total = totalFromFormValues subtotal tax

   {|
      SubTotal = subtotal
      Tax = tax
      Total = total
   |}

let private lineItemForm
   (index: int)
   : Form.Form<LineItemValues, InvoiceLineItem, IReactProperty>
   =
   let nameField =
      Form.textField {
         Parser = Ok
         Value = _.Name
         Update = fun newValue values -> { values with Name = newValue }
         Error = fun _ -> None
         Attributes = {
            Label = "Item Name"
            Placeholder = "Name"
            HtmlAttributes = []
         }
      }

   let quantityField =
      Form.textField {
         Parser = quantityParser
         Value = _.Quantity
         Update = fun newValue values -> { values with Quantity = newValue }
         Error = fun _ -> None
         Attributes = {
            Label = "Quantity"
            Placeholder = "Quantity"
            HtmlAttributes = []
         }
      }

   let unitPriceField =
      Form.textField {
         Parser = moneyParser "Unit Price"
         Value =
            fun values ->
               Form.Util.formattedMoney values.UnitPrice
               |> Option.defaultValue values.UnitPrice
         Update = fun newValue values -> { values with UnitPrice = newValue }
         Error = fun _ -> None
         Attributes = {
            Label = "Unit Price"
            Placeholder = "Cost per unit"
            HtmlAttributes = []
         }
      }

   Form.succeed (fun name qty unitPrice ->
      {
         Name = name
         Quantity = qty
         UnitPrice = unitPrice
      }
      : InvoiceLineItem)
   |> Form.append nameField
   |> Form.append quantityField
   |> Form.append unitPriceField

let private lineItemsForm
   : Form.Form<InvoiceValues, InvoiceLineItem list, IReactProperty> =
   Form.list
      {
         Value = _.LineItems
         Update = fun newItems values -> { values with LineItems = newItems }
         Default = {
            Name = ""
            Quantity = "1"
            UnitPrice = "0"
         }
         Attributes = {
            Label = ""
            Add = Some "Add Item"
            Delete = Some ""
         }
      }
      lineItemForm

let invoiceForm: Form.Form<InvoiceValues, Invoice, IReactProperty> =
   let taxField =
      Form.textField {
         Parser = taxParser
         Value =
            fun values ->
               Form.Util.formattedPercent values.TaxPercent
               |> Option.defaultValue values.TaxPercent
         Update = fun newValue values -> { values with TaxPercent = newValue }
         Error = fun _ -> None
         Attributes = {
            Label = "Tax:"
            Placeholder = "Tax Percent"
            HtmlAttributes = []
         }
      }

   let subTotalField (value: decimal) =
      Form.textField {
         Parser = moneyParser "Subtotal"
         Value = fun _ -> Money.format value
         Update = fun _ values -> values
         Error = fun _ -> None
         Attributes = {
            Label = "Subtotal:"
            Placeholder = ""
            HtmlAttributes = []
         }
      }
      |> Form.disable

   let totalField (value: decimal) =
      Form.textField {
         Parser = moneyParser "Total"
         Value = fun _ -> Money.format value
         Update = fun _ values -> values
         Error = fun _ -> None
         Attributes = {
            Label = "Total:"
            Placeholder = ""
            HtmlAttributes = []
         }
      }
      |> Form.disable

   Form.succeed (fun lineItems tax -> {
      Id = InvoiceId(Guid.NewGuid())
      LineItems = lineItems
      TaxPercent = tax
   })
   |> Form.append lineItemsForm
   |> Form.append (
      Form.meta (fun values ->
         let parsed = parsedInvoiceFromInvoiceValues values

         Form.succeed (fun tax _ _ -> tax)
         |> Form.append taxField
         |> Form.append (subTotalField parsed.SubTotal)
         |> Form.append (totalField parsed.Total)
         |> Form.group)
   )

let defaultInvoiceValues = {
   LineItems = [
      {
         Name = ""
         Quantity = ""
         UnitPrice = ""
      }
   ]
   TaxPercent = "0"
   SubTotal = "0"
   Total = "0"
}
