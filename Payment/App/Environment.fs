[<RequireQualifiedAccess>]
module EnvPayment

open FsConfig

let builder = Env.builder

type private Input = { MockInvoiceProcessing: bool option }

type InvoiceProcessingConfig = { MockInvoiceProcessing: bool }

let config =
   match AppConfig(builder.Configuration).Get<Input>() with
   | Ok input -> {
      MockInvoiceProcessing =
         input.MockInvoiceProcessing |> Option.defaultValue false
     }
   | Error err ->
      match err with
      | NotFound key -> invalidArg key "Not found"
      | BadValue(key, value) -> invalidArg key $"{value} is invalid type for"
      | NotSupported key -> invalidArg key "Not supported"
