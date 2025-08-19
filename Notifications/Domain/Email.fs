module Email

open System
open Validus

type Email = private {
   Email: string
} with

   override x.ToString() = x.Email

#if FABLE_COMPILER
   static member private emailRegex =
      Fable.Core.JS.Constructors.RegExp.Create @"^[^@\s]+@[^@\s]+\.[^@\s]+$"
#endif

   static member ofString: Validator<string, Email> =
      fun field input ->
         let rule (x: string) =
            if String.IsNullOrEmpty x then
               false
            else
               let email = x.Trim()

               if email.Length > 255 then
                  false
               else
#if FABLE_COMPILER
                  Email.emailRegex.IsMatch email
#else
                  try
                     (System.Net.Mail.MailAddress email).Address = email
                  with :? FormatException ->
                     false
#endif

         let message = sprintf "%s must be a valid email address"

         input
         |> Validator.create message rule field
         |> Result.map (fun v -> { Email = v })

   static member deserialize(email: string) : Email = { Email = email }

   static member empty = { Email = "" }
