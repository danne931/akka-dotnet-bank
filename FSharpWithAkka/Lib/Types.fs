module Lib.Types

open System
open System.Threading.Tasks


type BankEvent<'E> =
   {
      EntityId: Guid
      Timestamp: DateTime
      Data: 'E
   }

   member x.EventName = typedefof<'E>.Name

type Envelope = {
   EntityId: Guid
   Timestamp: DateTime
   EventName: string
}

type Validator<'a> = 'a -> Result<'a, string>
type AsyncValidator<'a> = 'a -> Task<Result<'a, string>>
