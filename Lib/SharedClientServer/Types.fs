module Lib.SharedTypes

open System
open System.Collections
open System.Collections.Generic
open Validus
open Validus.Operators

type Result<'T, 'Error> with
   /// Extension to flatten a Result's inner Option,
   /// converting the None case to an Error.
   static member unwrapOption
      (error: 'Error)
      (result: Result<Option<'T>, 'Error>)
      =
      match result with
      | Ok(Some value) -> Ok value
      | Ok None -> Error error
      | Error e -> Error e

type NonEmptyList<'T> = private {
   Head: 'T
   Tail: 'T list
} with

   member x.Length = 1 + List.length x.Tail

   interface IEnumerable<'T> with
      member x.GetEnumerator() : IEnumerator<'T> =
         seq {
            yield x.Head
            yield! x.Tail
         }
         |> _.GetEnumerator()

      member this.GetEnumerator() : IEnumerator =
         seq {
            yield this.Head
            yield! this.Tail
         }
         |> _.GetEnumerator()

[<RequireQualifiedAccess>]
module NonEmptyList =
   let create head tail = { Head = head; Tail = tail }

   let singleton head = { Head = head; Tail = [] }

   let head { Head = head } = head

   let tail { Tail = tail } = tail

   let cons x { Head = head; Tail = tail } = { Head = x; Tail = head :: tail }

   let append { Head = head; Tail = tail } { Head = head2; Tail = tail2 } =
      (head :: tail) @ (head2 :: tail2)

   let toList { Head = head; Tail = tail } = head :: tail

   let fromList =
      function
      | [] -> Error "EmptyList"
      | [ head: 'T ] -> Ok(singleton head)
      | head :: tail -> Ok(create head tail)

   let map transform { Head = head; Tail = tail } = {
      Head = transform head
      Tail = List.map transform tail
   }

   let fold transform state { Head = head; Tail = tail } =
      List.fold transform (transform state head) tail

   let sortBy fn { Head = head; Tail = tail } =
      match tail with
      | [] -> singleton head
      | items ->
         head :: items
         |> List.sortBy fn
         |> fromList
         |> Result.toOption
         |> _.Value

// TODO:
// Need to use more frequently when validating incoming data from commands.
// Currently only using when accepting webhook requests from card issuer Lithic.
type NonEmptyString =
   private
   | NonEmptyString of string

   override x.ToString() = string x.Value

   member x.Value = let (NonEmptyString str) = x in str

   static member map (transform: string -> string) (NonEmptyString str) =
      NonEmptyString(transform str)

   static member create(str: string) =
      if String.IsNullOrWhiteSpace str then
         Error "EmptyString"
      else
         Ok(NonEmptyString str)

   static member deserializeUnsafe(str: string) =
      str |> NonEmptyString.create |> Result.toOption |> _.Value

module Guid =
   let parseOptional (id: string) =
      try
         Some <| Guid.Parse id
      with _ ->
         None

type EntityId =
   | EntityId of Guid

   override x.ToString() = string x.Value

   member x.Value = let (EntityId id) = x in id

type CorrelationId =
   | CorrelationId of Guid

   override x.ToString() = string x.Value

   member x.Value = let (CorrelationId id) = x in id

   static member create() = CorrelationId <| Guid.NewGuid()

type EventId =
   | EventId of Guid

   override x.ToString() = string x.Value

   member x.Value = let (EventId id) = x in id

   static member create() = EventId <| Guid.NewGuid()

type TransactionId =
   | TransactionId of CorrelationId

   override x.ToString() = string x.Value

   member x.Value = let (TransactionId corrId) = x in corrId.Value

   member x.AsCorrelationId = CorrelationId x.Value

type OrgId =
   | OrgId of Guid

   override x.ToString() = string x.Value

   member x.Value = let (OrgId id) = x in id

   member x.AsEntityId = EntityId x.Value

   static member fromEntityId(EntityId entityId) = OrgId entityId

type AccountId =
   | AccountId of Guid

   override x.ToString() = string x.Value

   member x.Value = let (AccountId id) = x in id

type ParentAccountId =
   | ParentAccountId of Guid

   override x.ToString() = string x.Value

   member x.Value = let (ParentAccountId id) = x in id

   member x.AsEntityId = EntityId x.Value
   static member fromEntityId(EntityId id) = ParentAccountId id

type EmployeeId =
   | EmployeeId of Guid

   override x.ToString() =
      let (EmployeeId id) = x
      string id

   member x.Value = let (EmployeeId id) = x in id

   member x.AsEntityId = EntityId x.Value

   static member fromEntityId(EntityId id) = EmployeeId id

// Employee who initiated the command.
type InitiatedById =
   | InitiatedById of EmployeeId

   override x.ToString() = string x.Value

   member x.Value = let (EmployeeId id) = x.AsEmployeeId in id

   member x.AsEmployeeId = let (InitiatedById emId) = x in emId

type CardId =
   | CardId of Guid

   override x.ToString() = string x.Value

   member x.Value = let (CardId id) = x in id

type CommandApprovalRuleId =
   | CommandApprovalRuleId of Guid

   override x.ToString() = string x.Value

   member x.Value = let (CommandApprovalRuleId ruleId) = x in ruleId

type CommandApprovalProgressId =
   | CommandApprovalProgressId of CorrelationId

   override x.ToString() = string x.Value

   member x.Value: Guid =
      let (CommandApprovalProgressId progressId) = x
      let (CorrelationId id) = progressId
      id

/// ID indicating a transaction has been synced successfully
/// to the partner bank and is considered settled.
type SettlementId =
   | SettlementId of Guid

   override x.ToString() = string x.Value

   member x.Value = let (SettlementId id) = x in id

type PaymentRequestId =
   | PaymentRequestId of Guid

   override x.ToString() = string x.Value

   member x.Value = let (PaymentRequestId id) = x in id

   member x.AsCorrelationId = CorrelationId x.Value

type CounterpartyId =
   | CounterpartyId of Guid

   override x.ToString() = string x.Value

   member x.Value = let (CounterpartyId id) = x in id

type TransferId =
   | TransferId of Guid

   override x.ToString() = string x.Value

   member x.Value = let (TransferId id) = x in id

   member x.AsCorrelationId = CorrelationId x.Value

type CardIssuerCardId =
   | CardIssuerCardId of Guid

   override x.ToString() = string x.Value

   member x.Value = let (CardIssuerCardId id) = x in id

type CardIssuerTransactionId =
   | CardIssuerTransactionId of Guid

   override x.ToString() = string x.Value

   member x.Value = let (CardIssuerTransactionId id) = x in id

type Initiator = { Id: InitiatedById; Name: string }

module Initiator =
   /// System user represents transactions which do not
   /// originate from a human user.  Used in BillingCycleCommand,
   /// MaintenanceFeeCommand, etc.
   let System = {
      Id =
         "029528ee-a120-4301-b8b5-e9c60d859346"
         |> Guid.Parse
         |> EmployeeId
         |> InitiatedById
      Name = "System"
   }

type Envelope = {
   Id: EventId
   EntityId: EntityId
   OrgId: OrgId
   Timestamp: DateTime
   EventName: string
   CorrelationId: CorrelationId
   InitiatedBy: Initiator
}

type Command<'C> = {
   Id: EventId
   EntityId: EntityId
   OrgId: OrgId
   InitiatedBy: Initiator
   Timestamp: DateTime
   CorrelationId: CorrelationId
   Data: 'C
}

module Command =
   let create<'t>
      (entityId: EntityId)
      (orgId: OrgId)
      (correlationId: CorrelationId)
      (initiator: Initiator)
      (data)
      : Command<'t>
      =
      {
         Id = EventId <| Guid.NewGuid()
         EntityId = entityId
         OrgId = orgId
         Timestamp = DateTime.UtcNow
         CorrelationId = correlationId
         InitiatedBy = initiator
         Data = data
      }

   let envelope (cmd: Command<'C>) : Envelope = {
      Id = cmd.Id
      EntityId = cmd.EntityId
      OrgId = cmd.OrgId
      Timestamp = cmd.Timestamp
      CorrelationId = cmd.CorrelationId
      InitiatedBy = cmd.InitiatedBy
#if FABLE_COMPILER
      EventName = ""
#else
      EventName = typedefof<'C>.Name
#endif
   }

type BankEvent<'E> = {
   Id: EventId
   EntityId: EntityId
   OrgId: OrgId
   InitiatedBy: Initiator
   Timestamp: DateTime
   Data: 'E
   CorrelationId: CorrelationId
} with
#if FABLE_COMPILER
   member x.EventName = ""
#else
   member x.EventName = typedefof<'E>.Name
#endif

module BankEvent =
   let create<'E> (command: Command<'E>) : BankEvent<'E> = {
      Id = command.Id
      EntityId = command.EntityId
      OrgId = command.OrgId
      CorrelationId = command.CorrelationId
      InitiatedBy = command.InitiatedBy
      Timestamp = command.Timestamp
      Data = command.Data
   }

   let create2<'C, 'E> (command: Command<'C>) (evtData: 'E) : BankEvent<'E> = {
      Id = command.Id
      EntityId = command.EntityId
      OrgId = command.OrgId
      InitiatedBy = command.InitiatedBy
      CorrelationId = command.CorrelationId
      Timestamp = command.Timestamp
      Data = evtData
   }

[<RequireQualifiedAccess>]
type OrgStateTransitionError =
   | OrgNotReadyToStartOnboarding
   | OrgNotReadyToActivate
   | OrgNotActive
   | ApprovalRuleNotFound
   | ApprovalRuleHasConflictingCriteria
   | ApprovalRuleMultipleOfType of commandType: string
   | ApprovalRuleHasGapInCriteria of
      gap: decimal *
      amountToCloseGap: decimal *
      gapPrecedesOrFollows: string
   | ApprovalProgressWorklowNotActive
   | ApproverUnrecognized of EmployeeId * name: string
   | ApproverAlreadyApprovedCommand of EmployeeId * name: string

type AccountStateTransitionError =
   | ParentAccountAlreadyInitialized
   | ParentAccountNotActive
   | AccountNotReadyToActivate
   | AccountNotFound of AccountId
   | AccountNotActive of accountName: string
   | InsufficientBalance of balance: decimal * accountName: string
   | ExceededDailyInternalTransferLimit of decimal
   | ExceededDailyDomesticTransferLimit of decimal
   | TransferExpectedToOccurWithinOrg
   | AutoTransferOnlyOneRuleMayExistAtATime
   | AutoTransferRuleDoesNotExist
   | AutoTransferCycleDetected
   | RecipientRegistered
   | RecipientNotFound
   | RecipientAlreadyConfirmed

type EmployeeStateTransitionError =
   | EmployeeNotReadyToActivate
   | EmployeeNotActive
   | EmployeeStatusDisallowsInviteProgression of string
   | CardNotFound
   | CardPending
   | CardLocked
   | CardExpired
   | ExceededDailyDebit of limit: decimal * accrued: decimal
   | ExceededMonthlyDebit of limit: decimal * accrued: decimal
   | EmployeeStatusDisallowsAccessRestore of string
   | PurchaseProgressPurchaseNotFound
   | PurchaseProgressNoAdditionalEvents

type Err =
   | DatabaseError of exn
   | CacheError of exn
   | ValidationError of ValidationErrors
   | OrgStateTransitionError of OrgStateTransitionError
   | AccountStateTransitionError of AccountStateTransitionError
   | EmployeeStateTransitionError of EmployeeStateTransitionError
   | SerializationError of string
   | InvalidStatusCodeError of serviceName: string * code: int
   | SignalRError of exn
   | NetworkError of exn
   | NotImplementedError of NotImplementedException
   | UnexpectedError of string

   override x.ToString() =
      match x with
      | DatabaseError e -> $"DatabaseError: %s{e.Message}"
      | CacheError e -> $"CacheError: %s{e.Message}"
      | ValidationError e -> $"ValidationError: {ValidationErrors.toList e}"
      | OrgStateTransitionError e -> $"OrgStateTransitionError: {e}"
      | AccountStateTransitionError e -> $"AccountStateTransitionError: {e}"
      | EmployeeStateTransitionError e -> $"EmployeeStateTransitionError: {e}"
      | InvalidStatusCodeError(service, code) ->
         $"InvalidStatusCodeError {service}: Invalid status code {code}"
      | SerializationError err -> $"SerializationError: {err}"
      | SignalRError e -> $"SignalRError: {e.Message}"
      | NetworkError e -> $"NetworkError: {e.Message}"
      | NotImplementedError e -> e.Message
      | UnexpectedError msg -> msg

   member x.HumanFriendly =
      match x with
      | DatabaseError _ -> "Database Error"
      | CacheError _ -> "Cache Error"
      | SerializationError _ -> "Serialization Error"
      | SignalRError _ -> "SignalR Error"
      | NetworkError _ -> "Network Error"
      | InvalidStatusCodeError _ -> "Invalid Status Code"
      | NotImplementedError _ -> "Not Implemented"
      | UnexpectedError msg -> msg
      | ValidationError e ->
         e
         |> ValidationErrors.toList
         |> List.fold (fun acc errMsg -> acc + errMsg + ", ") ""
         |> fun str -> str.Remove(str.Length - 2)
      | OrgStateTransitionError e ->
         match e with
         | OrgStateTransitionError.OrgNotActive -> "Org Not Active"
         | OrgStateTransitionError.OrgNotReadyToStartOnboarding ->
            "Org Not Ready To Start Onboarding"
         | OrgStateTransitionError.OrgNotReadyToActivate ->
            "Org Not Ready to Activate"
         | OrgStateTransitionError.ApprovalRuleNotFound ->
            "Approval Rule Not Found"
         | OrgStateTransitionError.ApprovalRuleHasConflictingCriteria ->
            "Approval Rule Contains Criteria Conflicting With Another Rule"
         | OrgStateTransitionError.ApprovalRuleMultipleOfType cmdType ->
            $"Configuring multiple rules are not allowed for command type {cmdType}"
         | OrgStateTransitionError.ApprovalRuleHasGapInCriteria(gap,
                                                                amountToCloseGap,
                                                                gapPrecedesOrFollows) ->
            $"Detected a ${Math.Round(gap, 2)} gap with the {gapPrecedesOrFollows} rule's criteria.
              Set to ${Math.Round(amountToCloseGap, 2)} to fix it."
         | OrgStateTransitionError.ApproverUnrecognized(employeeId, name) ->
            $"Unrecognized approver approved/declined command {name}-{employeeId}"
         | OrgStateTransitionError.ApprovalProgressWorklowNotActive ->
            "Approval Progress Workflow Not Active"
         | OrgStateTransitionError.ApproverAlreadyApprovedCommand(employeeId,
                                                                  name) ->
            $"Approver already approved command {name}-{employeeId}"
      | AccountStateTransitionError e ->
         match e with
         | ParentAccountAlreadyInitialized ->
            "Parent account already initialized with primary accounts."
         | ParentAccountNotActive -> "Parent account not active."
         | AccountNotFound accountId -> $"Account Not Found {accountId}"
         | AccountStateTransitionError.AccountNotActive accountName ->
            $"Account Not Active {accountName}"
         | AccountStateTransitionError.AccountNotReadyToActivate ->
            "Account Not Ready to Activate"
         | AccountStateTransitionError.ExceededDailyInternalTransferLimit limit ->
            $"Exceeded Daily Internal Transfer Limit ${limit}"
         | AccountStateTransitionError.ExceededDailyDomesticTransferLimit limit ->
            $"Exceeded Daily Domestic Transfer Limit ${limit}"
         | AccountStateTransitionError.InsufficientBalance(balance, accountName) ->
            $"Insufficient Balance ${balance} for {accountName}"
         | AccountStateTransitionError.TransferExpectedToOccurWithinOrg ->
            "Expected to transfer funds within organization."
         | AccountStateTransitionError.AutoTransferOnlyOneRuleMayExistAtATime ->
            "Only one auto transfer rule may exist at time."
         | AccountStateTransitionError.AutoTransferRuleDoesNotExist ->
            "Attempted to update an auto transfer rule which does not exist."
         | AccountStateTransitionError.AutoTransferCycleDetected ->
            "Attempted to add a rule which would create cyclic transfers."
         | AccountStateTransitionError.RecipientRegistered ->
            "Recipient Registered"
         | AccountStateTransitionError.RecipientNotFound ->
            "Recipient Not Found"
         | AccountStateTransitionError.RecipientAlreadyConfirmed ->
            "Recipient Already Confirmed"
      | EmployeeStateTransitionError e ->
         match e with
         | EmployeeStateTransitionError.EmployeeNotActive ->
            "Employee Not Active"
         | EmployeeStateTransitionError.EmployeeNotReadyToActivate ->
            "Employee Not Ready to Activate"
         | EmployeeStateTransitionError.CardPending -> "Card Pending"
         | EmployeeStateTransitionError.CardExpired -> "Card Expired"
         | EmployeeStateTransitionError.CardLocked -> "Card Locked"
         | EmployeeStateTransitionError.CardNotFound -> "Card Not Found"
         | EmployeeStateTransitionError.ExceededDailyDebit(limit, _) ->
            $"Exceeded Daily Purchase Limit ${limit}"
         | EmployeeStateTransitionError.ExceededMonthlyDebit(limit, _) ->
            $"Exceeded Monthly Purchase Limit ${limit}"
         | EmployeeStateTransitionError.EmployeeStatusDisallowsInviteProgression state ->
            $"Employee not in a state ({state}) to cancel invite."
         | EmployeeStateTransitionError.EmployeeStatusDisallowsAccessRestore status ->
            $"Employee not in a state ({status}) to restore access."
         | EmployeeStateTransitionError.PurchaseProgressPurchaseNotFound ->
            "Purchase progress not found"
         | EmployeeStateTransitionError.PurchaseProgressNoAdditionalEvents ->
            "Purchase progress no additional events"

let validationErrorsHumanFriendly
   (result: ValidationResult<'t>)
   : Result<'t, string>
   =
   result |> Result.mapError (Err.ValidationError >> _.HumanFriendly)

[<RequireQualifiedAccess>]
type Role =
   | Admin
   | CardOnly
   | Scholar
   //| Custom of CustomEmployeeBehaviors
   member x.Display =
      match x with
      | Admin -> "Admin"
      | CardOnly -> "Card Only"
      | Scholar -> "Scholar"

   override x.ToString() =
      match x with
      | Admin -> "Admin"
      | CardOnly -> "CardOnly"
      | Scholar -> "Scholar"

   static member fromString(role: string) : Role option =
      if String.IsNullOrEmpty role then
         None
      else
         match role.ToLower() with
         | "admin" -> Some Admin
         | "cardonly" -> Some CardOnly
         | "scholar" -> Some Scholar
         | _ -> None

   static member fromStringUnsafe(role: string) : Role =
      match Role.fromString role with
      | None -> failwith "Error attempting to cast string to Role"
      | Some status -> status

type AccountNumber =
   | AccountNumber of int64

   override x.ToString() =
      let (AccountNumber num) = x
      string num

   member x.Last4 = x |> string |> (fun str -> str.Substring(str.Length - 4))

   static member fromString: Validator<string, AccountNumber> =
      Lib.Validators.parseInt64 *|* string
      >=> Check.String.betweenLen 6 15 *|* (Int64.Parse >> AccountNumber)

   // TODO: Remove this & provide a workflow to check account numbers against DB
   static member generate() : AccountNumber =
      let random = System.Random()

      List.init 15 (fun _ -> random.Next(1, 9) |> string)
      |> String.concat ""
      |> int64
      |> AccountNumber

   static member Empty = AccountNumber 1234567

type RoutingNumber =
   | RoutingNumber of string

   override x.ToString() =
      let (RoutingNumber num) = x
      string num

   static member fromString: Validator<string, RoutingNumber> =
      Check.String.equalsLen 9
      // Ensure each character is a digit
      >=> Check.String.pattern @"^\d+$" *|* RoutingNumber

   static member Empty = RoutingNumber "123456789"

type PartnerBankAccountId =
   | PartnerBankAccountId of string

   override x.ToString() = string x.Value

   member x.Value = let (PartnerBankAccountId id) = x in id

type PartnerBankLegalEntityId =
   | PartnerBankLegalEntityId of string

   override x.ToString() = string x.Value

   member x.Value = let (PartnerBankLegalEntityId id) = x in id

type PartnerBankCounterpartyId =
   | PartnerBankCounterpartyId of string

   override x.ToString() = string x.Value

   member x.Value = let (PartnerBankCounterpartyId id) = x in id

type Address = {
   City: string
   CountryCode: string
   Line1: string
   Line2: string
   PostalCode: string
   State: string
}

module Address =
   let empty = {
      City = ""
      CountryCode = ""
      Line1 = ""
      Line2 = ""
      PostalCode = ""
      State = ""
   }
