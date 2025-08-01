module Lib.SharedTypes

open System
open Validus
open Validus.Operators

module Guid =
   let parseOptional (id: string) =
      try
         Some <| Guid.Parse id
      with _ ->
         None

type EntityId =
   | EntityId of Guid

   override x.ToString() =
      let (EntityId id) = x
      string id

module EntityId =
   let get (entityId: EntityId) : Guid =
      let (EntityId id) = entityId
      id

type CorrelationId =
   | CorrelationId of Guid

   override x.ToString() =
      let (CorrelationId id) = x
      string id

module CorrelationId =
   let create () = CorrelationId <| Guid.NewGuid()

   let get (corrId: CorrelationId) : Guid =
      let (CorrelationId id) = corrId
      id

type EventId =
   | EventId of Guid

   override x.ToString() =
      let (EventId id) = x
      string id

type TransactionId =
   | TransactionId of CorrelationId

   override x.ToString() =
      let (TransactionId id) = x
      string id

module TransactionId =
   let get (txnId: TransactionId) : Guid =
      let (TransactionId id) = txnId
      CorrelationId.get id

   let toCorrelationId (id: TransactionId) : CorrelationId =
      let (TransactionId txnId) = id
      txnId

type OrgId =
   | OrgId of Guid

   override x.ToString() =
      let (OrgId id) = x
      string id

module OrgId =
   let get (orgId: OrgId) : Guid =
      let (OrgId id) = orgId
      id

   let toEntityId (orgId: OrgId) : EntityId =
      let (OrgId id) = orgId
      EntityId id

   let fromEntityId (entityId: EntityId) : OrgId =
      let (EntityId id) = entityId
      OrgId id

type AccountId =
   | AccountId of Guid

   override x.ToString() =
      let (AccountId id) = x
      string id

module AccountId =
   let get (accountId: AccountId) : Guid =
      let (AccountId id) = accountId
      id

type ParentAccountId =
   | ParentAccountId of Guid

   override x.ToString() =
      let (ParentAccountId id) = x
      string id

module ParentAccountId =
   let get (accountId: ParentAccountId) : Guid =
      let (ParentAccountId id) = accountId
      id

   let toEntityId (accountId: ParentAccountId) : EntityId =
      let (ParentAccountId id) = accountId
      EntityId id

   let fromEntityId (entityId: EntityId) : ParentAccountId =
      let (EntityId id) = entityId
      ParentAccountId id

type EmployeeId =
   | EmployeeId of Guid

   override x.ToString() =
      let (EmployeeId id) = x
      string id

module EmployeeId =
   let get (employeeId: EmployeeId) : Guid =
      let (EmployeeId id) = employeeId
      id

   let toEntityId (employeeId: EmployeeId) : EntityId =
      let (EmployeeId id) = employeeId
      EntityId id

   let fromEntityId (entityId: EntityId) : EmployeeId =
      let (EntityId id) = entityId
      EmployeeId id

// Employee who initiated the command.
type InitiatedById =
   | InitiatedById of EmployeeId

   override x.ToString() =
      let (InitiatedById initiatedById) = x
      let (EmployeeId id) = initiatedById
      string id

module InitiatedById =
   let toEmployeeId (id: InitiatedById) : EmployeeId =
      let (InitiatedById employeeId) = id
      employeeId

   let get (initiatedById: InitiatedById) : Guid =
      let (EmployeeId id) = toEmployeeId initiatedById
      id

type CardId =
   | CardId of Guid

   override x.ToString() =
      let (CardId id) = x
      string id

module CardId =
   let get (cardId: CardId) : Guid =
      let (CardId id) = cardId
      id

type CommandApprovalRuleId =
   | CommandApprovalRuleId of Guid

   override x.ToString() = string x.Value

   member x.Value: Guid =
      let (CommandApprovalRuleId ruleId) = x
      ruleId

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

   override x.ToString() =
      let (SettlementId id) = x
      string id

type PaymentRequestId =
   | PaymentRequestId of Guid

   override x.ToString() =
      let (PaymentRequestId id) = x
      string id

module PaymentRequestId =
   let get (payId: PaymentRequestId) =
      let (PaymentRequestId id) = payId
      id

   let toCorrelationId (PaymentRequestId payId) = CorrelationId payId

type TransferId =
   | TransferId of Guid

   override x.ToString() =
      let (TransferId id) = x
      string id

module TransferId =
   let get (transferId: TransferId) =
      let (TransferId id) = transferId
      id

   let toCorrelationId (TransferId transferId) = CorrelationId transferId

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
   | RecipientDeactivated
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
         | AccountStateTransitionError.RecipientDeactivated ->
            "Recipient Deactivated"
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
   | RoutingNumber of int

   override x.ToString() =
      let (RoutingNumber num) = x
      string num

   static member fromString: Validator<string, RoutingNumber> =
      Lib.Validators.parseInt *|* string
      >=> Check.String.equalsLen 9 *|* (Int32.Parse >> RoutingNumber)

   static member Empty = RoutingNumber 123456789

type ParentAccountNumber = ParentAccountNumber of AccountNumber
type ParentRoutingNumber = ParentRoutingNumber of RoutingNumber

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

type PositiveAmount =
   private
   | PositiveAmount of decimal

   static member create(value: decimal) =
      if value > 0m then
         Ok(PositiveAmount value)
      else
         Error "Non-Positive Amount"

   static member get(PositiveAmount v) = v

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

type PendingDeductions = {
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
