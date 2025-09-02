open Microsoft.AspNetCore.Builder
open Akkling
open Akkling.IO
open Akkling.IO.Tcp
open System
open System.Text.Json
open System.Net
open System.Collections.Concurrent

// This module mocks processing of ACH transfers, eventually returning
// success responses via TCP after a few progress check requests from the
// client.

let builder = WebApplication.CreateBuilder()
let app = builder.Build()

type Address = {
   city: string
   country_code: string
   line_1: string
   line_2: string
   postal_code: string
   state: string
}

type TransferRequest = {
   originating_account_id: string
   counterparty_id: string
   amount: decimal
   date: DateTime
   flow: string
   payment_network: string
   idempotency_key: string
}

type BookTransferRequest = {
   amount: decimal
   sender_bank_account_id: string
   receiver_bank_account_id: string
   idempotency_key: string
}

type BookTransferResponse = {
   confirmation_id: string
   idempotency_key: string
   amount: decimal
   sender_bank_account_id: string
   receiver_bank_account_id: string
   status: string
   details: obj
}

type CounterpartyRequest = {
   name: string
   account_number: string
   routing_number: string
   depository: string
   address: Address
}

type LegalEntityCreateRequest = {
   address: Address
   business_name: string
   description: string
   ein: string
   legal_type: string
   registration_id: {|
      country_code: string
      number: string
   |}
   website: string
}

type InternalAccountRequest = {
   legal_entity_id: string
   description: string
}

type InternalAccount = {
   legal_entity_id: string
   description: string
   account_id: string
   account_number: string
   routing_number: string
}

type Request = { action: string; data: obj }

type TransferResponse = {
   ok: bool
   status: string
   reason: string
   expected_settlement_date: DateTime
   idempotency_key: string
}

let transfersInProgress = ConcurrentDictionary<string, int * TransferRequest>()

let legalEntities = ConcurrentDictionary<string, LegalEntityCreateRequest>()
let internalAccounts = ConcurrentDictionary<string, InternalAccount>()
let counterparties = ConcurrentDictionary<string, CounterpartyRequest>()

let paymentNetworks = [ "ach" ]
let depository = [ "checking"; "savings" ]
let achDirection = [ "credit"; "debit" ]

// Compute response & mutate in-memory state of in-progress transfers.
let processTransferRequest (req: TransferRequest) (res: TransferResponse) =
   let counterpartyExists, counterparty =
      counterparties.TryGetValue req.counterparty_id

   if
      paymentNetworks
      |> List.exists (fun p -> p = req.payment_network.ToLower())
      |> not
   then
      {
         res with
            ok = false
            reason = "InvalidPaymentNetwork"
      }
   elif not counterpartyExists then
      {
         res with
            ok = false
            reason = "CounterpartyNotFound"
      }
   elif
      depository
      |> List.exists (fun d -> d = counterparty.depository.ToLower())
      |> not
   then
      {
         res with
            ok = false
            reason = "InvalidDepository"
      }
   elif req.amount <= 0m then
      {
         res with
            ok = false
            reason = "InvalidAmount"
      }
   elif
      depository
      |> List.exists (fun d -> d = counterparty.depository.ToLower())
      |> not
   then
      {
         res with
            ok = false
            reason = "InvalidACHDirection"
      }
   elif not (internalAccounts.ContainsKey req.originating_account_id) then
      {
         res with
            ok = false
            reason = "SenderBankAccountNotFound"
      }
   else
      transfersInProgress.TryAdd(req.idempotency_key, (0, req)) |> ignore
      { res with status = "ReceivedRequest" }

let processProgressCheckRequest (req: TransferRequest) (res: TransferResponse) =
   let exists, existing = transfersInProgress.TryGetValue req.idempotency_key
   let progressCheckCount, transfer = existing

   if not exists then
      {
         res with
            ok = false
            reason = "NoTransferProcessing"
      }
   else if progressCheckCount < 1 then
      transfersInProgress.TryUpdate(
         req.idempotency_key,
         (progressCheckCount + 1, transfer),
         existing
      )
      |> ignore

      {
         res with
            status = "VerifyingAccountInfo"
            expected_settlement_date = DateTime.UtcNow.AddDays 3
      }
   else
      transfersInProgress.TryRemove req.idempotency_key |> ignore
      { res with status = "Complete" }

let serializeTransferResponse (response: TransferResponse) =
   response |> JsonSerializer.Serialize |> ByteString.ofUtf8String

let actorSystem =
   System.create "mock-partner-bank" <| Configuration.defaultConfig ()

// Reads from the connection are delegated to this actor.
let tcpMessageHandler connection (ctx: Actor<obj>) =
   monitor ctx connection |> ignore

   let rec loop () = actor {
      let! msg = ctx.Receive()

      match msg with
      | Received data ->
         let req = data |> string |> JsonSerializer.Deserialize<Request>
         printfn "Received request %A" req

         let serializedResponse =
            if req.action = "CreateCounterparty" then
               let info =
                  req.data
                  |> string
                  |> JsonSerializer.Deserialize<CounterpartyRequest>

               if
                  String.IsNullOrEmpty info.account_number
                  || String.IsNullOrEmpty info.routing_number
               then
                  {|
                     ok = false
                     reason = "InvalidAccountInfo"
                  |}
                  |> JsonSerializer.Serialize
                  |> ByteString.ofUtf8String
               else
                  let cpId = "ctpy-" + Guid.NewGuid().ToString "N"

                  counterparties.TryAdd(cpId, info) |> ignore

                  {| id = cpId |}
                  |> JsonSerializer.Serialize
                  |> ByteString.ofUtf8String
            elif req.action = "TransferRequest" then
               let req =
                  req.data
                  |> string
                  |> JsonSerializer.Deserialize<TransferRequest>

               let transferRes = {
                  ok = true
                  status = ""
                  reason = ""
                  expected_settlement_date = DateTime.MinValue
                  idempotency_key = req.idempotency_key
               }

               processTransferRequest req transferRes
               |> serializeTransferResponse
            elif req.action = "ProgressCheck" then
               let req =
                  req.data
                  |> string
                  |> JsonSerializer.Deserialize<TransferRequest>

               let transferRes = {
                  ok = true
                  status = ""
                  reason = ""
                  expected_settlement_date = DateTime.MinValue
                  idempotency_key = req.idempotency_key
               }

               processProgressCheckRequest req transferRes
               |> serializeTransferResponse
            elif req.action = "BookTransfer" then
               let info =
                  req.data
                  |> string
                  |> JsonSerializer.Deserialize<BookTransferRequest>

               let res = {
                  confirmation_id = Guid.NewGuid().ToString "N"
                  idempotency_key = info.idempotency_key
                  amount = info.amount
                  sender_bank_account_id = info.sender_bank_account_id
                  receiver_bank_account_id = info.receiver_bank_account_id
                  status = "COMPLETED"
                  details = {| |}
               }

               let res =
                  if info.amount <= 0m then
                     {
                        res with
                           status = "REJECTED"
                           details = {| reason = "InvalidAmount" |}
                     }
                  elif
                     not (
                        internalAccounts.ContainsKey info.sender_bank_account_id
                     )
                  then
                     {
                        res with
                           status = "REJECTED"
                           details = {|
                              reason = "InvalidSenderBankAccount"
                           |}
                     }
                  elif
                     not (
                        internalAccounts.ContainsKey
                           info.receiver_bank_account_id
                     )
                  then
                     {
                        res with
                           status = "REJECTED"
                           details = {|
                              reason = "InvalidRecipientBankAccount"
                           |}
                     }
                  else
                     res

               res |> JsonSerializer.Serialize |> ByteString.ofUtf8String
            elif req.action = "CreateLegalBusinessEntity" then
               let req =
                  req.data
                  |> string
                  |> JsonSerializer.Deserialize<LegalEntityCreateRequest>

               let entityId = Guid.NewGuid().ToString "N"

               legalEntities.TryAdd(entityId, req) |> ignore

               {|
                  id = entityId
                  business_details = req
                  verification_status = "VERIFIED"
                  review_reasons = []
               |}
               |> JsonSerializer.Serialize
               |> ByteString.ofUtf8String
            elif req.action = "CreateInternalAccount" then
               let req =
                  req.data
                  |> string
                  |> JsonSerializer.Deserialize<InternalAccountRequest>

               let accountNumber =
                  let rnd = Random()

                  List.init 15 (fun _ -> rnd.Next(1, 9) |> string)
                  |> String.concat ""

               let routingNumber = "950123931"

               let accountId = Guid.NewGuid().ToString "N"

               let account = {
                  legal_entity_id = req.legal_entity_id
                  description = req.description
                  account_id = accountId
                  account_number = accountNumber
                  routing_number = routingNumber
               }

               internalAccounts.TryAdd(accountId, account) |> ignore

               {|
                  account_id = accountId
                  account_number = accountNumber
                  routing_number = routingNumber
               |}
               |> JsonSerializer.Serialize
               |> ByteString.ofUtf8String
            else
               serializeTransferResponse {
                  ok = false
                  status = ""
                  reason = "InvalidAction"
                  expected_settlement_date = DateTime.MinValue
                  idempotency_key = ""
               }

         ctx.Sender() <! TcpMessage.Write serializedResponse
         return! loop ()
      | Terminated(_, _, _)
      | ConnectionClosed _ ->
         printfn "<Disconnected>"
         return Stop
      | _ -> return Ignore
   }

   loop ()

let port, ip =
   try
      int <| Environment.GetEnvironmentVariable "TCP_BIND_PORT",
      Dns.GetHostAddresses(Dns.GetHostName())[0]
   with _ ->
      5007, IPAddress.Loopback

let endpoint = IPEndPoint(ip, port)

let tcpConnectionListener ctx =
   IO.Tcp ctx <! TcpMessage.Bind(untyped ctx.Self, endpoint, 100)

   let rec loop () = actor {
      let! (msg: obj) = ctx.Receive()

      match msg with
      | Bound endpoint -> printfn $"<Bound> \nEndpoint {endpoint}"
      | Connected(remote, _) ->
         let conn = ctx.Sender()

         printfn
            "<Connected> \nRemote Address: %A\nSender Actor Path: %A"
            remote
            conn.Path

         let aref = spawn ctx null (props (tcpMessageHandler conn))
         conn <! TcpMessage.Register(untyped aref)

         return! loop ()
      | _ -> return Ignore
   }

   loop ()

let listener =
   spawn actorSystem "tcpConnectionListener" (props tcpConnectionListener)

app.Run()

type Program() = class end
