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

type Recipient = {
   name: string
   account_number: string
   routing_number: string
   depository: string
}

type TransferRequest = {
   originating_account_id: string
   recipient: Recipient
   amount: decimal
   date: DateTime
   payment_network: string
}

type BookTransferRequest = {
   amount: decimal
   sender_bank_account_id: string
   receiver_bank_account_id: string
}

type LegalEntityCreateRequest = {
   address: {|
      city: string
      country_code: string
      line_1: string
      line_2: string
      postal_code: string
      state: string
   |}
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

type Request = {
   action: string
   idempotency_key: string
   data: obj
}

type Response = {
   ok: bool
   status: string
   reason: string
   idempotency_key: string
   expected_settlement_date: DateTime
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

let transfersInProgress = ConcurrentDictionary<string, int * TransferRequest>()

let legalEntities = ConcurrentDictionary<string, LegalEntityCreateRequest>()
let internalAccounts = ConcurrentDictionary<string, InternalAccount>()

let paymentNetworks = [ "ach" ]
let depository = [ "checking"; "savings" ]

// Compute response & mutate in-memory state of in-progress transfers.
let processTransferRequest (req: TransferRequest) (res: Response) =
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
   elif
      depository
      |> List.exists (fun d -> d = req.recipient.depository.ToLower())
      |> not
   then
      {
         res with
            ok = false
            reason = "InvalidDepository"
      }
   elif
      String.IsNullOrEmpty req.recipient.account_number
      || String.IsNullOrEmpty req.recipient.routing_number
   then
      {
         res with
            ok = false
            reason = "InvalidAccountInfo"
      }
   elif req.amount <= 0m then
      {
         res with
            ok = false
            reason = "InvalidAmount"
      }
   elif not (internalAccounts.ContainsKey req.originating_account_id) then
      {
         res with
            ok = false
            reason = "SenderBankAccountNotFound"
      }
   else
      transfersInProgress.TryAdd(res.idempotency_key, (0, req)) |> ignore
      { res with status = "ReceivedRequest" }

let processProgressCheckRequest (req: TransferRequest) (res: Response) =
   let exists, existing = transfersInProgress.TryGetValue res.idempotency_key
   let progressCheckCount, transfer = existing

   if not exists then
      {
         res with
            ok = false
            reason = "NoTransferProcessing"
      }
   else if progressCheckCount < 1 then
      transfersInProgress.TryUpdate(
         res.idempotency_key,
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
      transfersInProgress.TryRemove res.idempotency_key |> ignore
      { res with status = "Complete" }

let serializeTransferResponse (response: Response) =
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

         let transferRes = {
            ok = true
            status = ""
            reason = ""
            idempotency_key = req.idempotency_key
            expected_settlement_date = DateTime.MinValue
         }

         let serializedResponse =
            if req.action = "TransferRequest" then
               let req =
                  req.data
                  |> string
                  |> JsonSerializer.Deserialize<TransferRequest>

               processTransferRequest req transferRes
               |> serializeTransferResponse
            elif req.action = "ProgressCheck" then
               let req =
                  req.data
                  |> string
                  |> JsonSerializer.Deserialize<TransferRequest>

               processProgressCheckRequest req transferRes
               |> serializeTransferResponse
            elif req.action = "BookTransfer" then
               let info =
                  req.data
                  |> string
                  |> JsonSerializer.Deserialize<BookTransferRequest>

               let res = {
                  confirmation_id = Guid.NewGuid().ToString "N"
                  idempotency_key = req.idempotency_key
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
                  transferRes with
                     ok = false
                     reason = "InvalidAction"
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
