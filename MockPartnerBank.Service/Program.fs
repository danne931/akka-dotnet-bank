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

type Sender = {
   Name: string
   AccountNumber: string
   RoutingNumber: string
}

type Recipient = {
   Name: string
   AccountNumber: string
   RoutingNumber: string
   Depository: string
}

type TransferRequest = {
   Sender: Sender
   Recipient: Recipient
   Amount: decimal
   Date: DateTime
   PaymentNetwork: string
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
   Action: string
   TransactionId: string
   Data: obj
}

type Response = {
   Ok: bool
   Status: string
   Reason: string
   TransactionId: string
   ExpectedSettlementDate: DateTime option
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
      |> List.exists (fun p -> p = req.PaymentNetwork.ToLower())
      |> not
   then
      {
         res with
            Ok = false
            Reason = "InvalidPaymentNetwork"
      }
   elif
      depository
      |> List.exists (fun d -> d = req.Recipient.Depository.ToLower())
      |> not
   then
      {
         res with
            Ok = false
            Reason = "InvalidDepository"
      }
   elif
      String.IsNullOrEmpty req.Recipient.AccountNumber
      || String.IsNullOrEmpty req.Recipient.RoutingNumber
   then
      {
         res with
            Ok = false
            Reason = "InvalidAccountInfo"
      }
   elif req.Amount <= 0m then
      {
         res with
            Ok = false
            Reason = "InvalidAmount"
      }
   else
      transfersInProgress.TryAdd(res.TransactionId, (0, req)) |> ignore
      { res with Status = "ReceivedRequest" }

let processProgressCheckRequest (req: TransferRequest) (res: Response) =
   let exists, existing = transfersInProgress.TryGetValue res.TransactionId
   let progressCheckCount, transfer = existing

   if not exists then
      {
         res with
            Ok = false
            Reason = "NoTransferProcessing"
      }
   else if progressCheckCount < 1 then
      transfersInProgress.TryUpdate(
         res.TransactionId,
         (progressCheckCount + 1, transfer),
         existing
      )
      |> ignore

      {
         res with
            Status = "VerifyingAccountInfo"
            ExpectedSettlementDate = Some(DateTime.UtcNow.AddDays 3)
      }
   else
      transfersInProgress.TryRemove res.TransactionId |> ignore
      { res with Status = "Complete" }

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
            Ok = true
            Status = ""
            Reason = ""
            TransactionId = req.TransactionId
            ExpectedSettlementDate = None
         }

         let serializedResponse =
            if req.Action = "TransferRequest" then
               let req =
                  req.Data
                  |> string
                  |> JsonSerializer.Deserialize<TransferRequest>

               processTransferRequest req transferRes
               |> serializeTransferResponse
            elif req.Action = "ProgressCheck" then
               let req =
                  req.Data
                  |> string
                  |> JsonSerializer.Deserialize<TransferRequest>

               processProgressCheckRequest req transferRes
               |> serializeTransferResponse
            elif req.Action = "CreateLegalBusinessEntity" then
               let req =
                  req.Data
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
            elif req.Action = "CreateInternalAccount" then
               let req =
                  req.Data
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
                     Ok = false
                     Reason = "InvalidAction"
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
