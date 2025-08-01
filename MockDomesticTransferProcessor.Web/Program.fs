open Microsoft.AspNetCore.Builder
open Akkling
open Akkling.IO
open Akkling.IO.Tcp
open System
open System.Text.Json
open System.Net
open System.Collections.Generic

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

type Request = {
   Action: string
   Sender: Sender
   Recipient: Recipient
   Amount: decimal
   Date: DateTime
   TransactionId: string
   PaymentNetwork: string
}

type Response = {
   Sender: Sender
   Recipient: Recipient
   Ok: bool
   Status: string
   Reason: string
   TransactionId: string
   ExpectedSettlementDate: DateTime option
}

type InProgressTransfers = Dictionary<string, int * Request>

let inMemoryState = InProgressTransfers()

let paymentNetworks = [ "ach" ]
let depository = [ "checking"; "savings" ]

// Compute response & mutate in-memory state of in-progress transfers.
let processRequest (req: Request) =
   let res = {
      Sender = req.Sender
      Recipient = req.Recipient
      Ok = true
      Status = ""
      Reason = ""
      TransactionId = req.TransactionId
      ExpectedSettlementDate = None
   }

   if req.Action = "TransferRequest" then
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
         inMemoryState.Add(res.TransactionId, (0, req))
         { res with Status = "ReceivedRequest" }
   elif req.Action = "ProgressCheck" then
      if not <| inMemoryState.ContainsKey res.TransactionId then
         {
            res with
               Ok = false
               Reason = "NoTransferProcessing"
         }
      else
         let progressCheckCount, request = inMemoryState[res.TransactionId]

         if progressCheckCount < 1 then
            inMemoryState[res.TransactionId] <- progressCheckCount + 1, request

            {
               res with
                  Status = "VerifyingAccountInfo"
                  ExpectedSettlementDate = Some(DateTime.UtcNow.AddDays 3)
            }
         else
            inMemoryState.Remove res.TransactionId |> ignore
            { res with Status = "Complete" }
   else
      {
         res with
            Ok = false
            Reason = "InvalidAction"
      }

let serializeResponse (response: Response) =
   response |> JsonSerializer.Serialize |> ByteString.ofUtf8String

let actorSystem =
   System.create "mock-domestic-transfer-processor"
   <| Configuration.defaultConfig ()

// Reads from the connection are delegated to this actor.
let tcpMessageHandler connection (ctx: Actor<obj>) =
   monitor ctx connection |> ignore

   let rec loop () = actor {
      let! msg = ctx.Receive()

      match msg with
      | Received data ->
         let req = data |> string |> JsonSerializer.Deserialize<Request>
         printfn "Received request %A" req

         let res = processRequest req

         ctx.Sender() <! TcpMessage.Write(serializeResponse res)
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
