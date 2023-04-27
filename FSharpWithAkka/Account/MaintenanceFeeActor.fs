[<RequireQualifiedAccess>]
module MaintenanceFeeActor

open type Echo.Process
open System
open System.Threading.Tasks

open Lib.Types
open BankTypes
open Bank.Account.Domain
open Microsoft.FSharp.Core.Option

let private canIssueMaintenanceFee
   (getAccountEvents: Guid -> AccountEvent List Option Task)
   lookBackDate
   accountId
   =
   let eventsOpt = getAccountEvents(accountId).Result

   eventsOpt
   |> Option.bind (fun events ->
      let res = MaintenanceFee.computeFeeCriteria lookBackDate events

      if res.depositCriteria || res.balanceCriteria then
         printfn
            "Some criteria met for skipping the monthly maintenance fee: Deposit (%A) / Balance (%A)"
            res.depositCriteria
            res.balanceCriteria

         None
      else
         Some res)


let start
   (getAccountEvents: Guid -> AccountEvent List Option Task)
   (lookBackDate: _ -> DateTime)
   (scheduledAt: _ -> TimeSpan)
   (accountId: Guid)
   =
   let pid =
      spawn<Guid> (
         "monthly_maintenance_fee",
         (fun id ->
            let criteriaMet =
               canIssueMaintenanceFee getAccountEvents (lookBackDate ()) id

            if isSome criteriaMet then
               tellParent (
                  DebitCommand(
                     accountId,
                     DateTime.UtcNow,
                     MaintenanceFee.Constants.Fee,
                     Account.Constants.DebitOriginMaintenanceFee,
                     "Monthly Maintenance Fee"
                  )
                  |> ActorStateChangeCommand.init
               )
               |> ignore

            // Schedule the next maintenance fee check
            tellSelf (accountId, scheduledAt ()) |> ignore)
      )

   tell (pid, accountId, scheduledAt ()) |> ignore
   pid
