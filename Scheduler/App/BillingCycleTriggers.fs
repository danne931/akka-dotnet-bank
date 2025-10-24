[<RequireQualifiedAccess>]
module BillingCycleTriggers

open Quartz

let scheduleMonthly (logInfo: string -> unit) =
   let name = "BillingCycle"
   let group = "Bank"

   let builder =
      TriggerBuilder
         .Create()
         .ForJob($"{name}Job", group)
         .WithIdentity($"{name}Trigger", group)
         .WithDescription(
            "Account transactions are consolidated at the end of a billing cycle."
         )

   if Env.isProd then
      logInfo
         "Scheduling billing cycle for the first day of the month at 12:13AM"

      builder.WithCronSchedule("0 0 13 L * ? *").Build()
   else
      let minutes = 15
      logInfo $"Scheduling billing cycle for every {minutes} minutes."

      builder
         .WithSimpleSchedule(fun s ->
            s.WithIntervalInMinutes(minutes).RepeatForever() |> ignore)
         .Build()
