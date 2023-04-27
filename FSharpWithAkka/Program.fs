open Microsoft.AspNetCore.Builder
open Echo

open Bank.Account.Routes
open Bank.Transfer.Routes
open Bank.Hubs

let builder = WebApplication.CreateBuilder()

Config.startSignalR builder

Config.startActorModel ()
let es = Config.startEventStore builder

Config.injectDependencies builder es

let app = builder.Build()

app.UseDefaultFiles() |> ignore
app.UseStaticFiles() |> ignore

app.MapHub<AccountHub>("/accountHub") |> ignore

startTransferRoutes app es
startAccountRoutes app es

app.Run()

type Program() =
   class
   end
