open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Microsoft.FSharp.Core
open Echo

open Bank.Account.Routes
open Bank.Transfer.Routes
open Bank.Hubs

let builder = WebApplication.CreateBuilder()
(*
builder.Services.ConfigureHttpJsonOptions(fun opts ->
   JsonFSharpOptions.Default().AddToJsonSerializerOptions(opts.SerializerOptions)
   ()) |> ignore
*)

builder.Services.AddSignalR() |> ignore
builder.Services.AddRazorPages() |> ignore

Config.startActorModel () |> ignore
let es = Config.startEventStore builder

Config.injectDependencies builder es

let app = builder.Build()

app.UseStaticFiles() |> ignore
app.MapRazorPages() |> ignore
app.MapHub<AccountHub>("/accountHub") |> ignore

startTransferRoutes app es
startAccountRoutes app es

app.Run()

type Program() =
   class
   end
