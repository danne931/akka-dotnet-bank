open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open Akka.HealthCheck.Hosting
open Akka.HealthCheck.Hosting.Web
open Serilog

open Bank.User.Routes
open Bank.Account.Routes
open Bank.Transfer.Routes
open Bank.Hubs

let builder = EnvironmentConfig.builder

Config.startLogger builder

Config.enableDefaultHttpJsonSerialization builder

Config.startSignalR builder

Config.startQuartz builder

Config.startActorModel builder

Config.injectDependencies builder

builder.Services.AddEndpointsApiExplorer().AddSwaggerGen() |> ignore
builder.Services.WithAkkaHealthCheck(HealthCheckType.All) |> ignore

let app = builder.Build()

app.UseDefaultFiles() |> ignore
app.UseStaticFiles() |> ignore
app.UseSerilogRequestLogging() |> ignore

// Available at endpoint: /swagger/index.html
if app.Environment.IsDevelopment() then
   app.UseSwagger().UseSwaggerUI() |> ignore

app.MapHub<AccountHub>("/accountHub") |> ignore

// Available at endpoint: /healthz/akka
app.MapAkkaHealthCheckRoutes(
   optionConfigure =
      fun _ opt ->
         opt.ResponseWriter <-
            fun httpCtx healthReport ->
               Helper.JsonResponseWriter(httpCtx, healthReport)
)
|> ignore

startUserRoutes app
startTransferRoutes app
startAccountRoutes app

app.Run()
