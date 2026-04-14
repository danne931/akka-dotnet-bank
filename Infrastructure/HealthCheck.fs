module HealthCheck.App

open Akka.Hosting
open Akka.Cluster.Hosting

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Diagnostics.HealthChecks
open Microsoft.Extensions.Diagnostics.HealthChecks

(*
 * WithActorSystemLivenessCheck() registers a liveness check that returns
 * Unhealthy if the ActorSystem is terminated.
 *
 * You can implement custom health checks as well 
 * (ex: .WithHealthCheck("MyActor alive", ..))
 * See https://petabridge.com/blog/you-dont-need-akka-healthchecks-anymore/
 *)
let configureAkka (builder: AkkaConfigurationBuilder) =
   builder.WithActorSystemLivenessCheck().WithAkkaClusterReadinessCheck()

// Changing the default ResponseWriter to Json provides a more verbose
// response than the default string response of "healthy"/"unhealthy".
let private responseWriter (ctx: HttpContext) (report: HealthReport) =
   ctx.Response.WriteAsJsonAsync {|
      status = report.Status.ToString()
      totalDuration = report.TotalDuration
      checks = [
         for e in report.Entries ->
            {|
               name = e.Key
               status = string e.Value.Status
               duration = e.Value.Duration
               description = e.Value.Description
               tags = e.Value.Tags
               // anything you added via context.Registration
               data = e.Value.Data
            |}
      ]
   |}

/// Map health check endpoints:
/// - /healthz: All health checks
/// - /healthz/live: Only liveness health checks
/// - /healthz/ready: Only readiness health checks (Akka.Persistence)
let configureEndpoints (app: WebApplication) =
   // Available at: http://localhost:3000/healthz
   app.MapHealthChecks(
      "/healthz",
      HealthCheckOptions(
         Predicate = (fun _ -> true), // include all checks
         ResponseWriter = responseWriter
      )
   )
   |> ignore

   app.MapHealthChecks(
      "/healthz/live",
      HealthCheckOptions(
         Predicate = _.Tags.Contains("liveness"),
         ResponseWriter = responseWriter
      )
   )
   |> ignore

   app.MapHealthChecks(
      "/healthz/ready",
      HealthCheckOptions(
         Predicate = _.Name.Contains("Akka.Persistence"),
         ResponseWriter = responseWriter
      )
   )
   |> ignore
