namespace Bank.Infrastructure

open Microsoft.AspNetCore.Builder
open Serilog
open Serilog.Sinks.SystemConsole
open Serilog.Formatting.Compact

module LogInfra =
   let start (builder: WebApplicationBuilder) =
      // Initial logger for startup errors
      Log.Logger <- LoggerConfiguration().WriteTo.Console().CreateLogger()

      builder.Host.UseSerilog(fun ctx services loggerConfig ->
         loggerConfig.ReadFrom
            .Configuration(ctx.Configuration)
            .ReadFrom.Services(services)
            // TODO: Remove once guaranteed delivery consumer controller logging
            // on passivation fixed:
            // https://github.com/akkadotnet/akka.net/issues/7664#issuecomment-2991799919
            .Filter.ByExcluding(fun log ->
               log.Level = Events.LogEventLevel.Error
               && log.RenderMessage().Contains("DeathWatch"))
            .Enrich.FromLogContext()
            .WriteTo.Console(theme = Themes.AnsiConsoleTheme.Code)
            .WriteTo.File(CompactJsonFormatter(), Env.config.SerilogOutputFile)
         |> ignore)
