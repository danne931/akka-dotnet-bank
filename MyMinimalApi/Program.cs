using Account.Routes;
/*
using static Echo.Process;
using static LanguageExt.Prelude;

Echo.ProcessConfig.initialise();
Echo.ProcessId ping;
Echo.ProcessId pong;

var logger = spawn<string>("logger", Console.WriteLine);

// Ping process
ping = spawn<string>("ping", msg => {
   tell(logger, msg);
   tell(pong, "ping", 100*ms);
});

// Pong process
pong = spawn<string>("pong", msg => {
   tell(logger, msg);
   tell(ping, "pong", 100*ms);
});

// Trigger
tell(pong, "start");
*/

var builder = WebApplication.CreateBuilder(args);

AccountRoutes.Configure(builder);

var app = builder.Build();

app.MapGet("/", () => "Hello World!");

AccountRoutes.Start(app);

app.Run();

public partial class Program {}