using static Bank.Config;
using Bank.Account.Routes;
using Bank.Transfer.Routes;
using Bank.Hubs;

var builder = WebApplication.CreateBuilder(args);

builder.Services.AddSignalR();
builder.Services.AddRazorPages();
StartActorModel();
var es = StartEventStore(builder);

InjectDependencies(builder, es);

var app = builder.Build();

app.UseStaticFiles();
app.MapRazorPages();
app.MapHub<AccountHub>("/accountHub");

AccountRoutes.Start(app);
TransferRoutes.Start(app);

app.Run();

public partial class Program {}
