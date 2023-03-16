using static Bank.Config;
using Bank.Account.Routes;
using Bank.Transfer.Routes;

StartActorModel();
var es = StartEventStore();

var builder = WebApplication.CreateBuilder(args);
InjectDependencies(builder, es);

var app = builder.Build();

AccountRoutes.Start(app);
TransferRoutes.Start(app);

app.Run();

public partial class Program {}