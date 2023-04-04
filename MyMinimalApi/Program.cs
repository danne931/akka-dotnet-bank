using static Bank.Config;
using Bank.Account.Routes;
using Bank.Transfer.Routes;

var builder = WebApplication.CreateBuilder(args);

StartActorModel();
var es = StartEventStore(builder);

InjectDependencies(builder, es);

var app = builder.Build();

AccountRoutes.Start(app);
TransferRoutes.Start(app);

app.Run();

public partial class Program {}
