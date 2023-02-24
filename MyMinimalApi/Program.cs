using Account.Routes;

var builder = WebApplication.CreateBuilder(args);

AccountRoutes.Configure(builder);

var app = builder.Build();

app.MapGet("/", () => "Hello World!");

AccountRoutes.Start(app);

app.Run();

public partial class Program {}