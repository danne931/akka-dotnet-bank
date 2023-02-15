using LaYumba.Functional;
using static LaYumba.Functional.F;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.DependencyInjection;

using Lib;
using Account.Routes;
using Account.Domain;
using Account.Domain.Commands;

namespace Account.Tests;

public class AccountTests
{
   [Fact]
   public async Task WhenAccountDoesntExist_Then400()
   {
      await using var app = MockApp(
         Valid,
         new AccountRegistry(
            loadAccount: _ => Async<Option<AccountState>>(None),
            saveAndPublish: _ => Async(Unit())));

      var client = app.CreateClient();
 
      var res = await client.PostAsJsonAsync(
         AccountRoutes.Path.Transfer,
         MockTransferCmd());

      var obj = await res.Content.
         ReadFromJsonAsync<IEnumerable<UnknownAccountIdError>>();

      /*
       * TODO: uncomment when uniform validation response object shape
       *       decided upon.  convert other tests as well
      var obj = await res.Content.ReadFromJsonAsync<HttpValidationProblemDetails>();
      Assert.NotEmpty(obj!.Errors[typeof(UnknownAccountIdError).ToString()]);
      */

      Assert.Equal(HttpStatusCode.BadRequest, res.StatusCode);
      Assert.IsType<UnknownAccountIdError>(obj?.ElementAt(0));
   }

   [Fact]
   public async Task WhenValidationFails_Then400()
   {
      var changesPersisted = false;

      await using var app = MockApp(
         Validators.TransferValidation(() => DateTime.UtcNow.Date),
         new AccountRegistry(
            loadAccount: _ => Async(Some(MockAccount())),
            saveAndPublish: _ =>
            {
               changesPersisted = true;
               return Async(Unit());
            }));

      var client = app.CreateClient();
 
      var res = await client.
         PostAsJsonAsync(AccountRoutes.Path.Transfer, MockTransferCmdInvalidDate());
      var obj = await res.Content.
         ReadFromJsonAsync<IEnumerable<TransferDateIsPastError>>();

      Assert.Equal(HttpStatusCode.BadRequest, res.StatusCode);
      Assert.IsType<TransferDateIsPastError>(obj?.ElementAt(0));
      Assert.False(changesPersisted);
   }

   [Fact]
   public async Task WhenInsufficientBalance_Then400()
   {
      bool changesPersisted = false;

      await using var app = MockApp(
         Valid,
         new AccountRegistry(
            loadAccount: _ => Async(Some(MockAccount())),
            saveAndPublish: _ =>
            {
               changesPersisted = true;
               return Async(Unit());
            }));

      var client = app.CreateClient();
 
      var res = await client.PostAsJsonAsync(
         AccountRoutes.Path.Transfer,
         MockTransferCmd() with { Amount = 1200 });
      var obj = await res.Content.
         ReadFromJsonAsync<IEnumerable<InsufficientBalanceError>>();

      Assert.Equal(HttpStatusCode.BadRequest, res.StatusCode);
      Assert.IsType<InsufficientBalanceError>(obj?.ElementAt(0));
      Assert.False(changesPersisted);
   }

   [Fact]
   public async Task WhenEverythingWorks_Then200()
   {
      bool changesPersisted = false;

      await using var app = MockApp(
         Valid,
         new AccountRegistry(
            loadAccount: _ => Async(Some(MockAccount())),
            saveAndPublish: _ =>
            {
               changesPersisted = true;
               return Async(Unit());
            }));

      var client = app.CreateClient();

      var res = await client.PostAsJsonAsync(
         AccountRoutes.Path.Transfer,
         MockTransferCmd());

      Assert.True(changesPersisted);
      Assert.Equal(HttpStatusCode.OK, res.StatusCode);
   }

   [Fact]
   public async Task WhenLoadingFails_Then500()
   {
      bool changesPersisted = false;

      await using var app = MockApp(
         Valid,
         new AccountRegistry(
            loadAccount: _ => { throw new InvalidOperationException(); },
            saveAndPublish: _ =>
            {
               changesPersisted = true;
               return Async(Unit());
            }));

      var client = app.CreateClient();

      var res = await client.PostAsJsonAsync(
         AccountRoutes.Path.Transfer,
         MockTransferCmd());

      Assert.False(changesPersisted); 
      Assert.Equal(HttpStatusCode.InternalServerError, res.StatusCode);
   }

   [Fact]
   public async Task AccountIsOnlyLoadedOnce()
   {
      int accountLoaded = 0;
      int changesPersisted = 0;

      await using var app = MockApp(
         Valid,
         new AccountRegistry(
            loadAccount: _ => {
               accountLoaded++;
               return Async(Some(MockAccount()));
            },
            saveAndPublish: _ =>
            {
               changesPersisted++;
               return Async(Unit());
            }));

      var client = app.CreateClient();
      var cmd = MockTransferCmd();

      // make 2 transfers
      await client.PostAsJsonAsync(AccountRoutes.Path.Transfer, cmd);
      await client.PostAsJsonAsync(AccountRoutes.Path.Transfer, cmd);

      Assert.Equal(2, changesPersisted);
      Assert.Equal(1, accountLoaded);
   }

   [Fact]
   public async Task WhenPersistenceFails_Then500()
   {
      await using var app = MockApp(
         Valid,
         new AccountRegistry(
            loadAccount: _ => Async(Some(MockAccount())),
            saveAndPublish: _ => { throw new InvalidOperationException(); }));

      var client = app.CreateClient();

      var res = await client.PostAsJsonAsync(
         AccountRoutes.Path.Transfer,
         MockTransferCmd());

      Assert.Equal(HttpStatusCode.InternalServerError, res.StatusCode);
   }

   private static WebApplicationFactory<Program> MockApp(
      Validator<TransferCmd> validate,
      AccountRegistry accounts
   )
   => new WebApplicationFactory<Program>()
         .WithWebHostBuilder(builder => builder
            .ConfigureServices(services =>
            {
               services.AddSingleton<Validator<TransferCmd>>(validate);
               services.AddSingleton<AccountRegistry>(accounts);
            }));

   private static AccountState MockAccount() =>
      new AccountState(
         EntityId: Guid.Empty,
         Currency: "USD",
         Balance: 500,
         Status: AccountStatus.Active);

   private static TransferCmd MockTransferCmd () =>
      new(
         Amount: 200,
         Date: DateTime.Now.AddDays(1),
         EntityId: default,
         Beneficiary: default,
         Iban: default,
         Bic: default,
         Reference: default
      );

   private static TransferCmd MockTransferCmdInvalidDate () =>
      MockTransferCmd() with { Date = DateTime.Now.AddDays(-1) };
}