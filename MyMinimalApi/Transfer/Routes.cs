using EventStore.Client;
using LanguageExt;

using Lib;
using Lib.Types;
using static Lib.Route.Response;
using Bank.Account.API;
using Bank.Transfer.Domain;
using Validators = Bank.Transfer.Domain.Validators;

namespace Bank.Transfer.Routes;

public static class TransferRoutes {
   public static class Path {
      public const string Base = "/transfers";
      public const string TransferRecipient = $"{Base}/register-recipient";
   }

   public static void Start(WebApplication app) {
      app.MapPost(Path.Base, Transfer);
      app.MapPost(Path.TransferRecipient, RegisterTransferRecipient);
   }

   static Task<IResult> RegisterTransferRecipient(
      RegisterTransferRecipientCmd cmd,
      EventStoreClient es,
      AccountRegistry accounts
   ) =>
      AccountAPI.ProcessCommand<RegisterTransferRecipientCmd>(
         cmd,
         accounts,
         asyncValidate: Validators.RegisterTransferRecipient(es)
      )
      .Unwrap<Unit>();

   public static Task<IResult> Transfer(
      TransferCmd cmd,
      Validator<TransferCmd> validate,
      AccountRegistry accounts
   )
   => AccountAPI
      .ProcessCommand<TransferCmd>(cmd, accounts, validate)
      .Unwrap<Unit>();
}