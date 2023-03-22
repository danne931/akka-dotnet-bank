using LanguageExt;
using static LanguageExt.Prelude;
using static LanguageExt.List;
using Echo;
using static Echo.Process;
using System.Collections.Immutable;
using EventStore.Client;
using OneOf;

using Lib;
using Lib.Types;
using static Lib.Validators;
using ES = Lib.Persistence.EventStoreManager;
using AD = Bank.Account.Domain.Account;
using Bank.Account.Domain;
using static Bank.Account.Domain.Errors;
using Bank.Transfer.API;
using Bank.Transfer.Domain;

namespace Bank.Account.API;

public static class AccountAPI {
   public static TryAsync<Validation<Err, Guid>> Create(
      EventStoreClient client,
      AccountRegistry accounts,
      Validator<CreateAccountCmd> validate,
      ImmutableDictionary<string, Type> mapping,
      CreateAccountCmd command
   ) {
      var save = async (CreateAccountCmd cmd) => {
         var evt = cmd.ToEvent();
         await ES.SaveAndPublish(
            client,
            AD.EventTypeMapping,
            AD.StreamName(evt.EntityId),
            evt,
            // Create event stream only if it doesn't already exist.
            StreamState.NoStream
         );
         return Pass<CreatedAccount>()(evt);
      };
      var registerAccount = (CreatedAccount evt) => {
         var account = AD.Create(evt);
         return accounts
            .Register(account)
            .ToValidation(new Err("Account registration fail"))
            .AsTask();
      };
      var scheduleMaintenanceFee = (CreatedAccount evt) =>
         TaskSucc(Pass<ProcessId>()(ScheduleMaintenanceFee(evt, client, mapping)));

      var res =
         from cmd in TaskSucc(validate(command))
         from evt in save(cmd)
         from _ in registerAccount(evt)
         from pid in scheduleMaintenanceFee(evt)
         select evt.EntityId;
      return TryAsync(res);
   }

   public static Task<Option<Lst<object>>> GetAccountEvents(
   //public static TryOptionAsync<Lst<object>> GetAccountEvents(
      EventStoreClient client,
      Guid accountId,
      ImmutableDictionary<string, Type> mapping
   )
   => ES.ReadStream(client, AD.StreamName(accountId), mapping);

   public static async Task<Option<AccountState>> GetAccount(
      EventStoreClient client,
      Guid accountId,
      ImmutableDictionary<string, Type> mapping
   ) {
      var res = await GetAccountEvents(client, accountId, mapping);

      return res.Map(events =>
         fold(
            tail(events),
            AD.Create((CreatedAccount) head(events)),
            (state, evt) => state.Apply(evt)
         )
      );
   }

   public static Task<bool> Exists(EventStoreClient es, Guid id) =>
      ES.Exists(es, AD.StreamName(id));

   public static TryAsync<Validation<Err, Unit>> ProcessCommand<T>(
      T command,
      AccountRegistry accounts,
      OneOf<Validator<T>, AsyncValidator<T>> validate
   )
   where T : Command
   {
      var validation = validate.Match(
         validate => TaskSucc(validate(command)),
         asyncValidate => asyncValidate(command)
      );

      var getAccountVal = (Command cmd) =>
         accounts
            .Lookup(cmd.EntityId)
            .Map(opt => opt.ToValidation(UnknownAccountId(cmd.EntityId)));

      var res =
         from cmd in validation
         from acc in getAccountVal(cmd)
         select acc.SyncStateChange(cmd);

      return TryAsync(res);
   }

   public static Task<Unit> SoftDeleteEvents(
      AccountRegistry accounts,
      EventStoreClient client,
      Guid accountId
   ) {
      accounts.Delete(accountId);
      return ES.SoftDelete(client, AD.StreamName(accountId));
   }

   public static async Task<Unit> SaveAndPublish(
      EventStoreClient esClient,
      Event evt
   ) {
      await ES.SaveAndPublish(
         esClient,
         AD.EventTypeMapping,
         AD.StreamName(evt.EntityId),
         evt
      );

      if (evt is DebitedTransfer)
         BankTransferAPI.IssueTransferToRecipient((DebitedTransfer) evt);

      return unit;
   }

   public static ProcessId ScheduleMaintenanceFee(
      CreatedAccount evt,
      EventStoreClient esClient,
      ImmutableDictionary<string, Type> mapping
   ) {
      var pid = spawn<CreatedAccount>(
         $"monthly_maintenance_fee_{evt.EntityId}",
         async evt => {
            Console.WriteLine($"Monthly maintenance fee: {evt.EntityId}");
            var eventsOpt = await GetAccountEvents(esClient, evt.EntityId, mapping);

            eventsOpt.IfSome(events => {
               var date30DaysAgo = DateTime.UtcNow.AddDays(-30);

               var res = foldUntil(
                  events,
                  (
                     depositCriteria: false,
                     balanceCriteria: true,
                     account: AD.Create((CreatedAccount) head(events))
                  ),
                  (acc, evt) => {
                     acc.account = acc.account.Apply(evt);

                     if (((Event) evt).Timestamp < date30DaysAgo) {
                        acc.balanceCriteria = acc.account.Balance
                           >= AD.MonthlyMaintenanceFee.DailyBalanceThreshold;
                        return acc;
                     }

                     // Account balance must meet the balance threshold every day
                     // for the last month in order to skip the monthly fee.
                     if (acc.account.Balance < AD.MonthlyMaintenanceFee.DailyBalanceThreshold)
                        acc.balanceCriteria = false;

                     if (evt is DepositedCash &&
                         ((DepositedCash) evt).DepositedAmount >= AD.MonthlyMaintenanceFee.QualifyingDeposit
                     ) acc.depositCriteria = true;

                     return acc;
                  },
                  // If qualifying deposit found -> quit folding early.
                  tup => tup.depositCriteria
               );

               if (res.depositCriteria || res.balanceCriteria) {
                  Console.WriteLine(
                     "Some criteria met for skipping the monthly maintenance fee: " +
                     $"Deposit ({res.depositCriteria}) / Balance ({res.balanceCriteria})");
                  return;
               }

               tell($"@accounts_{evt.EntityId}", new DebitCmd(
                  evt.EntityId,
                  DateTime.UtcNow,
                  AD.MonthlyMaintenanceFee.Amount,
                  Origin: AD.MonthlyMaintenanceFee.Origin
               ));
            });

            tellSelf(evt, TimeSpan.FromDays(30));
         }
      );

      tell(pid, evt, TimeSpan.FromDays(30));
      return pid;
   }
}