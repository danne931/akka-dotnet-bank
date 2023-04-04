using Echo;
using static Echo.Process;
using LanguageExt;
using static LanguageExt.Prelude;
using static LanguageExt.List;
using static System.Console;

using Lib.Types;
using Bank.Account.Domain;
using AD = Bank.Account.Domain.Account;

namespace Bank.Account.Actors;

public static class MaintenanceFeeActor {
   public const string ActorName = "monthly_maintenance_fee";

   public static ProcessId Start(
      Func<Guid, Task<Option<Lst<object>>>> getAccountEvents,
      Func<DateTime> lookBackDate,
      Func<TimeSpan> scheduledAt,
      Guid accountId
   ) {
      var pid = spawn<Guid>(
         ActorName,
         async evt => {
            WriteLine($"Monthly maintenance fee: {accountId}");
            var eventsOpt = await getAccountEvents(accountId);

            eventsOpt.IfSome(events => {
               var lookback = lookBackDate();

               var res = foldUntil(
                  events,
                  (
                     depositCriteria: false,
                     balanceCriteria: true,
                     account: AD.Create((CreatedAccount) head(events))
                  ),
                  (acc, evt) => {
                     acc.account = acc.account.Apply(evt);

                     if (((Event) evt).Timestamp < lookback) {
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
                  WriteLine(
                     "Some criteria met for skipping the monthly maintenance fee: " +
                     $"Deposit ({res.depositCriteria}) / Balance ({res.balanceCriteria})");
                  return;
               }

               tellParent(new DebitCmd(
                  accountId,
                  DateTime.UtcNow,
                  AD.MonthlyMaintenanceFee.Amount,
                  Origin: AD.MonthlyMaintenanceFee.Origin
               ));
            });

            tellSelf(evt, scheduledAt());
         }
      );

      tell(pid, accountId, scheduledAt());
      return pid;
   }
}