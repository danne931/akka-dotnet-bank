using Echo;
using static Echo.Process;
using LanguageExt;
using static LanguageExt.Prelude;
using static LanguageExt.List;

using Lib.Types;
using Bank.Account.Domain;
using AD = Bank.Account.Domain.Account;

namespace Bank.Account.Actors;

public static class MaintenanceFeeActor {
   public static ProcessId ScheduleMaintenanceFee(
      Func<Guid, Task<Option<Lst<object>>>> getAccountEvents,
      Func<DateTime> lookBackDate,
      Func<TimeSpan> scheduledAt,
      CreatedAccount evt
   ) {
      var pid = spawn<CreatedAccount>(
         $"{AD.MonthlyMaintenanceFee.ActorName}_{evt.EntityId}",
         async evt => {
            Console.WriteLine($"Monthly maintenance fee: {evt.EntityId}");
            var eventsOpt = await getAccountEvents(evt.EntityId);

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

            tellSelf(evt, scheduledAt());
         }
      );

      tell(pid, evt, scheduledAt());
      register(pid.Name, pid);

      Console.WriteLine("monthly maintenance parent? " + pid.Parent);

      Console.WriteLine("monthly maintenance path? " + pid.Path);
      return pid;
   }
}