using Echo;
using static Echo.Process;
using LanguageExt;
using static LanguageExt.Prelude;

using Lib.Types;
using Bank.Account.Domain;

namespace Bank.Account.Actors;

using AccountsCache = Map<Guid, (AccountProcess process, DateTime date)>;

public class AccountRegistry {
   public ProcessId PID { get; init; }
   Func<Guid, Task<Option<AccountState>>> loadAccount;

   abstract record Msg(Guid Id);
   record LookupMsg(Guid Id) : Msg(Id);
   record RegisterMsg(Guid Id, AccountState AccountState) : Msg(Id);
   record DeleteMsg(Guid Id) : Msg(Id);

   public AccountRegistry(
      Func<Guid, Task<Option<AccountState>>> loadAccount,
      Func<Event, Task<Unit>> saveAndPublish,
      Func<Guid, Lst<ProcessId>> startChildActors
   ) {
      this.loadAccount = loadAccount;
     
      PID = spawn<AccountsCache, Msg>(
         "accounts",
         () => default(AccountsCache),
         (AccountsCache cache, Msg msg) => {
            switch (msg) {
               case LookupMsg m: {
                  Console.WriteLine("1. LOOKUP: " + m.Id);
                  reply(cache.Find(m.Id).Map(tup => tup.process));
                  return cache;
               }
               case RegisterMsg m: {
                  return cache.Find(msg.Id).Match(
                     // Edge case in which 2 concurrent requests have both
                     // loaded the account state.
                     Some: tup => {
                        Console.WriteLine("2. ++EDGE++");
                        reply(Some(tup.process));
                        return cache;
                     },
                     None: () => {
                        Console.WriteLine("2. REGISTER MSG & add to cache:" + m);

                        AccountProcess account = new(
                           m.AccountState,
                           saveAndPublish,
                           startChildActors
                        );

                        reply(Some(account));
                        
                        return cache.Add(m.Id, (account, DateTime.UtcNow));
                     }
                  );
               }
               case DeleteMsg m: {
                  var pid = $"accounts_{m.Id}";
                  kill("@" + pid);
                  Console.WriteLine($"Killed process {pid}");
                  return cache.Remove(m.Id);
               }
            }
            return cache;
         }
      );
   }

   public Task<Option<AccountProcess>> Lookup(Guid id) {
      var watch = System.Diagnostics.Stopwatch.StartNew();
      var account = ask<Option<AccountProcess>>(PID, new LookupMsg(id));
      watch.Stop();
      Console.WriteLine($"Execution Time: {watch.ElapsedMilliseconds} ms");

      if (account.IsSome) return TaskSucc(account);

      return
         from state in loadAccount(id)
         from acct in TaskSucc(Register(state))
         select acct;
   }

   public Option<AccountProcess> Register(AccountState account) {
      var process = ask<Option<AccountProcess>>(
         PID,
         new RegisterMsg(account.EntityId, account)
      );

      return process.Map(k => {
         register(k.PID.Name, k.PID);
         return k;
      });
   }

   public Unit Delete(Guid id) {
      Console.WriteLine($"Removing actor {id} from cache");
      tell(PID, new DeleteMsg(id));
      return unit;
   }
}