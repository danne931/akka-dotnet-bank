using LanguageExt;
using static LanguageExt.Prelude;
using Echo;
using static Echo.Process;
using static System.Console;

using Lib.Types;
using Bank.Account.Domain;

namespace Bank.Account.Actors;

public class AccountProcess {
   public ProcessId PID { get; init; }
   public record StartChildrenCmd(Guid id) : Command(id);

   public AccountProcess(
      AccountState initialState,
      Func<Event, Task<Unit>> saveAndPublish,
      Func<Guid, Lst<ProcessId>> startChildActors
   ) {
      PID = spawn<AccountState, Command>(
         $"accounts_{initialState.EntityId}",
         () => initialState,
         (AccountState account, Command cmd) => {
            WriteLine("account path " + PID.Path);
            WriteLine("account parent " + PID.Parent);

            if (cmd is StartChildrenCmd) {
               var pids = startChildActors(account.EntityId);
               WriteLine($"Account: Started child actors {pids}");
               return account;
            }

            WriteLine($"3. ACCOUNTPROCESS: compute state change from command: {cmd.EntityId}");
            var validation = account.StateTransition(cmd);

            // Persist within block, so that the agent doesn't process
            // new messages in a non-persisted state.
            // If the result is Valid, we proceed to save & publish
            return validation.Match(
               Fail: errs => {
                  WriteLine($"4. Validation Fail: {errs.Head.Message}");
                  return account;
               },
               Succ: tup => {
                  WriteLine($"4. ACCOUNTPROCESS: state transitioned {cmd.EntityId}");
                  saveAndPublish(tup.Event).Wait();
                  return tup.NewState;
               }
            );
         }
      );
      tell(PID, new StartChildrenCmd(initialState.EntityId));
   }

   // Commands are queued & processed sequentially.
   public Unit SyncStateChange(Command cmd) {
      WriteLine($"SYNC STATE for process {PID}: {cmd}");
      tell(PID, cmd);
      return unit;
   }
}