using LanguageExt;
using static LanguageExt.Prelude;
using Echo;
using static Echo.Process;

using Lib.Types;
using Account.Domain;

namespace Lib;

public class AccountProcess {
   public ProcessId PID { get; init; }

   public AccountProcess(
      AccountState initialState,
      Func<Event, Task<LanguageExt.Unit>> saveAndPublish
   ) {
      PID = spawn<AccountState, Command>(
         $"accounts_{initialState.EntityId}",
         () => initialState,
         (AccountState account, Command cmd) => {
 
            Console.WriteLine($"3. ACCOUNTPROCESS: compute state change from command: {cmd.EntityId}");

            var result = account.StateTransition(cmd);
   
            Console.WriteLine("4. ACCOUNTPROCESS: state transitioned" + cmd.EntityId);

            // Persist within block, so that the agent doesn't process
            // new messages in a non-persisted state.
            // If the result is Valid, we proceed to save & publish
            // the created event (the check is done as part of Traverse)
            result.Map(tpl => saveAndPublish(tpl.Event)).Sequence().Wait();

            return result.Map(tpl => tpl.Success.NewState).Head();//.HeadOrLeft(state);//.GetOrElse(state);
         }
      );
   }

   // Commands are queued & processed sequentially. 
   public Unit SyncStateChange(Command cmd) {
      Console.WriteLine($"SYNC STATE for process {PID}: {cmd}");
      tell(PID, cmd);
      return unit;
   }
}