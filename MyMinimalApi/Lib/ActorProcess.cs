using LanguageExt;

using Account.Domain;
using Account.Domain.Commands;
using Account.Domain.Events;

namespace Lib;

using Result = LanguageExt.Validation<Err, (Event Event, AccountState NewState)>;

public class AgentProcess//<T>
{
   public AgentProcess(
      AccountState initialState,
      Func<Event, Task<LanguageExt.Unit>> saveAndPublish
   ) {}
}
/*
public class AccountProcess// : AgentProcess//<AccountProcess>
{
   Agent<Command, Result> agent;

   public AccountProcess(
      AccountState initialState,
      //Func<AccountState, Command, Validation<(Event Event, AccountState NewState)>> stateTransition,
      Func<Event, Task<LanguageExt.Unit>> saveAndPublish
   ) //: base(initialState, saveAndPublish)
   => agent = Agent.Start(
      initialState,
      async (AccountState state, Command cmd) =>
      {
         Console.WriteLine("3. ACCOUNTPROCESS: compute state change from command: " + cmd);
         // Uses pure fns to calculate the result of the command

         //Result*//*Validation<(Event Event, T NewState)> result = cmd switch
         Result result = cmd switch {
            TransferCmd transfer => state.Debit(transfer),
            DepositCashCmd deposit => state.Deposit(deposit),
            FreezeAccountCmd freeze => state.Freeze(freeze)
         };
 
         //Result result = state.StateTransition(cmd);//state.StateTransition(cmd);
 
         Console.WriteLine("4. ACCOUNTPROCESS: result of state change: " + result);

         // Persist within block, so that the agent doesn't process
         // new messages in a non-persisted state.
         // If the result is Valid, we proceed to save & publish
         // the created event (the check is done as part of Traverse)
         await result.Traverse(tpl => saveAndPublish(tpl.Event));

         var newState = result.Map(tpl => tpl.NewState).GetOrElse(state);
         Console.WriteLine("5. ACCOUNT PROCESS: newState: " + newState);
         return (newState, result);
      });

   // Commands are queued & processed sequentially. 
   public Task<Result> SyncStateChange(Command cmd) => agent.Tell(cmd);
}
*/
public class AccountProcess
{
   LaYumba.Functional.Agent<Command, Result> agent;

   public AccountProcess(
      AccountState initialState,
      Func<Event, Task<LanguageExt.Unit>> saveAndPublish
   )
   => agent = LaYumba.Functional.Agent.Start(
      initialState,
      async (AccountState state, Command cmd) =>
      {
         Console.WriteLine("3. ACCOUNTPROCESS: compute state change from command: " + cmd);
         // Uses pure fns to calculate the result of the command

         //Result*//*Validation<(Event Event, T NewState)> result = cmd switch
         Result result = cmd switch {
            TransferCmd transfer => state.Debit(transfer),
            DepositCashCmd deposit => state.Deposit(deposit),
            FreezeAccountCmd freeze => state.Freeze(freeze)
         };
 
         //Result result = state.StateTransition(cmd);//state.StateTransition(cmd);
 
         Console.WriteLine("4. ACCOUNTPROCESS: result of state change: " + result);

         // Persist within block, so that the agent doesn't process
         // new messages in a non-persisted state.
         // If the result is Valid, we proceed to save & publish
         // the created event (the check is done as part of Traverse)

         await result.Map(tpl => saveAndPublish(tpl.Event)).Sequence();

         var newState = result.Map(tpl => tpl.Success.NewState).Head();//.HeadOrLeft(state);//.GetOrElse(state);
         Console.WriteLine("5. ACCOUNT PROCESS: newState: " + newState);
         return (newState, result);
      });

   // Commands are queued & processed sequentially. 
   public Task<Result> SyncStateChange(Command cmd) {
      Console.WriteLine("SYNC STATE " + cmd);
      return agent.Tell(cmd);
   }
}