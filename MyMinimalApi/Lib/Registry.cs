using System.Collections.Immutable;
//using LanguageExt;
//using static LanguageExt.Prelude;

using LaYumba.Functional;
using static LaYumba.Functional.F;
//using Unit = System.ValueTuple;

using Account.Domain;
using Account.Domain.Events;

namespace Lib;

//using AccountsCache = Map<Guid, AccountProcess>;
using AccountsCache = ImmutableDictionary<Guid, AccountProcess>;

/*
public class AccountRegistry
{
   LaYumba.Functional.Agent<Msg, LaYumba.Functional.Option<AccountProcess>> agent;
   Func<Guid, Task<LaYumba.Functional.Option<AccountState>>> loadAccount;

   abstract record Msg(Guid Id);
   record LookupMsg(Guid Id) : Msg(Id);
   record RegisterMsg(Guid Id, AccountState AccountState) : Msg(Id);

   public AccountRegistry(
      Func<Guid, Task<LaYumba.Functional.Option<AccountState>>> loadAccount,
      //Func<AccountState, Command, Validation<(Event Event, AccountState NewState)>> stateTransition,
      Func<Event, Task<Unit>> saveAndPublish
   )
   {
      Console.WriteLine("1. START Account Registry ctor");
      this.loadAccount = loadAccount;

      this.agent = LaYumba.Functional.Agent.Start
      (
         initialState: AccountsCache.Empty,
         process: (AccountsCache cache, Msg msg) => msg switch
         {
            LookupMsg m => (cache, cache.Find(m.Id)),

            RegisterMsg m => cache.Find(m.Id).Match
            (
               // Edge case in which 2 concurrent requests have both
               // loaded the account state.
               //LanguageExt.Prelude.Some<AccountProcess>(acc)
               Some: acc => (cache, acc),

               None: () =>
               {
                  Console.WriteLine("2. REGISTER MSG & add to cache:" + m);
                  AccountProcess account = new(m.AccountState, saveAndPublish);
                  return (cache.Add(m.Id, account), account);
               }
            )
         }
      );
   }

   public Task<LaYumba.Functional.Option<AccountProcess>> Lookup(Guid id)
   => agent
      .Tell(new LookupMsg(id))
      .OrElse(() => 
         from state in loadAccount(id) // loading the state is done in the calling thread
         from account in agent.Tell(new RegisterMsg(id, state))
         select account);
}
*/
/*
public class AccountRegistry
{
   Agent<Msg, Option<AccountProcess>> agent;
   Func<Guid, Task<Option<AccountState>>> loadAccount;

   abstract record Msg(Guid Id);
   record LookupMsg(Guid Id) : Msg(Id);
   record RegisterMsg(Guid Id, AccountState AccountState) : Msg(Id);

   public AccountRegistry(
      Func<Guid, Task<Option<AccountState>>> loadAccount,
      Func<AccountState, Command, Validation<(Event Event, AccountState NewState)>> stateTransition,
      Func<Event, Task<Unit>> saveAndPublish
   )
   {
      Console.WriteLine("1. START Account Registry ctor");
      this.loadAccount = loadAccount;

      this.agent = Agent.Start
      (
         initialState: AccountsCache.Empty,
         process: (AccountsCache cache, Msg msg) => msg switch
         {
            LookupMsg m => (cache, cache.Lookup(m.Id)),

            RegisterMsg m => cache.Lookup(m.Id).Match
            (
               // Edge case in which 2 concurrent requests have both
               // loaded the account state.
               Some: acc => (cache, Some(acc)),

               None: () =>
               {
                  Console.WriteLine("2. REGISTER MSG & add to cache:" + m);
                  AccountProcess account = new(m.AccountState, stateTransition, saveAndPublish);
                  return (cache.Add(m.Id, account), Some(account));
               }
            )
         }
      );
   }

   public Task<Option<AccountProcess>> Lookup(Guid id)
   => agent
      .Tell(new LookupMsg(id))
      .OrElse(() => 
         from state in loadAccount(id) // loading the state is done in the calling thread
         from account in agent.Tell(new RegisterMsg(id, state))
         select account);
}
*/
/*
public class AccountRegistry<T> where T : AgentProcess, new()
{
   Agent<Msg, Option<T>> agent;
   Func<Guid, Task<Option<AccountState>>> loadAccount;

   abstract record Msg(Guid Id);
   record LookupMsg(Guid Id) : Msg(Id);
   record RegisterMsg(Guid Id, AccountState AccountState) : Msg(Id);

   public AccountRegistry(
      ImmutableDictionary<Guid, T> Cache,
      Func<Guid, Task<Option<AccountState>>> loadAccount,
      Func<Event, Task<Unit>> saveAndPublish
   )
   {
      Console.WriteLine("1. START Account Registry ctor");
      this.loadAccount = loadAccount;

      this.agent = Agent.Start
      (
         initialState: Cache,
         process: (ImmutableDictionary<Guid, T> cache, Msg msg) => msg switch
         {
            LookupMsg m => (cache, cache.Lookup(m.Id)),

            RegisterMsg m => cache.Lookup(m.Id).Match
            (
               // Edge case in which 2 concurrent requests have both
               // loaded the account state.
               Some: acc => (cache, Some(acc)),

               None: () =>
               {
                  Console.WriteLine("2. REGISTER MSG & add to cache:" + m);
                  T account = (T) new AgentProcess(m.AccountState, saveAndPublish);
                  return (cache.Add(m.Id, account), Some(account));
               }
            )
         }
      );
   }

   public Task<Option<T>> Lookup(Guid id)
   => agent
      .Tell(new LookupMsg(id))
      .OrElse(() => 
         from state in loadAccount(id) // loading the state is done in the calling thread
         from account in agent.Tell(new RegisterMsg(id, state))
         select account);
}
*/


public class AccountRegistry
{
   Agent<Msg, Option<AccountProcess>> agent;
   Func<Guid, Task<Option<AccountState>>> loadAccount;

   abstract record Msg(Guid Id);
   record LookupMsg(Guid Id) : Msg(Id);
   record RegisterMsg(Guid Id, AccountState AccountState) : Msg(Id);

   public AccountRegistry(
      Func<Guid, Task<Option<AccountState>>> loadAccount,
      Func<Event, Task<LanguageExt.Unit>> saveAndPublish
   )
   {
      Console.WriteLine("1. START Account Registry ctor");
      this.loadAccount = loadAccount;

      this.agent = Agent.Start
      (
         initialState: AccountsCache.Empty,
         process: (AccountsCache cache, Msg msg) => msg switch
         {
            LookupMsg m => (cache, cache.Lookup(m.Id)),

            RegisterMsg m => cache.Lookup(m.Id).Match
            (
               // Edge case in which 2 concurrent requests have both
               // loaded the account state.
               Some: acc => (cache, Some(acc)),

               None: () =>
               {
                  Console.WriteLine("2. REGISTER MSG & add to cache:" + m);
                  AccountProcess account = new(m.AccountState, saveAndPublish);
                  Console.WriteLine("---accountprocess " + account);
                  return (cache.Add(m.Id, account), Some(account));
               }
            )
         }
      );
   }

   public Task<Option<AccountProcess>> Lookup(Guid id)
   => agent
      .Tell(new LookupMsg(id))
      .OrElse(() => 
         from state in loadAccount(id) // loading the state is done in the calling thread
         from account in agent.Tell(new RegisterMsg(id, state))
         select account);
}
/*
public class AccountRegistry
{
   LaYumba.Functional.Agent<Msg, Option<AccountProcess>> agent;
   Func<Guid, Task<LanguageExt.Option<AccountState>>> loadAccount;

   abstract record Msg(Guid Id);
   record LookupMsg(Guid Id) : Msg(Id);
   record RegisterMsg(Guid Id, AccountState AccountState) : Msg(Id);

   public AccountRegistry(
      Func<Guid, Task<LanguageExt.Option<AccountState>>> loadAccount,
      Func<Event, Task<LanguageExt.Unit>> saveAndPublish
   )
   {
      Console.WriteLine("1. START Account Registry ctor");
      this.loadAccount = loadAccount;

      this.agent = LaYumba.Functional.Agent.Start
      (
         initialState: AccountsCache.Empty,
         process: (AccountsCache cache, Msg msg) => msg switch
         {
            LookupMsg m => (cache, cache.Lookup(m.Id)),

            RegisterMsg m => cache.Lookup(m.Id).Match
            (
               // Edge case in which 2 concurrent requests have both
               // loaded the account state.
               Some: acc => (cache, acc),

               None: () =>
               {
                  Console.WriteLine("2. REGISTER MSG & add to cache:" + m);
                  AccountProcess account = new(m.AccountState, saveAndPublish);
                  return (cache.Add(m.Id, account), account);
               }
            )
         }
      );
   }

   public Task<Option<AccountProcess>> Lookup(Guid id)
   => agent
      .Tell(new LookupMsg(id))
      .OrElse(() => 
         from state in loadAccount(id) // loading the state is done in the calling thread
         //select state;
         from account in agent.Tell(new RegisterMsg(id, (AccountState) state))
         select account);
}
*/