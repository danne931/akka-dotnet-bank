module Lib.Saga

open System

open Lib.SharedTypes

type IActivity =
   /// Max attempts allowed if retrying due to inactivity or failure.
   /// Use 0 to indicate that we should wait indefinitely for the activity
   /// (ex: wait for some user input before the saga can proceed)
   abstract member MaxAttempts: int
   /// Assumes the activity is scheduled or "stuck" if
   /// some specified time has elapsed since it was started.
   abstract member InactivityTimeout: TimeSpan option

type ActivityLifeCycle<'Activity when 'Activity :> IActivity> = {
   Start: DateTime
   End: DateTime option
   Activity: 'Activity
   MaxAttempts: int
   Attempts: int
} with

   static member init (timestamp: DateTime) (activity: 'Activity) = {
      Start = timestamp
      End = None
      Activity = activity
      MaxAttempts = activity.MaxAttempts
      Attempts = 1
   }

   member x.HasRemainingAttempts =
      x.MaxAttempts = 0 || (x.Attempts < x.MaxAttempts)

   member x.IsRetryableAfterInactivity =
      match x.Activity.InactivityTimeout with
      | None -> false
      | Some time ->
         x.HasRemainingAttempts && (x.Start + time) < DateTime.UtcNow

   member x.finish(timestamp: DateTime) = { x with End = Some timestamp }

   member x.retry(timestamp: DateTime) = {
      x with
         Start = timestamp
         Attempts = x.Attempts + 1
   }

   member x.resetAttempts() = { x with Attempts = 1 }

type SagaLifeCycle<'Activity when 'Activity :> IActivity and 'Activity: equality>
   = {
   InProgress: ActivityLifeCycle<'Activity> list
   Completed: ActivityLifeCycle<'Activity> list
   Failed: ActivityLifeCycle<'Activity> list
   Aborted: ActivityLifeCycle<'Activity> list
} with

   member x.ActivitiesWithAttemptsExhausted =
      x.InProgress |> List.filter (_.HasRemainingAttempts >> not)

   member x.ActivitiesRetryableAfterInactivity =
      x.InProgress |> List.filter _.IsRetryableAfterInactivity

   member x.ActivityHasRemainingAttempts(activity: 'Activity) =
      x.InProgress
      |> List.exists (fun life ->
         life.Activity = activity && life.HasRemainingAttempts)

   member x.ActivityIsInProgress(activity: 'Activity) =
      x.InProgress |> List.exists (fun life -> life.Activity = activity)

   member x.ActivityHasFailed(activity: 'Activity) =
      x.Failed |> List.exists (fun life -> life.Activity = activity)

   /// Indicates all in-progress activities in a Saga have been attempted
   /// MaxAttempts times.  Let the saga sleep & manually reset later.
   member x.SagaExhaustedAttempts =
      x.ActivitiesWithAttemptsExhausted.Length > 0
      && x.ActivitiesWithAttemptsExhausted.Length = x.InProgress.Length

   /// Corresponds to an activity, if any, which may be retried due to
   /// inactivity after some TimeSpan (ex: 3 minutes or 10 seconds).
   /// If None, indicates an activity which must wait for external stimulus
   /// rather than assuming the inactivity is an indicator of an action
   /// being "stuck" or scheduled.
   member x.InactivityTimeout =
      x.InProgress
      |> List.choose _.Activity.InactivityTimeout
      |> List.sort
      |> List.tryHead

   static member empty: SagaLifeCycle<'Activity> = {
      InProgress = []
      Completed = []
      Failed = []
      Aborted = []
   }

   static member addActivity
      (timestamp: DateTime)
      (activity: 'Activity)
      (state: SagaLifeCycle<'Activity>)
      =
      {
         state with
            InProgress =
               ActivityLifeCycle.init timestamp activity :: state.InProgress
      }

   static member finishActivity
      (timestamp: DateTime)
      (activity: 'Activity)
      (state: SagaLifeCycle<'Activity>)
      =
      let complete =
         state.InProgress
         |> List.tryFind (fun w -> w.Activity = activity)
         |> Option.map (fun w -> w.finish timestamp :: state.Completed)
         |> Option.defaultValue state.Completed

      let wip =
         state.InProgress |> List.filter (fun w -> w.Activity <> activity)

      {
         state with
            InProgress = wip
            Completed = complete
      }

   static member failActivity
      (timestamp: DateTime)
      (activity: 'Activity)
      (state: SagaLifeCycle<'Activity>)
      =
      let failed =
         state.InProgress
         |> List.tryFind (fun w -> w.Activity = activity)
         |> Option.map (fun w -> w.finish timestamp :: state.Failed)
         |> Option.defaultValue state.Failed

      let wip =
         state.InProgress |> List.filter (fun w -> w.Activity <> activity)

      {
         state with
            InProgress = wip
            Failed = failed
      }

   static member abortActivities
      (timestamp: DateTime)
      (state: SagaLifeCycle<'Activity>)
      =
      {
         state with
            InProgress = []
            Aborted = state.InProgress |> List.map (fun w -> w.finish timestamp)
      }

   static member retryActivity
      (timestamp: DateTime)
      (activity: 'Activity)
      (state: SagaLifeCycle<'Activity>)
      =
      let wip =
         state.InProgress
         |> List.map (fun w ->
            if w.Activity = activity then w.retry timestamp else w)

      { state with InProgress = wip }

   static member retryActivitiesAfterInactivity
      (timestamp: DateTime)
      (state: SagaLifeCycle<'Activity>)
      =
      {
         state with
            InProgress =
               state.InProgress
               |> List.map (fun activity ->
                  if activity.IsRetryableAfterInactivity then
                     activity.retry timestamp
                  else
                     activity)
      }

   static member resetInProgressActivities(state: SagaLifeCycle<'Activity>) = {
      state with
         InProgress =
            state.InProgress
            |> List.map (fun activity ->
               if activity.HasRemainingAttempts then
                  activity
               else
                  activity.resetAttempts ())
   }

type SagaEvent<'E> = {
   Id: EventId
   CorrelationId: CorrelationId
   OrgId: OrgId
   Timestamp: DateTime
   Data: 'E
} with

   static member create orgId corrId (data: 'E) : SagaEvent<'E> = {
      Id = Guid.NewGuid() |> EventId
      OrgId = orgId
      CorrelationId = corrId
      Timestamp = DateTime.UtcNow
      Data = data
   }

type IPersistableSagaEvent = interface end

[<RequireQualifiedAccess>]
type SagaPersistableEvent<'SagaStartEvent, 'SagaEvent> =
   | StartEvent of SagaEvent<'SagaStartEvent>
   | Event of SagaEvent<'SagaEvent>

   interface IPersistableSagaEvent

   member x.CorrelationId =
      match x with
      | StartEvent e -> e.CorrelationId
      | Event e -> e.CorrelationId

type ISaga =
   abstract member SagaId: CorrelationId
   abstract member OrgId: OrgId
   abstract member ActivityInProgressCount: int
   abstract member ActivityAttemptsExhaustedCount: int
   abstract member ActivityRetryableAfterInactivityCount: int
   abstract member ExhaustedAllAttempts: bool
   abstract member InactivityTimeout: TimeSpan option

[<RequireQualifiedAccess>]
type SagaMessage<'SagaStartEvent, 'SagaEvent> =
   | Start of SagaEvent<'SagaStartEvent>
   | Event of SagaEvent<'SagaEvent>
   | CheckForRemainingWork
   | ResetInProgressActivities
   | GetSaga

[<RequireQualifiedAccess>]
type SagaStateTransitionError =
   | ReceivedEventOfDifferentSagaType
   | HasNotStarted
   | HasAlreadyStarted
   | InvalidStepProgression
   | HasAlreadyCompleted
