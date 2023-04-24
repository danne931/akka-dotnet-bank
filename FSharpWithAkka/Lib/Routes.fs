[<RequireQualifiedAccessAttribute>]
module RouteUtil

open System.Threading.Tasks
open type Microsoft.AspNetCore.Http.Results

let Unwrap (task: 'a Task) =
   Task.FromResult(
      if task.IsFaulted then
         Problem task.Exception.Message
      else
         Ok task.Result
   )

let UnwrapOption (task: 'a option Task) =
   Task.FromResult(
      if task.IsFaulted then
         Problem task.Exception.Message
      else
         match task.Result with
         | None -> NotFound()
         | Some(res) -> Json(res, Serialization.jsonOptions)
   )

let UnwrapValidation (task: Result<'a, string> Task) =
   Task.FromResult(
      if task.IsFaulted then
         Problem task.Exception.Message
      else
         match task.Result with
         | Error(e) -> BadRequest e
         | Ok(res) -> Json(res, Serialization.jsonOptions)
   )
