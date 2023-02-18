//using LaYumba.Functional;
using LanguageExt;
using static LanguageExt.Prelude;
using LanguageExt.ClassInstances;
using static Microsoft.AspNetCore.Http.Results;
using LanguageExt.Common;

using Account.Domain;
using Account.Domain.Events;

namespace Lib.Route;

public static class Response
{
   public static Task<IResult> Unwrap<T>(
      this TryAsync<Validation<Account.Domain.Error, T>> WrappedResult,
      Func<T, Object>? ShapeResponse = null
   )
   =>
   WrappedResult.Match(
      Fail: ExceptionResponse,
      Succ: val => val.Match(
         Fail: e => BadRequest(e.ToFullString()),
         Succ: state => Ok(ShapeResponse != null ? ShapeResponse(state) : state)
      )
   );


   public static Task<IResult> Unwrap<T>(
      //this Task<Validation<Error, T>> WrappedResult,
      //this Validation<InvalidCurrencyError, Task<Guid>> WrappedResult,
      //this Validation<InvalidCurrencyError, CreatedAccount> WrappedResult,
      this Task<TryOption<T>> WrappedResult,
      Func<T, Object>? ShapeResponse = null
   )
   {
      return WrappedResult.Match(
         Fail: ExceptionResponse,
         None: () => BadRequest(),
         Some: state => Ok(ShapeResponse != null ? ShapeResponse(state) : state)
      );
   }

   public static Task<IResult> Unwrap<T>(
      this Task<Option<T>> WrappedResult,
      //this TryOptionAsync<T> WrappedResult,
      Func<T, Object>? ShapeResponse = null
   )
   {
      return WrappedResult.ToTryOptionAsync().Match(
         Fail: ExceptionResponse,
         None: () => NotFound(),
         Some: state => Ok(ShapeResponse != null ? ShapeResponse(state) : state)
      );

/*
      return r.Match(
         Fail: ExceptionResponse,
         Succ: val => val.Match(
            //Invalid: ErrorsAsValidationResponse,
            None: () => NotFound(),
            Some: state => Ok(ShapeResponse != null ? ShapeResponse(state) : state))
      );
      */
   }

    private static IResult ErrorsAsValidationResponse(IEnumerable<Account.Domain.Error> errors) =>
      ValidationProblem(errors.Aggregate(
         new Dictionary<string, string[]>(),
         (acc, val) => {
            //acc[val.GetType().ToString()] = new string[]{val.Message};
            acc[val.GetType().ToString()] = new string[]{val.ToString()};
            return acc;
         }
      ));

   private static IResult ExceptionResponse(Exception ex/*Error ex*/) {
      Console.WriteLine("EXCEPTION: " + ex.Message);//ex.InnerException);
      return StatusCode(StatusCodes.Status500InternalServerError);
   }
}