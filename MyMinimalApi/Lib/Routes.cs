using static Microsoft.AspNetCore.Http.Results;
using LanguageExt;

using Account.Domain;

namespace Lib.Route;

public static class Response
{
   public static Task<IResult> Unwrap<T>(
      this TryAsync<Validation<Err, T>> WrappedResult,
      Func<T, Object>? ShapeResponse = null
   )
   =>
   WrappedResult.Match(
      Fail: ExceptionResponse,
      Succ: val => val.Match(
         Fail: BadRequest,
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
   }

   private static IResult ExceptionResponse(Exception ex) {
      Console.WriteLine("EXCEPTION: " + ex.Message);
      return StatusCode(StatusCodes.Status500InternalServerError);
   }
}