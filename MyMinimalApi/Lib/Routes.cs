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
      //this Task<Validation<Error, T>> WrappedResult,
      //this Validation<InvalidCurrencyError, Task<Guid>> WrappedResult,
      //this Validation<InvalidCurrencyError, CreatedAccount> WrappedResult,
      this Task<TryOption<Guid>> WrappedResult,
      Func<Guid, Object>? ShapeResponse = null
   )
   {
      return /*(Task<IResult>)*/WrappedResult.Match(
         Fail: ExceptionResponse,
         None: () => BadRequest(),
         Some: state => Ok(ShapeResponse != null ? ShapeResponse(state) : state)
      );
   }
   /*
   => WrappedResult.BiMap(
         Fail: e => ExceptionResponse((Error)e),
         Success: val => val.Map(
            //Invalid: errs => BadRequest(new { Errors = errs }),
            /*
             * TODO: uncomment decide on uniform response validation
             * object shape
             */
            //Invalid: ErrorsAsValidationResponse,
            //Fail: BadRequest,
            //state =>  Ok(ShapeResponse != null ? ShapeResponse(state) : state)));

   public static Task<IResult> Unwrap<T>(
      this Task<Option<T>> WrappedResult,
      //this TryOptionAsync<T> WrappedResult,
      Func<T, Object>? ShapeResponse = null
   ) {
      //var r = Aff(async () => await WrappedResult);
      //var affWithMappedFailState = r.MapFail(error => Error.New(error.Message + "MapFail Aff"));

      //var fin = r.Run();
      //var r = Aff(async () => WrappedResult).Run();
      //var res = await WrappedResult;
      /*
      var r = await WrappedResult;
      var a = r.ToEitherAsync(ExceptionResponse);
      a.Match(
         Left: ExceptionResponse
      )
      */
      //WrappedResult.
      return WrappedResult.ToTryOptionAsync().Match(
         Fail: ExceptionResponse,
         None: () => NotFound(),
         Some: state => Ok(ShapeResponse != null ? ShapeResponse(state) : state)
      );
      //.None(NotFound)
      //.MapFail(ExceptionResponse);
      /*
      return await WrappedResult.Map(
         opt => opt.Match(
            None: () => NotFound(),
            Some: state => Ok(ShapeResponse != null ? ShapeResponse(state) : state)
         )
      );
      */
/*
      return r.Match(
         Fail: ExceptionResponse,
         Succ: val => val.Match(
            //Invalid: ErrorsAsValidationResponse,
            None: () => NotFound(),
            Some: state => Ok(ShapeResponse != null ? ShapeResponse(state) : state))
      );
      */
      /*
      match<Task<Option<T>>, LanguageExt.Common.Error, Task<IResult>>(WrappedResult,
         ExceptionResponse,
         val => val.Match(
            */
            //Invalid: errs => BadRequest(new { Errors = errs }),
            /*
             * TODO: uncomment decide on uniform response validation
             * object shape
             */
            //Invalid: ErrorsAsValidationResponse,
            /*
            None: () => NotFound(),
            Some: state => Ok(ShapeResponse != null ? ShapeResponse(state) : state)));

         fails: ExceptionResponse,
         succeeds: val => val.Match(*/
            //Invalid: errs => BadRequest(new { Errors = errs }),
            /*
             * TODO: uncomment decide on uniform response validation
             * object shape
             */
            //Invalid: ErrorsAsValidationResponse,
            /*
            None: () => NotFound(),
            Some: state => Ok(ShapeResponse != null ? ShapeResponse(state) : state)));
            */
   }

    private static IResult ErrorsAsValidationResponse(IEnumerable<Error> errors) =>
      ValidationProblem(errors.Aggregate(
         new Dictionary<string, string[]>(),
         (acc, val) => {
            acc[val.GetType().ToString()] = new string[]{val.Message};
            return acc;
         }
      ));

   private static IResult ExceptionResponse(Exception ex/*Error ex*/) {
      Console.WriteLine("EXCEPTION: " + ex.Message);//ex.InnerException);
      return StatusCode(StatusCodes.Status500InternalServerError);
   }
}