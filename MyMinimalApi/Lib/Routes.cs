using LaYumba.Functional;
using static Microsoft.AspNetCore.Http.Results;

namespace Lib.Route;

public static class Response
{
   public static Task<IResult> Unwrap<T>(
      this Task<T> WrappedResult
   )
   => WrappedResult.Map(
      Faulted: ExceptionResponse,
      Completed: Ok 
   );

   public static Task<IResult> Unwrap<T>(
      this Task<Validation<T>> WrappedResult,
      Func<T, Object>? ShapeResponse = null
   )
   => WrappedResult.Map(
         Faulted: ExceptionResponse,
         Completed: val => val.Match(
            //Invalid: errs => BadRequest(new { Errors = errs }),
            /*
             * TODO: uncomment decide on uniform response validation
             * object shape
             */
            //Invalid: ErrorsAsValidationResponse,
            Invalid: BadRequest,
            Valid: state =>  Ok(ShapeResponse != null ? ShapeResponse(state) : state)));

   public static Task<IResult> Unwrap<T>(
      this Task<Option<T>> WrappedResult,
      Func<T, Object>? ShapeResponse = null
   )
   => WrappedResult.Map(
         Faulted: ExceptionResponse,
         Completed: val => val.Match(
            //Invalid: errs => BadRequest(new { Errors = errs }),
            /*
             * TODO: uncomment decide on uniform response validation
             * object shape
             */
            //Invalid: ErrorsAsValidationResponse,
            None: () => NotFound(),
            Some: state => Ok(ShapeResponse != null ? ShapeResponse(state) : state)));

    private static IResult ErrorsAsValidationResponse(IEnumerable<Error> errors) =>
      ValidationProblem(errors.Aggregate(
         new Dictionary<string, string[]>(),
         (acc, val) => {
            acc[val.GetType().ToString()] = new string[]{val.Message};
            return acc;
         }
      ));

   private static IResult ExceptionResponse(Exception ex) {
      Console.WriteLine("EXCEPTION: " + ex.InnerException);
      return StatusCode(StatusCodes.Status500InternalServerError);
   }
}