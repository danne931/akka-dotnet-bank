module TransactionCategoryProvider

open Feliz

let context =
   React.createContext<Map<int, Transaction.TransactionCategory>> (
      name = "TransactionCategoryContext",
      defaultValue = Map.empty
   )

[<ReactComponent>]
let TransactionCategoryProvider (child: Fable.React.ReactElement) =
   let categories, setCategories = React.useState Map.empty

   React.useEffectOnce (fun () ->
      async {
         match! TransactionService.getCategories () with
         | Ok cats -> setCategories cats
         | Error err ->
            Log.error $"Error fetching transaction categories: {err}"
      }
      |> Async.StartImmediate)

   React.contextProvider (context, categories, child)
