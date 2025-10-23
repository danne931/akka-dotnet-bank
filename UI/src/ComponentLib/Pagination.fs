module Pagination

open Feliz
open Fable.FontAwesome
open Elmish

open Lib.SharedTypes

type Page = int

type Config<'Cursor, 'T> = {
   PageLimit: int
   loadPage: 'Cursor option -> Async<Result<'T list option, Err>>
   onLoadError: Page -> Err -> unit
   /// I give you the last item in the page and you
   /// derive the Cursor from that item.
   getCursor: 'T -> 'Cursor
}

type CursorState<'Cursor> = {
   Cursor: 'Cursor option
   HasMore: bool
}

type State<'Cursor, 'T> = {
   Page: Page
   Cursors: Map<Page, CursorState<'Cursor>>
   Items: Map<Page, Deferred<Result<'T list option, Err>>>
}

type Msg<'T> =
   | LoadPage of Page * AsyncOperationStatus<Result<'T list option, Err>>
   | SetPageItems of Page * 'T list option
   | Reset

let init<'Cursor, 'T> () : State<'Cursor, 'T> * Cmd<Msg<'T>> =
   {
      Page = 1
      Cursors = Map.empty
      Items = Map [ 1, Deferred.Idle ]
   },
   Cmd.ofMsg (LoadPage(1, Started))

let update<'Cursor, 'T> (config: Config<'Cursor, 'T>) msg state =
   match msg with
   | LoadPage(page, Started) ->
      let state = { state with Page = page }

      // If we have visited this page, then do not refetch the data.
      // Otherwise fetch the data from the previous page's cursor.
      match state.Cursors.TryFind page with
      | Some _ -> state, Cmd.none
      | None ->
         let prevCursor =
            state.Cursors |> Map.tryFind (page - 1) |> Option.bind _.Cursor

         let cmd =
            async {
               let! result = config.loadPage prevCursor
               return LoadPage(page, Finished result)
            }
            |> Cmd.fromAsync

         state, cmd
   | LoadPage(page, Finished(Ok(Some items))) ->
      let cursorState = {
         Cursor = items |> List.tryLast |> Option.map config.getCursor
         HasMore = items.Length >= config.PageLimit
      }

      let state = {
         state with
            Cursors = Map.add page cursorState state.Cursors
            Items = Map.add page (Deferred.Resolved(Ok(Some items))) state.Items
      }

      state, Cmd.none
   | LoadPage(page, Finished(Ok None)) ->
      let cursorState = { Cursor = None; HasMore = false }

      let state = {
         state with
            Cursors = Map.add page cursorState state.Cursors
            Items = Map.add page (Deferred.Resolved(Ok None)) state.Items
      }

      state, Cmd.none
   | LoadPage(page, Finished(Error err)) ->
      config.onLoadError page err

      let state = {
         state with
            Items = Map.add page (Deferred.Resolved(Error err)) state.Items
      }

      state, Alerts.toastCommand err
   | Reset ->
      let page = 1

      let cmd =
         async {
            let! result = config.loadPage None
            return LoadPage(page, Finished result)
         }
         |> Cmd.fromAsync

      {
         Page = page
         Cursors = Map.empty
         Items = Map [ page, Deferred.Idle ]
      },
      cmd
   | SetPageItems(page, items) ->
      {
         state with
            Items = state.Items |> Map.add page (Deferred.Resolved(Ok items))
      },
      Cmd.none

let render (state: State<'Cursor, 'T>) (dispatch: Msg<'T> -> unit) =
   let page = state.Page

   let hasNextPage =
      state.Cursors
      |> Map.tryFind page
      |> Option.map _.HasMore
      |> Option.defaultValue true

   React.fragment [
      Html.a [
         attr.children [ Fa.i [ Fa.Solid.CaretRight ] [] ]

         attr.href ""

         attr.classes [
            "pagination"
            if not hasNextPage then
               "secondary"
         ]

         if not hasNextPage then
            attr.ariaDisabled true

         attr.onClick (fun e ->
            e.preventDefault ()

            if hasNextPage then
               dispatch (LoadPage(page + 1, Started)))
      ]

      Html.a [
         attr.children [ Fa.i [ Fa.Solid.CaretLeft ] [] ]

         attr.href ""

         attr.classes [
            "pagination"
            if page = 1 then
               "secondary"
         ]

         if page = 1 then
            attr.ariaDisabled true

         attr.onClick (fun e ->
            e.preventDefault ()

            if page > 1 then
               dispatch (LoadPage(page - 1, Started)))
      ]
   ]
