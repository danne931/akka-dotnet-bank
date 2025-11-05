module Nickname

open Feliz
open Feliz.UseElmish
open Elmish

open Lib.SharedTypes

type State = {
   PendingNickname: NonEmptyString option
   NicknamePersistence: Deferred<Result<unit, Err>>
}

type Msg =
   | SetPendingNickname of NonEmptyString option
   | SaveNickname of
      nickname: NonEmptyString option *
      AsyncOperationStatus<Result<unit, Err>>

let init (alias: NonEmptyString option) () =
   {
      PendingNickname = alias
      NicknamePersistence = Deferred.Idle
   },
   Cmd.none

let update
   (closeEdit: unit -> unit)
   (persistNickname: NonEmptyString option -> Async<Result<unit, Err>>)
   (onNicknameSaved: NonEmptyString option -> unit)
   msg
   state
   =
   match msg with
   | SetPendingNickname nickname ->
      {
         state with
            PendingNickname = nickname
      },
      Cmd.none
   | SaveNickname(nickname, Started) ->
      let save = async {
         let! res = persistNickname nickname
         return SaveNickname(nickname, Finished res)
      }

      {
         state with
            NicknamePersistence = Deferred.InProgress
      },
      Cmd.fromAsync save
   | SaveNickname(nickname, Finished(Ok _)) ->
      onNicknameSaved nickname
      closeEdit ()

      {
         state with
            NicknamePersistence = Deferred.Idle
      },
      Cmd.none
   | SaveNickname(_, Finished(Error err)) ->
      {
         state with
            NicknamePersistence = Deferred.Resolved(Error err)
      },
      Alerts.toastCommand err

let private nicknameCancelButton onCancel =
   Html.a [
      attr.href ""
      attr.text "Cancel"
      attr.style [ style.padding 10 ]
      attr.classes [ "secondary" ]
      attr.onClick (fun e ->
         e.preventDefault ()
         onCancel ())
   ]

[<ReactComponent>]
let NicknameEditComponent
   (props:
      {|
         OriginalName: NonEmptyString
         Alias: NonEmptyString option
         persistNickname: NonEmptyString option -> Async<Result<unit, Err>>
         onNicknameSaved: NonEmptyString option -> unit
         onCancel: unit -> unit
      |})
   =
   let alias = props.Alias
   let originalName = props.OriginalName

   let update =
      update props.onCancel props.persistNickname props.onNicknameSaved

   let state, dispatch = React.useElmish (init alias, update, [||])

   let pendingNickname = state.PendingNickname

   let nicknameInputRef = React.useInputRef ()

   React.useEffectOnce (fun () ->
      match nicknameInputRef.current with
      | None -> ()
      | Some input -> input.focus ())

   classyNode Html.div [ "nickname" ] [
      Html.input [
         attr.ref nicknameInputRef
         attr.ariaLabel "Nickname"
         attr.type' "text"
         attr.placeholder "Edit nickname"

         attr.value (
            pendingNickname |> Option.map _.Value |> Option.defaultValue ""
         )

         attr.onChange (fun input ->
            NonEmptyString.create input
            |> Result.toOption
            |> Msg.SetPendingNickname
            |> dispatch)
      ]

      match pendingNickname, alias with
      | None, None -> Html.small $"No change from original name {originalName}."
      | None, Some _ ->
         Html.small
            $"Transactions will display with the original name {originalName} for past and future transactions."
      | Some pending, _ ->
         if pending = originalName then
            Html.small $"No change from original name {originalName}."
         elif (Some pending) = alias then
            Html.small $"No change from nickname {alias}"
         else
            Html.small
               $"Transactions for {originalName} will display as {pending} for past and future transactions."

      classyNode Html.div [ "nickname-controls" ] [
         if
            pendingNickname = (Some originalName) || pendingNickname = alias
         then
            nicknameCancelButton props.onCancel
         else
            nicknameCancelButton props.onCancel

            Html.a [
               attr.href ""
               attr.text "Save"
               attr.style [ style.padding 10 ]
               attr.onClick (fun e ->
                  e.preventDefault ()

                  Msg.SaveNickname(
                     pendingNickname
                     |> Option.map (NonEmptyString.map _.Trim()),
                     Started
                  )
                  |> dispatch)
            ]
      ]
   ]
