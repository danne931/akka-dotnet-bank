[<RequireQualifiedAccess>]
module Alerts

open Elmish.SweetAlert

open Lib.SharedTypes

let toast (err: Err) =
   ToastAlert(err.HumanFriendly)
      .Title("Error:")
      .Position(AlertPosition.BottomEnd)
      .Type(AlertType.Error)

let toastCommand (err: Err) : Elmish.Cmd<_> = toast err |> SweetAlert.Run

let toastSuccess (msg: string) =
   ToastAlert(msg)
      .Title("Success:")
      .Timeout(6000)
      .Position(AlertPosition.BottomEnd)
      .Type(AlertType.Success)

let toastSuccessCommand (msg: string) : Elmish.Cmd<_> =
   toastSuccess msg |> SweetAlert.Run
