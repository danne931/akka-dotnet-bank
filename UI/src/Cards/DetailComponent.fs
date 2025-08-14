module CardDetail

open Feliz
open Feliz.UseElmish
open Feliz.Router
open Elmish
open System
open Fable.FontAwesome
open Elmish.SweetAlert

open Bank.Account.Domain
open Bank.Org.Domain
open UIDomain.Org
open UIDomain.Account
open UIDomain.Employee
open Bank.Employee.Domain
open Lib.SharedTypes
open Dropdown
open Bank.Employee.Forms.PurchaseLimitForm
open CommandApproval

type State = {
   NicknamePersistence: Deferred<Result<EmployeeCommandReceipt, Err>>
   LockPersistence: Deferred<Result<EmployeeCommandReceipt, Err>>
   IsEditingNickname: bool
   IsEditingPurchaseLimit: bool
}

type NicknameEditMsg = {
   CommandInput: CardNicknamed
   Employee: Employee
   InitiatedBy: Initiator
}

type CardLockMsg = {
   WillLock: bool
   Employee: Employee
   Card: Card
   InitiatedBy: Initiator
   UserSession: UserSession
   UnlockRequiresApproval: CommandApprovalRule option
}

type Msg =
   | ToggleNicknameEdit
   | TogglePurchaseLimitEdit
   | SaveNickname of
      NicknameEditMsg *
      AsyncOperationStatus<Result<EmployeeCommandReceipt, Err>>
   | ShowCardLockConfirmation of CardLockMsg
   | ConfirmUpdateLock of
      CardLockMsg *
      AsyncOperationStatus<Result<EmployeeCommandReceipt, Err>>
   | DismissUpdateLock

let init () =
   {
      NicknamePersistence = Deferred.Idle
      LockPersistence = Deferred.Idle
      IsEditingNickname = false
      IsEditingPurchaseLimit = false
   },
   Cmd.none

let update
   (onCardUpdate: EmployeeCommandReceipt -> unit)
   (orgDispatch: OrgProvider.Msg -> unit)
   msg
   state
   =
   match msg with
   | ToggleNicknameEdit ->
      {
         state with
            IsEditingNickname = not state.IsEditingNickname
      },
      Cmd.none
   | TogglePurchaseLimitEdit ->
      {
         state with
            IsEditingPurchaseLimit = not state.IsEditingPurchaseLimit
      },
      Cmd.none
   | SaveNickname(edit, Started) ->
      let command =
         EditCardNicknameCommand.create
            edit.Employee.CompositeId
            edit.InitiatedBy
            edit.CommandInput
         |> EmployeeCommand.EditCardNickname

      let submitCommand = async {
         let! res = EmployeeService.submitCommand edit.Employee command
         return Msg.SaveNickname(edit, Finished res)
      }

      {
         state with
            NicknamePersistence = Deferred.InProgress
      },
      Cmd.fromAsync submitCommand
   | SaveNickname(_, Finished(Ok receipt)) ->
      onCardUpdate receipt

      {
         state with
            IsEditingNickname = false
            NicknamePersistence = Deferred.Idle
      },
      Cmd.none
   | SaveNickname(_, Finished(Error err)) ->
      {
         state with
            NicknamePersistence = Deferred.Resolved(Error err)
      },
      Alerts.toastCommand err
   | ShowCardLockConfirmation msg ->
      let lockOrUnlock = if msg.WillLock then "lock" else "unlock"

      let title =
         let title =
            $"{lockOrUnlock} {msg.Card.Display} for {msg.Employee.Name}"

         if not msg.WillLock && msg.UnlockRequiresApproval.IsSome then
            $"Request approval to {title}"
         else
            $"This will {title}"

      let confirm =
         ConfirmAlert(
            title,
            function
            | ConfirmAlertResult.Confirmed ->
               Msg.ConfirmUpdateLock(msg, Started)
            | ConfirmAlertResult.Dismissed _ -> Msg.DismissUpdateLock
         )
            .Title($"Are you sure you want to {lockOrUnlock} this card?")
            .Type(AlertType.Question)
            .ShowCloseButton(true)

      state, SweetAlert.Run confirm
   | DismissUpdateLock -> state, Cmd.none
   | ConfirmUpdateLock(msg, Started) ->
      let command =
         if msg.WillLock then
            LockCardCommand.create msg.Employee.CompositeId msg.InitiatedBy {
               CardId = msg.Card.CardId
               CardName = msg.Card.CardNickname |> Option.defaultValue ""
               EmployeeName = msg.Employee.Name
               CardNumberLast4 = msg.Card.CardNumberLast4
               Reference = None
            }
            |> EmployeeCommand.LockCard
         else
            UnlockCardCommand.create msg.Employee.CompositeId msg.InitiatedBy {
               CardId = msg.Card.CardId
               CardName = msg.Card.CardNickname |> Option.defaultValue ""
               EmployeeName = msg.Employee.Name
               CardNumberLast4 = msg.Card.CardNumberLast4
               Reference = None
            }
            |> EmployeeCommand.UnlockCard

      let submitCommand = async {
         let! res = EmployeeService.submitCommand msg.Employee command
         return Msg.ConfirmUpdateLock(msg, Finished res)
      }

      {
         state with
            LockPersistence = Deferred.InProgress
      },
      Cmd.fromAsync submitCommand
   | ConfirmUpdateLock(msg, Finished(Ok receipt)) ->
      match
         msg.WillLock, msg.UnlockRequiresApproval, receipt.PendingCommand
      with
      | false, Some rule, EmployeeCommand.UnlockCard cmd ->
         CommandApprovalProgress.RequestCommandApproval.fromApprovableCommand
            msg.UserSession
            rule
            (cmd |> UnlockCard |> ApprovableCommand.PerCommand)
         |> OrgCommand.RequestCommandApproval
         |> OrgProvider.Msg.OrgCommand
         |> orgDispatch
      | _ -> onCardUpdate receipt

      {
         state with
            IsEditingPurchaseLimit = false
            LockPersistence = Deferred.Idle
      },
      Cmd.none
   | ConfirmUpdateLock(_, Finished(Error err)) ->
      {
         state with
            LockPersistence = Deferred.Resolved(Error err)
      },
      Alerts.toastCommand err

let private nicknameCancelButton dispatch =
   Html.a [
      attr.href ""
      attr.text "Cancel"
      attr.style [ style.padding 10 ]
      attr.classes [ "secondary" ]
      attr.onClick (fun e ->
         e.preventDefault ()
         dispatch Msg.ToggleNicknameEdit)
   ]

let private nicknameSaveButton onClick =
   Html.a [
      attr.href ""
      attr.text "Save"
      attr.style [ style.padding 10 ]
      attr.onClick (fun e ->
         e.preventDefault ()
         onClick ())
   ]

[<ReactComponent>]
let CardNicknameComponent
   (session: UserSession)
   (card: CardWithMetrics)
   dispatch
   =
   let nickname = card.Card.CardNickname
   let pendingNickname, setNickname = React.useState nickname

   let nicknameInputRef = React.useInputRef ()

   React.useEffectOnce (fun () ->
      match nicknameInputRef.current with
      | None -> ()
      | Some input -> input.focus ())

   classyNode Html.div [ "nickname" ] [
      Html.input [
         attr.ref nicknameInputRef
         attr.ariaLabel "Card Nickname"
         attr.type' "text"
         attr.placeholder "Edit card nickname"

         attr.value (pendingNickname |> Option.defaultValue "")

         attr.onChange (fun input ->
            if String.IsNullOrWhiteSpace input then None else Some input
            |> setNickname)
      ]

      if pendingNickname <> nickname then
         match pendingNickname with
         | None -> Html.small $"Card nickname ({nickname}) unchanged."
         | Some alias ->
            Html.small $"Card nickname ({nickname}) will display as {alias}."

      classyNode Html.div [ "nickname-controls" ] [
         match pendingNickname with
         | Some alias when pendingNickname <> nickname ->
            if pendingNickname <> nickname then
               nicknameCancelButton dispatch

               nicknameSaveButton (fun () ->
                  let msg =
                     Msg.SaveNickname(
                        {
                           CommandInput = {
                              Name = alias
                              PriorName = nickname
                              CardId = card.Card.CardId
                           }
                           Employee = card.Employee
                           InitiatedBy = session.AsInitiator
                        },
                        Started
                     )

                  dispatch msg)
         | _ -> ()
      ]
   ]

[<ReactComponent>]
let CardDetailComponent
   (userSession: UserSession)
   (org: OrgWithAccountProfiles)
   (card: CardWithMetrics)
   (onCardUpdate: EmployeeCommandReceipt -> unit)
   =
   let orgDispatch = React.useContext OrgProvider.dispatchContext

   let state, dispatch =
      React.useElmish (init, update onCardUpdate orgDispatch, [||])

   let lockCardMsg = {
      InitiatedBy = userSession.AsInitiator
      Employee = card.Employee
      Card = card.Card
      WillLock =
         match card.Card.Status with
         | CardStatus.Active -> true
         | _ -> false
      UserSession = userSession
      UnlockRequiresApproval =
         CommandApprovalRule.commandTypeRequiresApproval
            (ApprovableCommandType.ApprovablePerCommand UnlockCardCommandType)
            userSession.AsInitiator.Id
            org.Org.CommandApprovalRules
   }

   classyNode Html.div [ "card-detail" ] [
      Html.small card.Employee.Name
      Html.div [
         Html.p [
            attr.style [ style.display.inlineBlock ]
            attr.text card.Card.Display
         ]
         Html.small " "
         Html.small [
            attr.text $"({card.Card.Status})"

            let color =
               match card.Card.Status with
               | CardStatus.Frozen _
               | CardStatus.Closed -> Style.color.alert
               | CardStatus.Pending -> Style.color.secondary
               | CardStatus.Active -> Style.color.primary

            attr.style [ style.color color ]
         ]
      ]

      if state.IsEditingNickname then
         CardNicknameComponent userSession card dispatch

      classyNode Html.div [ "spending-container" ] [
         Html.small "Daily spending"

         Html.b
            $"{Money.format card.DailyPurchaseAccrued} of {Money.format card.Card.DailyPurchaseLimit}"

         Html.progress [
            card.DailyPurchaseAccrued / card.Card.DailyPurchaseLimit
            |> string
            |> attr.value
         ]
      ]

      classyNode Html.div [ "spending-container" ] [
         Html.small "Monthly spending"

         Html.b
            $"{Money.format card.MonthlyPurchaseAccrued} of {Money.format card.Card.MonthlyPurchaseLimit}"

         Html.progress [
            card.MonthlyPurchaseAccrued / card.Card.MonthlyPurchaseLimit
            |> string
            |> attr.value
         ]
      ]

      if state.IsEditingPurchaseLimit then
         PurchaseLimitFormComponent
            userSession
            (fun receipt ->
               onCardUpdate receipt
               dispatch Msg.TogglePurchaseLimitEdit)
            card.Card
            card.Employee

      classyNode Html.div [ "grid"; "card-detail-menu" ] [
         match card.Card.Status with
         | CardStatus.Active ->
            Html.div [
               attr.role "button"
               attr.classes [ "outline" ]
               attr.children [
                  Html.span [ Fa.i [ Fa.Solid.Key ] [] ]
                  Html.span "Lock Card"
               ]
               attr.onClick (fun _ ->
                  dispatch <| Msg.ShowCardLockConfirmation lockCardMsg)
            ]
         | CardStatus.Frozen _ ->
            let cardUnlockInProgress =
               org.Org.CommandApprovalProgress
               |> Map.exists (fun _ p ->
                  match p.Status, p.CommandToInitiateOnApproval with
                  | CommandApprovalProgress.Status.Pending,
                    ApprovableCommand.PerCommand(UnlockCard c) ->
                     c.Data.CardId = card.Card.CardId
                  | _ -> false)

            Html.div [
               attr.role "button"
               attr.classes [ "outline" ]
               if cardUnlockInProgress then
                  attr.disabled true
               attr.children [
                  Html.span [ Fa.i [ Fa.Solid.Key ] [] ]
                  Html.span (
                     if cardUnlockInProgress then
                        "Pending approval"
                     else
                        "Unlock Card"
                  )
               ]
               attr.onClick (fun _ ->
                  dispatch <| Msg.ShowCardLockConfirmation lockCardMsg)
            ]
         | CardStatus.Pending -> ()
         | CardStatus.Closed -> ()

         Html.div [
            attr.role "button"
            attr.classes [ "outline" ]
            attr.children [
               Html.span [ Fa.i [ Fa.Solid.History ] [] ]
               Html.span "Transactions"
            ]
            attr.onClick (fun _ ->
               {
                  TransactionBrowserQuery.empty with
                     Date = Some UIDomain.DateFilter.Last30Days
                     SelectedCards =
                        Some [
                           {
                              Display = card.Card.Display
                              CardId = card.Card.CardId
                           }
                        ]
               }
               |> Routes.TransactionsUrl.queryPath
               |> Router.navigate)
         ]
      ]

      DropdownComponent {|
         Direction = DropdownDirection.LTR
         ShowCaret = true
         Button = None
         Items = [
            {
               Text = "Edit Nickname"
               OnClick = fun _ -> dispatch Msg.ToggleNicknameEdit
               IsSelected = state.IsEditingNickname
            }
            if card.Card.Status = CardStatus.Active then
               {
                  Text = "Edit Purchase Limits"
                  OnClick = fun _ -> dispatch Msg.TogglePurchaseLimitEdit
                  IsSelected = state.IsEditingPurchaseLimit
               }
         (*
            {
               Text = "Deactivate Card"
               OnClick = fun _ -> ()
               IsSelected = false
            }
            *)
         ]
      |}

      classyNode Html.div [ "card-summary" ] [
         classyNode Html.div [ "grid" ] [
            Html.small "Account"
            Html.p (
               org.Accounts.TryFind card.Card.AccountId
               |> Option.map _.Name
               |> Option.defaultValue "-"
            )
         ]

         classyNode Html.div [ "grid" ] [
            Html.small "Card type"
            Html.p (
               if card.Card.Virtual then
                  $"Virtual {card.Card.CardType}"
               else
                  $"Physical {card.Card.CardType}"
            )
         ]
      ]
   ]
