module EmployeeDashboard

open Feliz
open Feliz.UseElmish
open Elmish
open Feliz.Router
open Fable.FontAwesome

open Bank.Org.Domain
open Bank.Employee.Domain
open UIDomain.Employee
open UIDomain.Org
open Lib.SharedTypes
open EmployeeDetail
open Bank.Employee.Forms.EmployeeCreateForm
open EmployeeSearch
open TableControlPanel

[<RequireQualifiedAccess>]
type EmployeeFilterView =
   | Employees
   | Roles

[<RequireQualifiedAccess>]
type EmployeeFilter =
   | Employees of (SelectedEmployee list) option
   | Roles of (Role list) option

type State = {
   Query: EmployeeQuery
   Employees: Deferred<EmployeesMaybe>
}

let private actionNav (action: EmployeeActionView option) =
   let queryString =
      {
         Routes.IndexUrl.employeeBrowserQuery () with
            Action = action
      }
      |> EmployeeBrowserQuery.toQueryParams
      |> Router.encodeQueryString

   [| Routes.EmployeeUrl.BasePath; queryString |]

let private employeeCommandProcessing
   (state: State)
   (receipt: EmployeeCommandReceipt)
   =
   let em = receipt.PendingState

   {
      state with
         Employees =
            (Deferred.map << Result.map << Option.map)
               (fun employees ->
                  match receipt.PendingEvent with
                  | EmployeeEvent.CreatedEmployee _ -> em :: employees
                  | _ ->
                     employees
                     |> List.map (fun e ->
                        if e.EmployeeId = em.EmployeeId then em else e))
               state.Employees
   }

type Msg =
   | LoadEmployees of EmployeeQuery * AsyncOperationStatus<EmployeesMaybe>
   | UpdateFilter of EmployeeFilter
   | EmployeeCommandProcessing of EmployeeCommandReceipt
   | SubmittedEmployeeForApproval of EmployeeCommandReceipt

let init (browserQuery: EmployeeBrowserQuery) () =
   let query = EmployeeService.networkQueryFromBrowserQuery browserQuery

   {
      Employees = Deferred.Idle
      Query = query
   },
   Cmd.ofMsg (LoadEmployees(query, Started))

let update orgId msg state =
   match msg with
   | LoadEmployees(query, Started) ->
      let loadEmployees = async {
         let! res = EmployeeService.getEmployees orgId query
         return LoadEmployees(query, Finished res)
      }

      {
         Query = query
         Employees = Deferred.InProgress
      },
      Cmd.fromAsync loadEmployees
   | LoadEmployees(_, Finished(Ok(Some employees))) ->
      {
         state with
            Employees = Deferred.Resolved(Ok(Some employees))
      },
      Cmd.none
   | LoadEmployees(_, Finished(Ok None)) ->
      {
         state with
            Employees = Deferred.Resolved(Ok None)
      },
      Cmd.none
   | LoadEmployees(_, Finished(Error err)) ->
      {
         state with
            Employees = Deferred.Resolved(Error err)
      },
      Cmd.none
   | UpdateFilter filter ->
      let browserQuery = Routes.IndexUrl.employeeBrowserQuery ()

      let browserQuery =
         match filter with
         | EmployeeFilter.Employees selected -> {
            browserQuery with
               SelectedEmployees = selected
           }
         | EmployeeFilter.Roles roles -> { browserQuery with Roles = roles }

      let browserQueryParams =
         browserQuery
         |> EmployeeBrowserQuery.toQueryParams
         |> Router.encodeQueryString

      state, Cmd.navigate (Routes.EmployeeUrl.BasePath, browserQueryParams)
   | EmployeeCommandProcessing receipt ->
      let state = employeeCommandProcessing state receipt
      state, Cmd.none
   | SubmittedEmployeeForApproval receipt ->
      let state = employeeCommandProcessing state receipt

      state,
      Cmd.batch [
         Cmd.navigate (actionNav None)
         Alerts.toastSuccessCommand "Employee invite submitted for approval"
      ]

let selectedEmployee
   (selectedEmployeeId: EmployeeId)
   (state: State)
   : Employee option
   =
   match state.Employees with
   | Deferred.Resolved(Ok(Some employees)) ->
      employees |> List.tryFind (fun e -> e.EmployeeId = selectedEmployeeId)
   | _ -> None

let renderTableRow
   (org: Org)
   (employee: Employee)
   (selectedEmployeeId: EmployeeId option)
   =
   let updatedRolePendingApproval =
      employeeRolePendingApproval
         org.CommandApprovalProgress.Values
         employee.EmployeeId

   Html.tr [
      attr.key (string employee.EmployeeId)

      match selectedEmployeeId with
      | Some id when id = employee.EmployeeId -> attr.classes [ "selected" ]
      | _ -> ()

      attr.onClick (fun _ ->
         employee.EmployeeId
         |> EmployeeActionView.ViewEmployee
         |> Some
         |> actionNav
         |> Router.navigate)

      attr.children [
         Html.th [ attr.scope "row" ]

         Html.td employee.Name

         Html.td (string employee.Email)

         Html.td [
            match updatedRolePendingApproval with
            | Some pendingRole ->
               attr.text $"{employee.Role.Display} -> {pendingRole.Display}"

               attr.custom (
                  "data-tooltip",
                  $"{pendingRole.Display} role pending approval."
               )
            | None -> attr.text employee.Role.Display
         ]

         Html.td employee.Status.Display
      ]
   ]

let renderTable
   (org: Org)
   (employees: Employee list)
   (selectedEmployeeId: EmployeeId option)
   =
   Html.table [
      attr.classes [ "clickable-table" ]
      attr.role "grid"
      attr.children [
         Html.thead [
            Html.tr [
               Html.th [ attr.scope "col" ]

               Html.th [ attr.scope "col"; attr.text "Name" ]

               Html.th [ attr.scope "col"; attr.text "Email" ]

               Html.th [ attr.scope "col"; attr.text "Role" ]

               Html.th [ attr.scope "col"; attr.text "Status" ]
            ]
         ]

         Html.tbody [
            for employee in employees ->
               renderTableRow org employee selectedEmployeeId
         ]
      ]
   ]

[<ReactComponent>]
let EmployeeDashboardComponent
   (url: Routes.EmployeeUrl)
   (session: UserSession)
   =
   let browserQuery = Routes.IndexUrl.employeeBrowserQuery ()

   let state, dispatch =
      React.useElmish (
         init browserQuery,
         update session.OrgId,
         [| box browserQuery.ChangeDetection |]
      )

   let orgCtx = React.useContext OrgProvider.context
   let orgDispatch = React.useContext OrgProvider.dispatchContext

   let selectedEmployeeId = Routes.EmployeeUrl.employeeIdMaybe url

   let close _ = Router.navigate (actionNav None)

   let employees =
      state.Employees
      |> (Deferred.map << Result.map << Option.map) (
         List.sortBy (fun e ->
            let roleSort =
               match e.Role with
               | Role.Admin -> 1
               | Role.CardOnly -> 2
               | Role.Scholar -> 3

            let statusSort =
               match e.Status with
               | EmployeeStatus.PendingInviteApproval _ -> 1
               | EmployeeStatus.PendingRestoreAccessApproval -> 2
               | EmployeeStatus.PendingInviteConfirmation _ -> 3
               | EmployeeStatus.Active -> 4
               | EmployeeStatus.Closed -> 5
               | EmployeeStatus.ReadyForDelete -> 6
               | EmployeeStatus.InitialEmptyState -> 6

            statusSort, roleSort)
      )

   classyNode Html.div [ "employee-dashboard" ] [
      classyNode Html.main [ "container-fluid" ] [
         classyNode Html.div [ "grid" ] [
            Html.section [
               Html.h4 [ attr.text "Employees" ]

               Html.progress [
                  attr.custom ("data-transactions-loader", "")
                  if Deferred.resolved employees then
                     attr.value 100
               ]

               TableControlPanelComponent {|
                  FilterViewOptions = [
                     EmployeeFilterView.Employees, "Employees"
                     EmployeeFilterView.Roles, "Employee Role"
                  ]
                  RenderFilterViewOnSelect =
                     function
                     | EmployeeFilterView.Employees ->
                        EmployeeMultiSelectSearchComponent {|
                           OrgId = session.OrgId
                           Selected = browserQuery.SelectedEmployees
                           OnSelect =
                              EmployeeFilter.Employees
                              >> Msg.UpdateFilter
                              >> dispatch
                           Dependencies = None
                        |}
                     | EmployeeFilterView.Roles ->
                        CheckboxFieldset.render {|
                           Options =
                              [ Role.Admin; Role.Scholar; Role.CardOnly ]
                              |> List.map (fun o -> {
                                 Id = o
                                 Display = o.Display
                              })
                           SelectedItems = browserQuery.Roles
                           OnChange =
                              EmployeeFilter.Roles
                              >> Msg.UpdateFilter
                              >> dispatch
                        |}
                  FilterPills =
                     [
                        {
                           View = EmployeeFilterView.Roles
                           OnDelete =
                              fun () ->
                                 EmployeeFilter.Roles None
                                 |> Msg.UpdateFilter
                                 |> dispatch
                           Content =
                              state.Query.Roles
                              |> Option.map EmployeeQuery.rolesToDisplay
                        }
                     ]
                     @ [
                        match browserQuery.SelectedEmployees with
                        | None -> ()
                        | Some selected ->
                           for employee in selected ->
                              {
                                 View = EmployeeFilterView.Employees
                                 OnDelete =
                                    fun () ->
                                       selected
                                       |> List.filter (fun e ->
                                          e.Id <> employee.Id)
                                       |> fun es ->
                                          (if es.IsEmpty then None else Some es)
                                          |> EmployeeFilter.Employees
                                          |> Msg.UpdateFilter
                                          |> dispatch
                                 Content = Some employee.Name
                              }
                     ]
                  SubsequentChildren =
                     Some [
                        Html.button [
                           attr.classes [ "create-employee" ]
                           attr.children [
                              Fa.i [ Fa.Solid.UserPlus ] []
                              Html.span "Invite"
                           ]

                           attr.onClick (fun _ ->
                              Some EmployeeActionView.Create
                              |> actionNav
                              |> Router.navigate)
                        ]
                     ]
               |}

               match orgCtx, employees with
               | _, Resolved(Error _) ->
                  Html.small "Uh oh. Error getting employees."
               | _, Resolved(Ok None) -> Html.small "Uh oh. No employees."
               | Resolved(Ok(Some org)), Resolved(Ok(Some employees)) ->
                  if employees.IsEmpty then
                     Html.small "Uh oh. No employees."
                  else
                     renderTable org.Org employees selectedEmployeeId
               | _ -> ()
            ]

            match url with
            | Routes.EmployeeUrl.EmployeesWithSearchQuery query ->
               match query.Action with
               | Some action ->
                  classyNode Html.article [ "form-wrapper" ] [
                     Html.h6 (
                        match action with
                        | EmployeeActionView.Create -> "Invite Employee"
                        | _ -> "Employee Detail"
                     )

                     CloseButton.render close

                     match action with
                     | EmployeeActionView.Create ->
                        EmployeeCreateFormComponent
                           session
                           (fun receipt ->
                              close ()
                              dispatch (Msg.EmployeeCommandProcessing receipt))
                           (fun (approvalRequest, receipt) ->
                              approvalRequest
                              |> OrgCommand.RequestCommandApproval
                              |> OrgProvider.Msg.OrgCommand
                              |> orgDispatch

                              receipt
                              |> Msg.SubmittedEmployeeForApproval
                              |> dispatch)
                     | EmployeeActionView.ViewEmployee id ->
                        classyNode Html.div [ "employee-detail" ] [
                           match employees, orgCtx with
                           | Deferred.Resolved _,
                             Deferred.Resolved(Ok(Some org)) ->
                              match selectedEmployee id state with
                              | None -> Html.small "Uh oh. Employee not found."
                              | Some employee ->
                                 EmployeeDetailComponent
                                    session
                                    employee
                                    org.Org
                                    (Msg.EmployeeCommandProcessing >> dispatch)
                           | _ -> Html.progress []
                        ]
                  ]
                  |> ScreenOverlay.Portal
               | None -> ()
            | _ -> ()
         ]
      ]
   ]
