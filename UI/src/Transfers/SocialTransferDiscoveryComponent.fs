module OrgSocialTransferDiscovery

open Feliz
open Feliz.UseElmish
open Elmish
open System

open Bank.Org.Domain
open Lib.SharedTypes

type State = {
   SearchInput: string option
   PreviousSearchQuery: string option
   Orgs: Deferred<Result<SocialTransferDiscoveryCandidate list option, Err>>
}

type Msg =
   | SetSearchInput of string
   | SearchOrgs of
      orgToExclude: OrgId *
      searchQuery: string *
      AsyncOperationStatus<
         Result<SocialTransferDiscoveryCandidate list option, Err>
       >

let init () =
   {
      SearchInput = None
      PreviousSearchQuery = None
      Orgs = Deferred.Idle
   },
   Cmd.none

let update msg state =
   match msg with
   | SetSearchInput str ->
      let state =
         if String.IsNullOrWhiteSpace str then
            {
               state with
                  SearchInput = None
                  PreviousSearchQuery = None
                  Orgs = Deferred.Idle
            }
         else
            { state with SearchInput = Some str }

      state, Cmd.none
   | SearchOrgs(orgId, searchQuery, Started) ->
      if state.PreviousSearchQuery = Some searchQuery then
         state, Cmd.none
      else
         let cmd =
            async {
               let! res =
                  OrgService.searchOrgTransferSocialDiscovery orgId searchQuery

               return SearchOrgs(orgId, searchQuery, Finished res)
            }
            |> Cmd.fromAsync

         {
            state with
               Orgs = Deferred.InProgress
               PreviousSearchQuery = Some searchQuery
         },
         cmd
   | SearchOrgs(_, _, Finished(Ok res)) ->
      {
         state with
            Orgs = Deferred.Resolved(Ok res)
      },
      Cmd.none
   | SearchOrgs(_, _, Finished(Error err)) ->
      Log.error $"Error fetching orgs: {err}"

      {
         state with
            Orgs = Deferred.Resolved(Error err)
      },
      Alerts.toastCommand err

[<ReactComponent>]
let OrgSearchComponent
   (orgId: OrgId)
   (makeChildren:
      string option
         -> Deferred<Result<SocialTransferDiscoveryCandidate list option, Err>>
         -> ReactElement)
   =
   let state, dispatch = React.useElmish (init, update, [||])

   let searchRef = React.useInputRef ()

   // Fetch orgs after a debounce.
   React.useEffect (
      fun () ->
         let timer =
            Fable.Core.JS.setTimeout
               (fun () ->
                  match state.SearchInput with
                  | None -> ()
                  | Some searchQuery ->
                     dispatch <| Msg.SearchOrgs(orgId, searchQuery, Started))
               800

         React.createDisposable (fun _ -> Fable.Core.JS.clearTimeout timer)
      , [| box state.SearchInput |]
   )

   React.useEffectOnce (fun () ->
      match searchRef.current with
      | Some searchInput -> searchInput.focus ()
      | None -> ())

   React.fragment [
      Html.input [
         attr.type' "search"
         attr.name $"search-orgs"
         attr.placeholder "Search by organization"
         attr.ariaLabel "Search orgs"
         attr.value (state.SearchInput |> Option.defaultValue "")
         attr.onChange (Msg.SetSearchInput >> dispatch)
         attr.ref searchRef
      ]

      makeChildren state.SearchInput state.Orgs
   ]

[<ReactComponent>]
let OrgSearchSelectComponent
   (orgId: OrgId)
   (label: string)
   (makeChildren:
      string option
         -> Deferred<Result<SocialTransferDiscoveryCandidate option, Err>>
         -> ReactElement)
   =
   let selectedOrg, setSelectedOrg =
      React.useState<SocialTransferDiscoveryCandidate option> None

   OrgSearchComponent orgId (fun searchInput orgsDeferred ->
      match orgsDeferred with
      | Deferred.Idle -> Html.none
      | Deferred.Resolved(Ok(Some orgs)) ->
         let selectedOrg = selectedOrg |> Option.orElse (List.tryHead orgs)

         let selectedOrgId =
            selectedOrg |> Option.map (fun o -> string o.OrgId)

         React.fragment [
            Html.label [ Html.text label ]

            Html.select [
               attr.onChange (fun (orgId: string) ->
                  orgs
                  |> List.tryFind (fun o -> string o.OrgId = orgId)
                  |> setSelectedOrg)

               attr.value (selectedOrgId |> Option.defaultValue "")

               attr.children [
                  for org in orgs do
                     let orgId = string org.OrgId

                     Html.option [
                        attr.value orgId
                        attr.text org.OrgName
                        if Some orgId = selectedOrgId then
                           attr.disabled true
                     ]
               ]
            ]

            makeChildren searchInput (Deferred.Resolved(Ok selectedOrg))
         ]
      | Deferred.InProgress -> Html.progress []
      | Deferred.Resolved(Ok None) -> Html.p "No organizations found."
      | Deferred.Resolved(Error err) -> Html.p $"Error searching organizations")
