module EmployeeSqlMapper

open System

open Lib.SharedTypes
open Bank.Employee.Domain
open OrganizationSqlMapper

let table = "employee"

module EmployeeTypeCast =
   let status = "employee_status"
   let role = "employee_role"

module EmployeeFields =
   let employeeId = "employee_id"
   let orgId = OrgFields.orgId
   let role = "role"
   let email = "email"
   let firstName = "first_name"
   let lastName = "last_name"
   let status = "status"
   let statusDetail = "status_detail"
   let searchQuery = "search_query"
   let inviteToken = "invite_token"
   let inviteExpiration = "invite_expiration"
   let authProviderUserId = "auth_provider_user_id"

module EmployeeSqlReader =
   let employeeId (read: RowReader) =
      EmployeeFields.employeeId |> read.uuid |> EmployeeId

   let orgId = OrgSqlReader.orgId

   let role (read: RowReader) =
      read.string EmployeeFields.role |> Role.fromStringUnsafe

   let email (read: RowReader) =
      read.string EmployeeFields.email |> Email.deserialize

   let firstName (read: RowReader) = read.string EmployeeFields.firstName
   let lastName (read: RowReader) = read.string EmployeeFields.lastName
   let searchQuery (read: RowReader) = read.text EmployeeFields.searchQuery

   let inviteToken (read: RowReader) : InviteToken option =
      let token = read.uuidOrNone EmployeeFields.inviteToken
      let exp = read.dateTimeOrNone EmployeeFields.inviteExpiration

      Option.map2
         (fun token exp -> { Token = token; Expiration = exp })
         token
         exp

   let statusDetail (read: RowReader) =
      read.text EmployeeFields.statusDetail
      |> Serialization.deserializeUnsafe<EmployeeStatus>

   let authProviderUserId (read: RowReader) =
      read.uuidOrNone EmployeeFields.authProviderUserId

   let employee (read: RowReader) : Employee = {
      EmployeeId = employeeId read
      OrgId = orgId read
      Role = role read
      Email = email read
      FirstName = firstName read
      LastName = lastName read
      Cards = Map.empty
      Status = statusDetail read
      AuthProviderUserId = authProviderUserId read
   }

module EmployeeSqlWriter =
   let employeeId (EmployeeId id) = Sql.uuid id

   let employeeIds (ids: EmployeeId list) =
      ids |> List.map _.Value |> Array.ofList |> Sql.uuidArray

   let orgId = OrgSqlWriter.orgId
   let role (role: Role) = Sql.string <| string role

   let roles (roles: Role list) =
      roles |> List.map string |> List.toArray |> Sql.stringArray

   let email (email: Email) = Sql.string <| string email
   let firstName = Sql.string
   let lastName = Sql.string
   let searchQuery = Sql.text

   let status (status: EmployeeStatus) = status |> string |> Sql.string

   let statusDetail (status: EmployeeStatus) =
      status |> Serialization.serialize |> Sql.jsonb

   let inviteToken = Sql.uuidOrNone
   let inviteExpiration (date: DateTime option) = Sql.timestamptzOrNone date

   let inviteTokenFromStatus (status: EmployeeStatus) =
      match status with
      | EmployeeStatus.PendingInviteConfirmation invite ->
         inviteToken (Some invite.Token.Token)
      | _ -> inviteToken None

   let inviteExpirationFromStatus (status: EmployeeStatus) =
      match status with
      | EmployeeStatus.PendingInviteConfirmation invite ->
         inviteExpiration (Some invite.Token.Expiration)
      | _ -> inviteExpiration None

   let authProviderUserId = Sql.uuidOrNone
