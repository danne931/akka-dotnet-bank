module Constants

open System

open Lib.SharedTypes

let ORG_ID_REMOVE_SOON =
   "ec3e94cc-eba1-4ff4-b3dc-55010ecf67b9" |> Guid.Parse |> OrgId

let LOGGED_IN_EMPLOYEE_ID_REMOVE_SOON =
   "4a53945f-bff1-4955-b964-098120f1080d" |> Guid.Parse |> EmployeeId

let DAILY_PURCHASE_LIMIT_DEFAULT = 10_000m
let MONTHLY_PURCHASE_LIMIT_DEFAULT = 150_000m

let SIGNAL_R_HUB = "/bankHub"
