[<RequireQualifiedAccess>]
module Permissions

open Lib.SharedTypes

type Access(roles: Role list) =
   member x.Roles = roles
   member x.HasAccess(role: Role) = x.Roles |> List.contains role

let private all = [ Role.Admin; Role.Scholar; Role.CardOnly ]

let private admin = Role.Admin
let private scholar = Role.Scholar
let private card = Role.CardOnly

// Employee
let GetEmployees = Access [ admin; scholar ]
let GetEmployeeHistory = Access all
let CreateEmployee = Access [ admin ]
let UpdateRole = Access [ admin ]
let DebitRequest = Access [ admin; card ]
let UpdateDailyDebitLimit = Access [ admin ]
let LockCard = Access [ admin ]
let UnlockCard = Access [ admin ]
let CancelEmployeeInvitation = Access [ admin ]
let RestoreEmployeeAccess = Access [ admin ]
let ResendInviteNotification = Access [ admin; scholar ]

// Transfer
let ManageTransferRecipient = Access [ admin ]
let SubmitTransfer = Access [ admin ]

// Transaction
let GetTransactions = Access [ admin; scholar ]
let GetTransactionInfo = Access all
let GetCategories = Access [ admin; scholar ]
let ManageTransactionCategory = Access [ admin; scholar ]
let ManageTransactionNotes = Access all
let GetMerchants = Access all
let ManageMerchants = Access [ admin; scholar ]

// Account
let GetOrgAndAccountProfiles = Access [ admin; scholar ]
let GetAccount = Access [ admin; scholar ]
let CreateAccount = Access [ admin ]
let Deposit = Access [ admin ]
let CloseAccount = Access [ admin ]
let BillingStatement = Access [ admin; scholar ]

let Diagnostic = Access [ admin ]
