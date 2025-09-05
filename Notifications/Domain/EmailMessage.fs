module EmailMessage

open System

open Lib.SharedTypes
open Email

#if !FABLE_COMPILER
type OrgOnboardingEmailInfo = { Email: Email; BusinessName: string }

type OrgOnboardingApplicationRejectedEmailInfo = {
   Info: OrgOnboardingEmailInfo
   Reason: string
}

type OrgOnboardingApplicationRequiresRevision = {
   Info: OrgOnboardingEmailInfo
   Reason: string
}

type EmployeeInviteEmailInfo = {
   Name: string
   Email: Email
   Token: string
}

type CardSetupSuccessEmailInfo = {
   EmployeeName: string
   EmployeeEmail: Email
}

type CardSetupFailEmailInfo = { EmployeeName: string; Reason: string }

type InternalTransferBetweenOrgsEmailInfo = {
   SenderAccountName: string
   RecipientBusinessName: string
   Amount: decimal
   OriginatedFromPaymentRequest: PaymentRequestId option
}

type InternalTransferBetweenOrgsDepositEmailInfo = {
   SenderBusinessName: string
   RecipientAccountName: string
   Amount: decimal
   OriginatedFromPaymentRequest: PaymentRequestId option
}

type PlatformPaymentEmailInfo = {
   PayeeBusinessName: string
   PayerBusinessName: string
   Amount: decimal
}

type ThirdPartyPaymentEmailInfo = {
   PayerEmail: Email
   PayeeBusinessName: string
   Amount: decimal
   SecurePaymentFormUrl: string
}

type DomesticTransferEmailInfo = {
   SenderAccountName: string
   RecipientName: string
   Amount: decimal
}

type PurchaseEmailInfo = {
   Email: Email
   Amount: decimal
   Merchant: string
   CardNumberLast4: string
}

type PurchaseFailEmailInfo = { Email: Email; Reason: string }

type ScheduledTransferInsufficientBalanceWarning = {
   SenderAccountName: string
   AvailableBalance: decimal
   ScheduledTransfersCount: int
   ScheduledTransfersAmount: decimal
   ImminentScheduledTransferDate: DateTime
}

[<RequireQualifiedAccess>]
type EmailInfo =
   | OrgOnboardingApplicationSubmitted of OrgOnboardingEmailInfo
   | OrgOnboardingApplicationAccepted of OrgOnboardingEmailInfo
   | OrgOnboardingApplicationRejected of
      OrgOnboardingApplicationRejectedEmailInfo
   | OrgOnboardingApplicationRequiresRevision of
      OrgOnboardingApplicationRequiresRevision
   | AccountOpen of accountName: string
   | AccountClose of accountName: string
   | BillingStatement
   | Purchase of PurchaseEmailInfo
   | PurchaseFailed of PurchaseFailEmailInfo
   | InternalTransferBetweenOrgs of InternalTransferBetweenOrgsEmailInfo
   | InternalTransferBetweenOrgsDeposited of
      InternalTransferBetweenOrgsDepositEmailInfo
   | PlatformPaymentRequested of PlatformPaymentEmailInfo
   | PlatformPaymentReminder of PlatformPaymentEmailInfo
   | PlatformPaymentDeclined of PlatformPaymentEmailInfo
   | ThirdPartyPaymentRequested of ThirdPartyPaymentEmailInfo
   | ThirdPartyPaymentReminder of ThirdPartyPaymentEmailInfo
   | DomesticTransfer of DomesticTransferEmailInfo
   | ScheduledTransferInsufficientBalanceWarning of
      ScheduledTransferInsufficientBalanceWarning
   | ApplicationErrorRequiresSupport of error: string
   | EmployeeInvite of EmployeeInviteEmailInfo
   | CardSetupSuccess of CardSetupSuccessEmailInfo
   | CardSetupFail of CardSetupFailEmailInfo

type EmailMessage = {
   OrgId: OrgId
   CorrelationId: CorrelationId
   Info: EmailInfo
} with

   static member create orgId corrId info = {
      OrgId = orgId
      CorrelationId = corrId
      Info = info
   }
#endif
