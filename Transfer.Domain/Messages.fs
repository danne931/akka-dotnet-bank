module TransferMessages

open Bank.Transfer.Domain
open DomesticTransfer.Service.Domain

[<RequireQualifiedAccess>]
type AutoTransferMessage =
   | StartScheduledAutoTransfers of AutomaticTransfer.CronSchedule

[<RequireQualifiedAccess>]
type DomesticTransferServiceMessage =
   | TransferRequest of DomesticTransferServiceAction * DomesticTransfer

[<RequireQualifiedAccess>]
type ScheduledTransfersLowBalanceMessage = | Detect
