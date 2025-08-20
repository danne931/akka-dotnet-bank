module TransferMessages

[<RequireQualifiedAccess>]
type AutoTransferMessage =
   | StartScheduledAutoTransfers of AutomaticTransfer.CronSchedule

[<RequireQualifiedAccess>]
type ScheduledTransfersLowBalanceMessage = | Detect
