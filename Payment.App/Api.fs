module Bank.Payment.Api

open System.Threading.Tasks
open FsToolkit.ErrorHandling

open Lib.Postgres
open Lib.SharedTypes
open Bank.Payment.Domain
open PaymentSqlMapper

module Reader = PaymentSqlReader

let getPayments
   (orgId: OrgId)
   : Result<PaymentRequestSummary option, Err> Task
   =
   taskResultOption {
      let query =
         $"""
         SELECT
            pay.*,
            payeeOrg.{OrganizationSqlMapper.OrgFields.name} as payee_org_name,
            payerOrg.{OrganizationSqlMapper.OrgFields.name} as payer_org_name,
            platformPay.{PaymentFields.Platform.payerParentAccountId},
            platformPay.{PaymentFields.Platform.payerOrgId},
            thirdPartyPay.{PaymentFields.ThirdParty.payerName},
            thirdPartyPay.{PaymentFields.ThirdParty.payerEmail},
            thirdPartyPay.{PaymentFields.ThirdParty.shortId},
            recurrence.{RecurringPaymentScheduleSqlMapper.Fields.pattern},
            recurrence.{RecurringPaymentScheduleSqlMapper.Fields.terminationDetail},
            recurrence.{RecurringPaymentScheduleSqlMapper.Fields.paymentsRequestedCount}
         FROM {Table.payment} pay
         LEFT JOIN {Table.platformPayment} platformPay using({PaymentFields.paymentId})
         LEFT JOIN {Table.thirdPartyPayment} thirdPartyPay using({PaymentFields.paymentId})
         LEFT JOIN {RecurringPaymentScheduleSqlMapper.table} recurrence
            ON
               pay.{PaymentFields.recurringPaymentScheduleId} IS NOT NULL
               AND pay.{PaymentFields.recurringPaymentScheduleId} = recurrence.{RecurringPaymentScheduleSqlMapper.Fields.id}
         JOIN {OrganizationSqlMapper.table} payeeOrg ON payeeOrg.org_id = {PaymentFields.payeeOrgId}
         LEFT JOIN {OrganizationSqlMapper.table} payerOrg ON payerOrg.org_id = {PaymentFields.Platform.payerOrgId}
         """

      let query =
         $"""
         SELECT * FROM ({query})
         WHERE {PaymentFields.payeeOrgId} = @payeeOrgId

         UNION ALL

         SELECT * FROM ({query})
         WHERE {PaymentFields.Platform.payerOrgId} = @payeeOrgId
         """

      let! payments =
         pgQuery<PaymentRequest option>
            query
            (Some [ "payeeOrgId", PaymentSqlWriter.payeeOrgId orgId ])
            (fun read ->
               let shared = {
                  Id = Reader.paymentId read
                  InitiatedBy = Reader.initiatedById read
                  Amount = Reader.amount read
                  Payee = {
                     OrgId = Reader.payeeOrgId read
                     OrgName = read.string "payee_org_name"
                     AccountId = Reader.payeeAccountId read
                     ParentAccountId = Reader.payeeParentAccountId read
                  }
                  Status = Reader.status read
                  CreatedAt = Reader.createdAt read
                  DueAt = Reader.dueAt read
                  Memo = Reader.memo read
                  RecurrenceSettings =
                     PaymentFields.recurringPaymentScheduleId
                     |> read.uuidOrNone
                     |> Option.map (fun _ ->
                        RecurringPaymentScheduleSqlMapper.Reader.recurrenceSettings
                           read)
               }

               match Reader.requestType read with
               | PaymentRequestType.Platform ->
                  Some(
                     PaymentRequest.Platform {
                        SharedDetails = shared
                        Payer = {
                           OrgName = read.string "payer_org_name"
                           OrgId = Reader.Platform.payerOrgId read
                           ParentAccountId =
                              Reader.Platform.payerParentAccountId read
                        }
                     }
                  )
               | PaymentRequestType.ThirdParty ->
                  Some(
                     PaymentRequest.ThirdParty {
                        SharedDetails = shared
                        ShortId = Reader.ThirdParty.shortId read
                        Payer = {
                           Name = Reader.ThirdParty.payerName read
                           Email = Reader.ThirdParty.payerEmail read
                        }
                     }
                  ))

      let paymentSummary =
         payments
         |> List.choose id
         |> List.fold
            (fun acc payment ->
               if payment.SharedDetails.Payee.OrgId = orgId then
                  {
                     acc with
                        OutgoingRequests = payment :: acc.OutgoingRequests
                  }
               else
                  {
                     acc with
                        IncomingRequests = payment :: acc.IncomingRequests
                  })
            {
               IncomingRequests = []
               OutgoingRequests = []
            }

      return paymentSummary
   }
