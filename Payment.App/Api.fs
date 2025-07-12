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
            {Table.payment}.*,
            payeeOrg.{OrganizationSqlMapper.OrgFields.name} as payee_org_name,
            payerOrg.{OrganizationSqlMapper.OrgFields.name} as payer_org_name,
            {Table.platformPayment}.{PaymentFields.Platform.payerParentAccountId},
            {Table.platformPayment}.{PaymentFields.Platform.payerOrgId},
            {Table.thirdPartyPayment}.{PaymentFields.ThirdParty.payerName},
            {Table.thirdPartyPayment}.{PaymentFields.ThirdParty.payerEmail},
            {Table.thirdPartyPayment}.{PaymentFields.ThirdParty.shortId}
         FROM {Table.payment}
         LEFT JOIN {Table.platformPayment} using({PaymentFields.paymentId})
         LEFT JOIN {Table.thirdPartyPayment} using({PaymentFields.paymentId})
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
                  Expiration = Reader.expiration read
                  Memo = Reader.memo read
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
