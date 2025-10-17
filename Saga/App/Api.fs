module SagaApi

open FsToolkit.ErrorHandling

open Lib.SharedTypes
open Lib.Postgres
open AppSagaSqlMapper
open SagaDTO

let getAllSagas () : TaskResultOption<SagaDTO list, Err> = taskResultOption {
   let query =
      $"SELECT * FROM {table}
        ORDER BY created_at DESC
        LIMIT 100"

   let! sagas = pgQuery query None Reader.sagaState

   return sagas |> List.map AppSaga.SagaDTO.fromSaga
}
