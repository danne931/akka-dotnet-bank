module BlobStorage

open Azure.Storage.Blobs
open FsToolkit.ErrorHandling

let getContainerClient () : TaskResult<BlobContainerClient, string> =
   taskResult {
      let serviceClient =
         BlobServiceClient Env.config.AzureBlobStorage.ConnectionString

      let containerClient =
         serviceClient.GetBlobContainerClient
            Env.config.AzureBlobStorage.ContainerName

      let! _ = containerClient.CreateIfNotExistsAsync()

      return containerClient
   }
   |> TaskResult.catch _.Message
