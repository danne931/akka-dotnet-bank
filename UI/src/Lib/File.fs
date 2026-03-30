module File

open Fable.Core
open Browser.Types

[<Emit("new File($0, $1, $2)")>]
let private createFile (parts: obj[]) (name: string) (options: obj) : File =
   jsNative

let fromBlob (blob: Blob) (fileName: string) : File =
   createFile [| blob :> obj |] fileName {| type' = blob.``type`` |}
