module JsonTree

open Feliz
open Fable.Core.JsInterop

let private treeComponent: ReactElement = import "JSONTree" "react-json-tree"

let renderJsonTree (data: obj) : ReactElement =
   Interop.reactApi.createElement (treeComponent, createObj [ "data" ==> data ])
