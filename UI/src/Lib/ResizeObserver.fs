module ResizeObserver

open Fable.Core
open Feliz
open Browser.Types

type ResizeDimensions = {
   x: decimal
   y: decimal
   width: decimal
   height: decimal
   top: decimal
   left: decimal
   bottom: decimal
   right: decimal
}

[<ImportMember("@mantine/hooks")>]
let useResizeObserver
   : unit -> (IRefValue<HTMLElement option> * ResizeDimensions) =
   jsNative
