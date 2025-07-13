module NanoId

let private size = 10

#if FABLE_COMPILER
open Fable.Core.JsInterop
let private nanoIdImport: int -> string = import "nanoid" "nanoid"
let nanoid () : string = nanoIdImport size
#else
let nanoid () : string =
   NanoidDotNet.Nanoid.Generate(size = size)
#endif
