module PDFViewer

open System
open Feliz
open Elmish
open Feliz.UseElmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.FontAwesome
open Browser.Types

open Lib.SharedTypes

// Enable clickable links
importSideEffects "react-pdf/dist/Page/AnnotationLayer.css"

// Display text layer correctly.
// Without this the PDF will render and then all of the text content will
// be duplicated in plain text format.
importSideEffects "react-pdf/dist/Page/TextLayer.css"

[<ImportMember("react-pdf")>]
let private Document: ReactElement = jsNative

[<ImportMember("react-pdf")>]
let private Page: ReactElement = jsNative

[<ImportMember("react-pdf")>]
let private pdfjs: obj = jsNative

let private options =
   createObj [
      // Support non-Latin characters
      "cMapUrl" ==> "/cmaps/"
      // Support standard fonts (deprecated in PDF 1.5, but still around)
      "standardFontDataUrl" ==> "/standard_fonts/"
      // Support JPEG 2000
      "wasmUrl" ==> "/wasm/"
   ]

type PDFDocumentProxy = { numPages: int }

[<ReactComponent>]
let PDFViewerComponent
   (props:
      {|
         PDFFile: File
         onLoadSuccess: PDFDocumentProxy -> unit
         renderPDFInContainer: ReactElement -> ReactElement
      |})
   =
   let containerRef, dimensions = ResizeObserver.useResizeObserver ()
   let numPages, setNumPages = React.useState 0

   let maxWidth = 1600
   let pageWidth = min (int dimensions.width) maxWidth

   let pages =
      match numPages with
      | 0 -> []
      | numPages ->
         [ 1..numPages ]
         |> List.map (fun page ->
            React.keyedFragment (
               $"page_{page}",
               [
                  if page > 1 then
                     Html.br []

                  Interop.reactApi.createElement (
                     Page,
                     createObj [ "pageNumber" ==> page; "width" ==> pageWidth ]
                  )
               ]
            ))

   props.renderPDFInContainer (
      Html.div [
         attr.ref containerRef
         attr.children [
            Interop.reactApi.createElement (
               Document,
               createObj [
                  "file" ==> (props.PDFFile :> obj)
                  "onLoadSuccess"
                  ==> fun proxy ->
                     setNumPages proxy.numPages
                     props.onLoadSuccess proxy
                  "children" ==> pages
                  "options" ==> options
               ]
            )
         ]
      ]
   )

// Initialize worker
[<Emit("new URL($0, import.meta.url).toString()")>]
let private createWorkerUrl (_: string) : string = jsNative

pdfjs?GlobalWorkerOptions?workerSrc <-
   createWorkerUrl "pdfjs-dist/build/pdf.worker.min.mjs"

type State = {
   UploadProgress: Deferred<unit>
   File: File option
} with

   static member Init = {
      UploadProgress = Deferred.Idle
      File = None
   }

type Msg =
   | SetFile of File option
   | UploadFile of File * AsyncOperationStatus<Result<unit, Err>>

let init () = State.Init, Cmd.none

let update (uploadFile: File -> Async<Result<unit, Err>>) msg state =
   match msg with
   | SetFile fileOpt -> { state with File = fileOpt }, Cmd.none
   | UploadFile(file, Started) ->
      let upload = async {
         let! res = uploadFile file
         return Msg.UploadFile(file, Finished res)
      }

      {
         state with
            UploadProgress = Deferred.InProgress
      },
      Cmd.fromAsync upload
   | UploadFile(_, Finished(Ok())) ->
      {
         state with
            UploadProgress = Deferred.Resolved()
      },
      Cmd.none
   | UploadFile(file, Finished(Error err)) ->
      Log.error (string err)
      let err = Err.UnexpectedError $"Problem uploading {file.name}"
      State.Init, Alerts.toastCommand err

[<ReactComponent>]
let PDFUploadComponent
   (props:
      {|
         Label: string option
         uploadFile: File -> Async<Result<unit, Err>>
         renderPDFInContainer: ReactElement -> ReactElement
      |})
   =
   let state, dispatch = React.useElmish (init, update props.uploadFile, [||])

   let onFileChange (e: Event) =
      let input = e.target :?> HTMLInputElement

      match input.files with
      | files when files.length > 0 -> dispatch (Msg.SetFile(Some files.[0]))
      | _ -> dispatch (Msg.SetFile None)

   let fileId = string (Guid.NewGuid())

   Html.div [
      match state.File with
      | None ->
         Html.div [
            Html.label [
               attr.htmlFor fileId
               attr.text (props.Label |> Option.defaultValue "")
            ]
            Html.input [
               attr.id fileId
               attr.type' "file"
               attr.onChange onFileChange
            ]
         ]
      | Some file ->
         match state.UploadProgress with
         | Deferred.Resolved() ->
            Html.div [
               Fa.i [ Fa.Solid.FileAlt ] []

               Html.span [
                  attr.text file.name
                  attr.style [ style.marginLeft (length.rem 0.5) ]
               ]
            ]
         | _ ->
            Html.div [ Html.progress []; Html.p $"Uploading {file.name}..." ]

         PDFViewerComponent {|
            PDFFile = file
            onLoadSuccess = fun _ -> dispatch (Msg.UploadFile(file, Started))
            renderPDFInContainer = props.renderPDFInContainer
         |}
   ]
