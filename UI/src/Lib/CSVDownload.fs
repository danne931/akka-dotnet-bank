module CSVDownload

let save (props: {| FileName: string; Content: string |}) =
   Fable.Core.JsInterop.emitJsExpr
      (props.Content, props.FileName)
      """
      (() => {
         const blob = new Blob([$0], { type: 'text/csv' })

         // Create temporary URL for the CSV Blob
         const url = URL.createObjectURL(blob)
         const a = document.createElement('a')
         a.href = url

         // Set filename for download
         a.download = $1
         document.body.appendChild(a)

         a.click()

         // Clean up DOM & free memory
         document.body.removeChild(a)
         URL.revokeObjectURL(url)
      })()
   """
   |> ignore
