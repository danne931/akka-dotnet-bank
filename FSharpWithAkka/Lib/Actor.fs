module ActorUtil

open type Echo.Process

let isAlive pid =
   try
      exists pid
   with ex when true ->
      printfn "Echo.Process.exists exception: %A %A" pid ex.Message
      false
