namespace Prelude

[<AutoOpen>]
module ToplevelOperators =
  open System
  let inline to_s x = x.ToString()

  let inline (?|) opt df = defaultArg opt df

  let inline (!!) (x: Lazy<'a>) = x.Value

  let inline undefined (x: 'a) : 'b = NotImplementedException(to_s x) |> raise

  let inline reraise' ex = System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(ex).Throw(); failwith "impossible"

  let inline private ccl (fc: ConsoleColor) =
    Console.ForegroundColor <- fc;
    { new IDisposable with
        member x.Dispose() = Console.ResetColor() }

  let inline cprintf color format =
    Printf.kprintf (fun s -> use c = ccl color in printf "%s" s) format

  let inline cprintfn color format =
    Printf.kprintf (fun s -> use c = ccl color in printfn "%s" s) format

  let inline dispose (disp: #System.IDisposable) =
    match disp with null -> () | x -> x.Dispose()

  let inline implicit (x: ^a) : ^b = ((^a or ^b) : (static member op_Implicit: ^a -> ^b) x)

  let inline succ (n: ^number) =
    n + LanguagePrimitives.GenericOne< ^number >

  let inline pred (n: ^number) =
    n - LanguagePrimitives.GenericOne< ^number >
