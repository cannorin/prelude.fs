namespace Prelude

[<AutoOpen>]
module ToplevelOperators =
  open System
  let inline to_s x = x.ToString()

  let inline (?|) opt df = defaultArg opt df

  let inline (!!) (x: ^X) = (^X: (member Value: _) x)

  let inline item1 (x: ^X) = (^X: (member Item1: _) x)
  let inline item2 (x: ^X) = (^X: (member Item2: _) x)
  let inline item3 (x: ^X) = (^X: (member Item3: _) x)
  let inline item4 (x: ^X) = (^X: (member Item4: _) x)
  let inline item5 (x: ^X) = (^X: (member Item5: _) x)
  let inline item6 (x: ^X) = (^X: (member Item6: _) x)
  let inline item7 (x: ^X) = (^X: (member Item7: _) x)

  let inline undefined () = NotImplementedException() |> raise

  let inline notImplemented message = NotImplementedException(message) |> raise

  let inline reraise' ex = System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(ex).Throw(); failwith "impossible"

  let inline private ccl (fc: ConsoleColor) =
    Console.ForegroundColor <- fc;
    { new IDisposable with
        member x.Dispose() = Console.ResetColor() }

  let inline cprintf color format =
    Printf.kprintf (fun s -> use __ = ccl color in printf "%s" s) format

  let inline cprintfn color format =
    Printf.kprintf (fun s -> use __ = ccl color in printfn "%s" s) format

  let inline flip f a b = f b a

  let inline tee f x = f x |> ignore; x

  let inline repeat n f x =
    let rec t i f acc =
      if i <= 0 then acc else t (i-1) f (f acc)
    t n f x

  let inline teefn format x =
    Printf.kprintf (fun s -> printfn "%s" s; x) format x

  let inline dispose (disp: #System.IDisposable) =
    match disp with null -> () | x -> x.Dispose()

  let inline memoize f =
    let mutable map = Map.empty
    fun x ->
      match (map :> Collections.Generic.IDictionary<_, _>).TryGetValue(x) with
        | true, v -> v
        | _ ->
          let result = f x
          map <- map |> Map.add x result
          result

  let inline implicit (x: ^a) : ^b = ((^a or ^b) : (static member op_Implicit: ^a -> ^b) x)

  let inline zero< ^n when ^n: (static member Zero: ^n) > =
    LanguagePrimitives.GenericZero< ^n >

  let inline succ (n: ^number) =
    n + LanguagePrimitives.GenericOne< ^number >

  let inline pred (n: ^number) =
    n - LanguagePrimitives.GenericOne< ^number >

  /// type annotation operator
  let inline annotate<'X> (x: 'X) = x
