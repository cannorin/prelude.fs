namespace Prelude

module Lazy =
  let inline run (x: Lazy<_>) = x.Value

  let inline force (x: Lazy<_>) = x.Force()
  
  let inline bind (f: 'a -> Lazy<'b>) (x: Lazy<'a>) : Lazy<'b> = lazy (f x.Value).Value

  let inline returnValue x = Lazy<_>.CreateFromValue x

  let inline returnThunk thunk = Lazy<_>.Create thunk

  let inline map (f: 'a -> 'b) (x: Lazy<'a>) = lazy (f x.Value)

  let inline flatten (x: Lazy<Lazy<'a>>) = lazy (!!(!!x))

  let inline apply (f: Lazy<'a -> 'b>) (x: Lazy<'a>) = lazy (!!f !!x)

module Tuple =
  let inline map2 f g (x, y) = (f x, g y)
  let inline map3 f g h (x, y, z) = (f x, g y, h z)
  let inline map4 f g h i (x, y, z, w) = (f x, g y, h z, i w)
  let inline map5 f g h i j (x, y, z, w, v) = (f x, g y, h z, i w, j v)

module Result =
  let inline bimap f g res =
    match res with
      | Ok x -> Ok (f x)
      | Error e -> Error (g e)

  let inline toOption res =
    match res with
      | Ok x -> Some x
      | Error _ -> None
  
  let inline toChoice res =
    match res with
      | Ok x -> Choice1Of2 x
      | Error e -> Choice2Of2 e

  let inline ofOption opt =
    match opt with
      | Some x -> Ok x
      | None -> Error ()

  let inline ofChoice cic =
    match cic with
      | Choice1Of2 x -> Ok x
      | Choice2Of2 e -> Error e

  let inline get res =
    match res with
      | Ok x -> x
      | Error e -> reraise' e

  let inline catch f res =
    match res with
      | Ok x -> x
      | Error e -> f e

  let inline defaultValue y res =
    match res with
      | Ok x -> x
      | Error _ -> y

module Async =
  open System
  open Microsoft.FSharp.Control

  let inline run x = Async.RunSynchronously x
  let inline returnValue x = async.Return x
  let inline bind f m = async.Bind(m, f)
  let inline map  f m = bind (f >> returnValue) m
  let never = Async.Sleep -1
  let unit  = returnValue ()

  let inline timeout (timeout : TimeSpan) a =
    async {
      try
        let! child = Async.StartChild(a, int timeout.TotalMilliseconds) in
        let! result = child in
        return Some result
      with
        | :? TimeoutException -> return None
    }