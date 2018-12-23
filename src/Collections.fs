namespace Prelude
open System.Collections.Generic

type array2d<'t> = 't[,]
type array3d<'t> = 't[,,]    

module List =
  let inline splitWith predicate xs =
    List.foldBack (fun x state ->
      if predicate x then
        [] :: state
      else
        match state with
        | [] -> [[x]]
        | h :: t -> (x :: h) :: t
    ) xs []

  let inline split separator xs = splitWith ((=) separator) xs

  let inline skipSafe length xs =
    if List.length xs > length then
      List.skip length xs
    else List.empty

  let inline foldi folder state xs =
    List.fold (fun (i, state) x -> (i + 1, folder i state x)) (0, state) xs |> snd

module Seq =
  let inline splitWith predicate xs =
    let i = ref 1
    xs |> Seq.groupBy (fun x -> if predicate x then incr i; 0 else !i)
       |> Seq.filter (fst >> ((<>) 0))
       |> Seq.map snd
  
  let inline split separator xs = splitWith ((=) separator) xs

  let inline skipSafe length xs = 
    xs |> Seq.indexed
       |> Seq.skipWhile (fst >> ((>) length))
       |> Seq.map snd
  
  let inline foldi folder state xs =
    Seq.fold (fun (i, state) x -> (i + 1, folder i state x)) (0, state) xs |> snd

module Array =
  let inline skipSafe length xs =
    if Array.length xs > length then
      Array.skip length xs
    else Array.empty

  let inline foldi folder state xs =
    Array.fold (fun (i, state) x -> (i + 1, folder i state x)) (0, state) xs |> snd

module Map =
  open FSharp.Collections
  let inline choose c m =
    m |> Map.fold (
      fun newMap k v ->
        match c k v with
          | Some x -> newMap |> Map.add k x
          | None   -> newMap
    ) Map.empty

  let inline append m1 m2 =
    Map.fold (fun m k v -> Map.add k v m) m1 m2

  let inline concat ms =
    ms |> Seq.fold (fun state m -> append state m) Map.empty

type dict<'a, 'b> = IDictionary<'a, 'b>

module Dict =
  let inline empty<'a, 'b when 'a: comparison> = Map.empty :> dict<'a, 'b>
  let inline map f (d: #dict<_, _>) =
    dict <| seq {
      for KVP(k, v) in d do
        yield k, f k v
    }
  let inline filter p (d: #dict<_, _>) =
    dict <| seq {
      for KVP(k, v) in d do
        if p k v then yield k,v
    }
  let inline choose c (d: #dict<_, _>) =
    dict <| seq {
      for KVP(k, v) in d do
        match c k v with
          | Some x -> yield k, x
          | None -> ()
    }
  let inline fold f init (d: #dict<_, _>) =
    Seq.fold (fun state (KVP(k, v)) -> f state k v) init d
  let inline count (xs: #dict<_, _>) = xs.Count
  let inline exists pred (xs: #dict<_, _>) =
    xs :> seq<_> |> Seq.exists (function KVP(k, v) -> pred k v)
  let inline containsKey x (xs: #dict<_, _>) = xs.ContainsKey x
  let inline find key (xs: #dict<_, _>) = xs.[key]
  let inline tryFind key (xs: #dict<_, _>) =
    if xs.ContainsKey key then xs.[key] |> Some else None
  let inline toMap (xs: #dict<_, _>) =
    let mutable m = Map.empty
    for KVP(k, v) in xs do
      m <- m |> Map.add k v
    m
  let inline toMutable (xs: #dict<'a, 'b>) = new Dictionary<'a, 'b>(xs :> IDictionary<_, _>)
  let inline toSeq (xs: #dict<_, _>) = xs :> seq<kvp<_, _>>
