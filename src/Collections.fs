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

  let inline splitWhile condition xs =
    let mutable b = true
    List.partition (fun x -> if b && condition x then true else b <- false; false) xs

  let inline tryTake length xs =
    if List.length xs >= length then
      List.take length xs |> Some
    else None

  let inline skipSafe length xs =
    if List.length xs > length then
      List.skip length xs
    else List.empty

  let inline tryFold folder state xs =
    List.fold (fun state x -> match state with Some s -> folder s x | None -> None) (Some state) xs

  let inline foldWhile condition folder state xs =
    List.fold
      (fun (cond, state) x -> if cond && condition x then true, folder state x else false, state)
      (true, state)
      xs
    |> snd

  let inline foldi folder state xs =
    List.fold (fun (i, state) x -> (i + 1, folder i state x)) (0, state) xs |> snd

  let inline mapFoldi folder state xs =
    List.mapFold
      (fun (i, state) x -> let result, state = folder i state x in result, (i+1, state))
      (0, state)
      xs
    |> Tuple.map2 id snd

module Seq =
  let inline splitWith predicate xs =
    let i = ref 1
    xs |> Seq.groupBy (fun x -> if predicate x then incr i; 0 else !i)
       |> Seq.filter (fst >> ((<>) 0))
       |> Seq.map snd
  
  let inline split separator xs = splitWith ((=) separator) xs

  let inline splitWhile condition xs =
    let xs = Seq.cache xs
    Seq.takeWhile condition xs, Seq.skipWhile condition xs

  let inline skipSafe length xs = 
    xs |> Seq.indexed
       |> Seq.skipWhile (fst >> ((>) length))
       |> Seq.map snd

  let inline tryTake length xs =
    let xs' = xs |> Seq.indexed |> Seq.cache
    if xs' |> Seq.exists (fst >> ((=) (length - 1))) then
      xs' |> Seq.take length |> Seq.map snd |> Some
    else None
 
  let inline tryFold folder state xs =
    Seq.fold (fun state x -> match state with Some s -> folder s x | None -> None) (Some state) xs

  let inline foldWhile condition folder state xs =
    Seq.fold
      (fun (cond, state) x -> if cond && condition x then true, folder state x else false, state)
      (true, state)
      xs
    |> snd
 
  let inline foldi folder state xs =
    Seq.fold (fun (i, state) x -> (i + 1, folder i state x)) (0, state) xs |> snd

  let inline mapFoldi folder state xs =
    Seq.mapFold
      (fun (i, state) x -> let result, state = folder i state x in result, (i+1, state))
      (0, state)
      xs
    |> Tuple.map2 id snd

module Array =
  let inline skipSafe length xs =
    if Array.length xs > length then
      Array.skip length xs
    else Array.empty

  let inline tryTake length xs =
    if Array.length xs > length then
      Array.take length xs |> Some
    else if Array.length xs = length then Some xs
    else None

  let inline tryFold folder state xs =
    Array.fold (fun state x -> match state with Some s -> folder s x | None -> None) (Some state) xs

  let inline foldWhile condition folder state xs =
    Array.fold
      (fun (cond, state) x -> if cond && condition x then true, folder state x else false, state)
      (true, state)
      xs
    |> snd

  let inline foldi folder state xs =
    Array.fold (fun (i, state) x -> (i + 1, folder i state x)) (0, state) xs |> snd

  let inline mapFoldi folder state xs =
    Array.mapFold
      (fun (i, state) x -> let result, state = folder i state x in result, (i+1, state))
      (0, state)
      xs
    |> Tuple.map2 id snd

module Map =
  open FSharp.Collections
  let inline choose c m =
    m |> Map.fold (
      fun newMap k v ->
        match c k v with
          | Some x -> newMap |> Map.add k x
          | None   -> newMap
    ) Map.empty

  /// Appends two maps. If there is a duplicate key,
  /// the value in the latter map (`m2`) will be used.
  let inline append m1 m2 =
    Map.fold (fun m k v -> Map.add k v m) m1 m2

  /// Concats multiple maps. If there is a duplicate key,
  /// the value in the last map containing that key will be used.
  let inline concat ms =
    ms |> Seq.fold (fun state m -> append state m) Map.empty

  /// Merges two maps. If there is a duplicate key, the `merger` function
  /// will be called: the first parameter is the key, the second is the value
  /// found in the formar map `m1`, and the third is the one found in `m2`.
  let inline merge merger m1 m2 =
    Map.fold (fun m k v1 -> 
      match m |> Map.tryFind k with
        | Some v2 -> Map.add k (merger k v1 v2) m
        | None -> Map.add k v1 m
      ) m1 m2
  
  /// Merges multiple maps. If there is a duplicate key, the `merger` function
  /// will be called: the first parameter is the key, the second is the value
  /// already found in the earlier maps, and the third is the value newly found.
  let inline mergeMany merger ms =
    ms |> Seq.fold (fun state m -> merge merger state m) Map.empty

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
