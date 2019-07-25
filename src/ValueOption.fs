namespace Prelude

module ValueOption =
  let inline ofOption option = match option with Some x -> ValueSome x | None -> ValueNone

  let inline toOption option = match option with ValueSome x -> Some x | ValueNone -> None

[<AutoOpen>]
module ValueOptionExtension =
  type ValueOption<'T> with
    member inline this.IsSome = ValueOption.isSome this
    member inline this.IsNone = ValueOption.isNone this

module ValueOptionFunctions =
  open System.Collections.Generic
  
  module Array =
    let inline tryLast' (array: _[]) =
      if array.Length = 0 then ValueNone
      else ValueSome array.[array.Length-1]

    let inline tryHead' (array: _[]) =
      if array.Length = 0 then ValueNone
      else ValueSome array.[0]

    let inline tryTake' length xs =
      if Array.length xs > length then
        Array.take length xs |> ValueSome
      else if Array.length xs = length then ValueSome xs
      else ValueNone

    let inline tryItem' index (array: _[]) =
      if index < 0 || index >= array.Length then ValueNone
      else ValueSome (array.[index])
    
    let inline tryFind' predicate (array: _[]) =
      let rec loop i =
        if i >= array.Length then ValueNone else
        if predicate array.[i] then ValueSome array.[i] else loop (i+1)
      loop 0

    let inline tryFindIndex' predicate (array: _[]) =
      let len = array.Length
      let rec go n = if n >= len then ValueNone elif predicate array.[n] then ValueSome n else go (n+1)
      go 0

  module IEnumerator =
    let inline tryItem' index (e : IEnumerator<'T>) =
      let rec loop index =
        if not (e.MoveNext()) then ValueNone
        elif index = 0 then ValueSome(e.Current)
        else loop (index-1)
      loop index
    
  module Seq =
    let inline tryLast' (source : seq<_>) =
      use e = source.GetEnumerator()
      if e.MoveNext() then
        let mutable res = e.Current
        while (e.MoveNext()) do res <- e.Current
        ValueSome res
      else
        ValueNone

    let inline tryHead' (source : seq<_>) =
      use e = source.GetEnumerator()
      if (e.MoveNext()) then ValueSome e.Current
      else ValueNone

    let inline tryTake' length xs =
      let xs' = xs |> Seq.indexed |> Seq.cache
      if xs' |> Seq.exists (fst >> ((=) (length - 1))) then
        xs' |> Seq.take length |> Seq.map snd |> ValueSome
      else ValueNone

    let inline tryItem' index (source : seq<'T>) =
      if index < 0 then ValueNone else
      use e = source.GetEnumerator()
      IEnumerator.tryItem' index e
    
    let inline tryFind' predicate (source : seq<'T>)  =
      use e = source.GetEnumerator()
      let mutable res = ValueNone
      while (ValueOption.isNone res && e.MoveNext()) do
        let c = e.Current
        if predicate c then res <- ValueSome(c)
      res

    let inline tryFindIndex' predicate (source:seq<_>) =
      use ie = source.GetEnumerator()
      let rec loop i =
        if ie.MoveNext() then
          if predicate ie.Current then
            ValueSome i
          else loop (i+1)
        else
          ValueNone
      loop 0

    let inline choose' chooser (source: seq<_>) =
      seq {
        for item in source do
          match chooser item with
            | ValueSome x -> yield x
            | ValueNone -> ()
      }
  
  module List =
    let inline tryLast' (list: 'T list) =
      let rec loop list =
        match list with
          | [x] -> ValueSome x
          | _ :: tail -> loop tail
          | [] -> ValueNone
      loop list

    let inline tryHead' list = match list with x::_ -> ValueSome x | [] -> ValueNone

    let inline tryItem' index list =
      let rec loop index list =
        match list with
          | h::t when index >= 0 ->
            if index = 0 then ValueSome h else loop (index - 1) t
          | _ -> ValueNone
      loop index list

    let inline tryTake' length xs =
      if List.length xs >= length then
        List.take length xs |> ValueSome
      else ValueNone

    let inline tryFind' predicate list =
      let rec loop list =
        match list with
          | [] -> ValueNone
          | h::t -> if predicate h then ValueSome h else loop t
      loop list

    let inline tryFindIndex' predicate list = 
      let rec loop n = function[] -> ValueNone | h::t -> if predicate h then ValueSome n else loop (n+1) t
      loop 0 list

    let inline choose' chooser list =
      List.foldBack (fun x state ->
        match chooser x with
          | ValueSome x -> x :: state
          | ValueNone -> state
      ) [] list

  module Map =
    let inline tryFind' key (table: Map<_, _>) =
      let mutable v = Unchecked.defaultof<_>
      if table.TryGetValue(key, &v) then
        ValueSome v
      else ValueNone

    let inline choose' c m =
      m |> Map.fold (
        fun newMap k v ->
          match c k v with
            | ValueSome x -> newMap |> Map.add k x
            | ValueNone   -> newMap
      ) Map.empty
