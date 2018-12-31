namespace Prelude

module ValueOption =
  let inline get option =
    match option with ValueNone -> invalidArg "option" "value was ValueNone" | ValueSome x -> x
  
  let inline isSome option = match option with ValueNone -> false | ValueSome _ -> true

  let inline isNone option = match option with ValueNone -> true  | ValueSome _ -> false

  let inline defaultValue value option = match option with ValueNone -> value | ValueSome v -> v

  let inline defaultWith defThunk option = match option with ValueNone -> defThunk () | ValueSome v -> v

  let inline orElse ifNone option = match option with ValueNone -> ifNone | ValueSome _ -> option

  let inline orElseWith ifNoneThunk option = match option with ValueNone -> ifNoneThunk () | ValueSome _ -> option

  let inline count option = match option with ValueNone -> 0 | ValueSome _ -> 1

  let inline fold<'T,'State> folder (state:'State) (option: voption<'T>) = match option with ValueNone -> state | ValueSome x -> folder state x

  let inline foldBack<'T,'State> folder (option: voption<'T>) (state:'State) =  match option with ValueNone -> state | ValueSome x -> folder x state

  let inline exists predicate option = match option with ValueNone -> false | ValueSome x -> predicate x

  let inline forall predicate option = match option with ValueNone -> true | ValueSome x -> predicate x

  let inline contains value option = match option with ValueNone -> false | ValueSome v -> v = value

  let inline iter action option = match option with ValueNone -> () | ValueSome x -> action x

  let inline map mapping option = match option with ValueNone -> ValueNone | ValueSome x -> ValueSome (mapping x)

  let inline map2 mapping option1 option2 = 
    match option1, option2 with
    | ValueSome x, ValueSome y -> ValueSome (mapping x y)
    | _ -> ValueNone

  let inline map3 mapping option1 option2 option3 = 
    match option1, option2, option3 with
    | ValueSome x, ValueSome y, ValueSome z -> ValueSome (mapping x y z)
    | _ -> ValueNone

  let inline bind binder option = match option with ValueNone -> ValueNone | ValueSome x -> binder x

  let inline flatten option = match option with ValueNone -> ValueNone | ValueSome x -> x

  let inline filter predicate option = match option with ValueNone -> ValueNone | ValueSome x -> if predicate x then ValueSome x else ValueNone

  let inline toArray option = match option with  ValueNone -> [| |] | ValueSome x -> [| x |]

  let inline toList option = match option with  ValueNone -> [ ] | ValueSome x -> [ x ]

  let inline toNullable option = match option with ValueNone -> System.Nullable() | ValueSome v -> System.Nullable(v)

  let inline ofNullable (value:System.Nullable<'T>) = if value.HasValue then ValueSome value.Value else ValueNone

  let inline ofObj value = match value with null -> ValueNone | _ -> ValueSome value

  let inline toObj value = match value with ValueNone -> null | ValueSome x -> x

  let inline ofOption option = match option with Some x -> ValueSome x | None -> ValueNone

  let inline toOption option = match option with ValueSome x -> Some x | ValueNone -> None

module UseValueOptionAsDefault =
  let inline Some x = ValueSome x
  let inline None<'a> : voption<'a> = ValueNone
  let (|Some|None|) = function
    | ValueSome x -> Some x
    | ValueNone   -> None
  module Option = ValueOption

[<AutoOpen>]
module ValueOptionExtension =
  open UseValueOptionAsDefault
  type ValueOption<'T> with
    member inline this.IsSome = ValueOption.isSome this
    member inline this.IsNone = ValueOption.isNone this

  open System.Collections.Generic
  
  module Array =
    let inline tryLast' (array: _[]) =
      if array.Length = 0 then None
      else Some array.[array.Length-1]

    let inline tryHead' (array: _[]) =
      if array.Length = 0 then None
      else Some array.[0]

    let inline tryTake' length xs =
      if Array.length xs > length then
        Array.take length xs |> Some
      else if Array.length xs = length then Some xs
      else None

    let inline tryItem' index (array: _[]) =
      if index < 0 || index >= array.Length then None
      else Some (array.[index])
    
    let inline tryFind' predicate (array: _[]) =
      let rec loop i =
        if i >= array.Length then None else
        if predicate array.[i] then Some array.[i] else loop (i+1)
      loop 0

    let inline tryFindIndex' predicate (array: _[]) =
      let len = array.Length
      let rec go n = if n >= len then None elif predicate array.[n] then Some n else go (n+1)
      go 0


  module IEnumerator =
    let inline tryItem' index (e : IEnumerator<'T>) =
      let rec loop index =
        if not (e.MoveNext()) then None
        elif index = 0 then Some(e.Current)
        else loop (index-1)
      loop index
    
  module Seq =
    let inline tryLast' (source : seq<_>) =
      use e = source.GetEnumerator()
      if e.MoveNext() then
        let mutable res = e.Current
        while (e.MoveNext()) do res <- e.Current
        Some res
      else
        None

    let inline tryHead' (source : seq<_>) =
      use e = source.GetEnumerator()
      if (e.MoveNext()) then Some e.Current
      else None

    let inline tryTake' length xs =
      let xs' = xs |> Seq.indexed |> Seq.cache
      if xs' |> Seq.exists (fst >> ((=) (length - 1))) then
        xs' |> Seq.take length |> Seq.map snd |> Some
      else None

    let inline tryItem' index (source : seq<'T>) =
      if index < 0 then None else
      use e = source.GetEnumerator()
      IEnumerator.tryItem' index e
    
    let inline tryFind' predicate (source : seq<'T>)  =
      use e = source.GetEnumerator()
      let mutable res = None
      while (ValueOption.isNone res && e.MoveNext()) do
        let c = e.Current
        if predicate c then res <- Some(c)
      res

    let inline tryFindIndex' predicate (source:seq<_>) =
      use ie = source.GetEnumerator()
      let rec loop i =
        if ie.MoveNext() then
          if predicate ie.Current then
            Some i
          else loop (i+1)
        else
          None
      loop 0
  
  module List =
    let inline tryLast' (list: 'T list) =
      let rec loop list =
        match list with
        | [x] -> Some x
        | _ :: tail -> loop tail
        | [] -> None
      loop list

    let inline tryHead' list = match list with x::_ -> Some x | [] -> None

    let inline tryItem' index list =
      let rec loop index list =
        match list with
        | h::t when index >= 0 ->
            if index = 0 then Some h else loop (index - 1) t
        | _ -> None
      loop index list

    let inline tryTake' length xs =
      if List.length xs >= length then
        List.take length xs |> Some
      else None

    let inline tryFind' predicate list =
      let rec loop list =
        match list with
        | [] -> None
        | h::t -> if predicate h then Some h else loop t
      loop list

    let inline tryFindIndex' predicate list = 
      let rec loop n = function[] -> None | h::t -> if predicate h then Some n else loop (n+1) t
      loop 0 list

  module Map =
    let inline tryFind' key (table: Map<_, _>) =
      let mutable v = Unchecked.defaultof<_>
      if table.TryGetValue(key, &v) then
        Some v
      else None

    let inline choose' c m =
      m |> Map.fold (
        fun newMap k v ->
          match c k v with
            | ValueSome x -> newMap |> Map.add k x
            | ValueNone   -> newMap
      ) Map.empty
