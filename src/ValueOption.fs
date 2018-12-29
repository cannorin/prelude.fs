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

[<AutoOpen>]
module ValueOptionExtension =
  type ValueOption<'T> with
    member inline this.IsSome = ValueOption.isSome this
    member inline this.IsNone = ValueOption.isNone this
