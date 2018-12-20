namespace Prelude

[<AutoOpen>]
module Patterns =
  open System.Text.RegularExpressions

  let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

  let (|DefaultValue|) dv x =
    match x with
      | Some v -> v
      | None -> dv
  
  [<AutoOpen>]
  module Kvp =
    open System.Collections.Generic
    type kvp<'a, 'b> = KeyValuePair<'a, 'b>
    let inline KVP (a, b) = kvp(a, b)
    let (|KVP|) (x: kvp<_, _>) = (x.Key, x.Value)

  [<AutoOpen>]
  module Nat =
    type nat = uint32
    let inline S i = i + 1u
    [<Literal>]
    let Z = 0u
    let (|S|Z|) i = if i = 0u then Z else S (i-1u)