namespace Prelude

module String =
  open System
  open System.Text
  open System.Globalization

  let inline startsWith (s: ^a) (str: ^String) : bool = (^String: (member StartsWith: ^a -> bool) str, s)

  let inline endsWith (s: ^a) (str: ^String) : bool = (^String: (member EndsWith: ^a -> bool) str, s)

  let inline contains (s: ^a) (str: ^String) : bool = (^String: (member IndexOf: ^a -> int) str, s) <> -1

  let inline findIndex (q: ^T) (str: ^String) = 
    (^String: (member IndexOf: ^T -> int) (str, q))

  let inline findIndexAfter (q: ^T) i (str: ^String) = 
    (^String: (member IndexOf: ^T -> int -> int) (str, q, i))

  let inline findLastIndex (q: ^T) (str: ^String) = 
    (^String: (member LastIndexOf: ^T -> int) (str, q))

  let inline findLastIndexAfter (q: ^T) i (str: ^String) = 
    (^String: (member LastIndexOf: ^T -> int -> int) (str, q, i))

  let inline insertAt s i (str: string) = str.Insert(i, s)

  let inline removeAfter i (str: string) = str.Remove i

  let inline remove startIndex endIndex (str: string) = str.Remove(startIndex, endIndex)

  let inline stringf format (x: ^a) = (^a : (member ToString: string -> string) (x, format))

  let inline substringAfter i (str: string) = str.Substring i

  let inline substring startIndex endIndex (str: string) = str.Substring(startIndex, endIndex)

#if !NETSTANDARD1_6
  let inline normalize (str: string) = str.Normalize()
  
  let inline normalizeWithForm (nf: NormalizationForm) (str: string) = 
    str.Normalize nf

  let inline toLower (ci: CultureInfo) (str: string) = str.ToLower ci

  let inline toUpper (ci: CultureInfo) (str: string) = str.ToUpper ci
#else
  let inline toLower (ci: CultureInfo) (str: string) = ci.TextInfo.ToLower str

  let inline toUpper (ci: CultureInfo) (str: string) = ci.TextInfo.ToUpper str
#endif

  let inline toLowerInvariant (str: string) = str.ToLowerInvariant()

  let inline toUpperInvariant (str: string) = str.ToUpperInvariant()

  let inline padLeft i (str: string) = str.PadLeft i

  let inline padLeftBy i c (str: string) = str.PadLeft(i, c)
  
  let inline padRight i (str: string) = str.PadRight i

  let inline padRightBy i c (str: string) = str.PadRight(i, c)

  let inline trim (str: string) = str.Trim()

  let inline trimStart (str: string) = str.TrimStart()

  let inline trimEnd (str: string) = str.TrimEnd()

  let inline trimBy (trimChar: char) (str: string) = str.Trim(trimChar)
  
  let inline trimBySeq (trimChars: char seq) (str: string) = str.Trim(trimChars |> Seq.toArray)

  let inline trimStartBy (trimChar: char) (str: string) = str.TrimStart(trimChar)
  
  let inline trimStartBySeq (trimChars: char seq) (str: string) = str.TrimStart(trimChars |> Seq.toArray)

  let inline trimEndBy (trimChar: char) (str: string) = str.TrimEnd(trimChar)
  
  let inline trimEndBySeq (trimChars: char seq) (str: string) = str.TrimEnd(trimChars |> Seq.toArray)

  let inline replace (before: ^T) (after: ^T) (s: ^String) =
    (^String: (member Replace: ^T -> ^T -> ^String) (s, before, after))

  let inline split (sp: ^T) (s: ^String) =
    (^String: (member Split: ^T array -> StringSplitOptions -> ^String array) (s, [|sp|], StringSplitOptions.None))

  let inline splitSeq (sp: ^T seq) (s: ^String) =
    (^String: (member Split: ^T array -> StringSplitOptions -> ^String array) (s, Seq.toArray sp, StringSplitOptions.None))

  let inline splitSkipEmpty (sp: ^T) (s: ^String) =
    (^String: (member Split: ^T array -> StringSplitOptions -> ^String array) (s, [|sp|], StringSplitOptions.RemoveEmptyEntries))

  let inline splitSeqSkipEmpty (sp: ^T seq) (s: ^String) =
    (^String: (member Split: ^T array -> StringSplitOptions -> ^String array) (s, Seq.toArray sp, StringSplitOptions.RemoveEmptyEntries))

  let inline toChars (s: string) = s.ToCharArray()

  let inline ofChars (chars: #seq<char>) = System.String.Concat chars

  let inline item i (str: string) = str.[i]

  let inline tryItem (index: int) (source: string) =
    if index >= 0 && index < source.Length then Some source.[index] else None

  let inline rev (str: string) = 
    new String(str.ToCharArray() |> Array.rev)

  let inline private whileBase pred act str =
    if String.IsNullOrEmpty str then
      ""
    else
      let mutable i = 0
      while i < String.length str && str |> item i |> pred do i <- i + 1 done
      if i = 0 then ""
      else str |> act i

  let inline take count (source: string) = source.[..count-1]

  let inline skip count (source: string) = source.[count..]

  let inline takeWhile predicate (str: string) =
    whileBase predicate take str

  let inline skipWhile predicate (str: string) =
    whileBase predicate skip str

  let inline truncate count (source: string) =
    if count < 1 then String.Empty
    else if String.length source <= count then source
    else take count source

  let inline drop     count (source: string) =
    if count < 1 then source
    else if String.length source >= count then String.Empty
    else skip count source

  let inline build (builder: StringBuilder -> unit) =
    let sb = new StringBuilder()
    builder sb
    sb.ToString()

[<AutoOpen>]
module StringExtensions =
  open System.Text

  type StringBuilder with
    member inline this.printf format =
      Printf.kprintf (fun s -> this.Append s |> ignore) format

    member inline this.printfn format =
      Printf.kprintf (fun s -> this.AppendLine s |> ignore) format
