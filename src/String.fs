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

  let inline substringAfter i (str: string) = str.Substring i

  let inline substring startIndex endIndex (str: string) = str.Substring(startIndex, endIndex)

  let inline normalize (nfo: NormalizationForm option) (str: string) = 
    match nfo with Some nf -> str.Normalize nf | None -> str.Normalize()

  let inline toLower (ci: CultureInfo) (str: string) = str.ToLower ci

  let inline toLowerInvariant (str: string) = str.ToLowerInvariant()

  let inline toUpper (ci: CultureInfo) (str: string) = str.ToUpper ci

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

  let inline removeEmptyEntries (sp: string array) = sp |> Array.filter (String.IsNullOrEmpty >> not)

  let inline toChars (s: string) = s.ToCharArray()

  let inline ofChars (chars: #seq<char>) = System.String.Concat chars

  let inline nth i (str: string) = str.[i]

  let inline rev (str: string) = 
    new String(str.ToCharArray() |> Array.rev)

  let inline private whileBase pred act str =
    if String.IsNullOrEmpty str then
      ""
    else
      let mutable i = 0
      while i < String.length str && str |> nth i |> pred do i <- i + 1 done
      if i = 0 then ""
      else str |> act i

  let inline take i str =
    if i = 0 then ""
    else if i >= String.length str then str
    else removeAfter i str

  let inline skip i str =
    if i = 0 then str
    else if i >= String.length str then ""
    else substringAfter i str

  let inline takeWhile predicate (str: string) =
    whileBase predicate take str

  let inline skipWhile predicate (str: string) =
    whileBase predicate skip str
