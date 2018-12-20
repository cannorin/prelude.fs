namespace Prelude

module Path =
  open System
  open System.IO

  let combine x y = Path.Combine(x, y)

  let combineMany xs = Path.Combine <| Seq.toArray xs

  let makeRelativeTo parentDir file =
    let filePath = new Uri(file)
    let path =
      new Uri (
        if (parentDir |> String.endsWith (to_s Path.DirectorySeparatorChar) |> not) then
          sprintf "%s%c" parentDir Path.DirectorySeparatorChar
        else
          parentDir
      )
    Uri.UnescapeDataString(
      path.MakeRelativeUri(filePath)
      |> to_s
      |> String.replace '/' Path.DirectorySeparatorChar)