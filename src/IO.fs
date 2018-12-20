namespace Prelude
open System
open System.IO

module Path =

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

module File =
  let inline isHidden path =
    File.GetAttributes(path).HasFlag(FileAttributes.Hidden)

module Directory =
  let inline isHidden dir =
    DirectoryInfo(dir).Attributes.HasFlag(FileAttributes.Hidden)
  
  let rec enumerateFilesRecursively includeHidden dir =
    seq {
      for x in Directory.EnumerateFiles dir do
        if includeHidden || not (File.isHidden x) then
          yield x
      for subdir in Directory.EnumerateDirectories dir do
        if includeHidden || not (isHidden subdir) then
          yield! enumerateFilesRecursively includeHidden subdir
    }