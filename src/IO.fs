namespace Prelude
open System
open System.IO

module Path =
  let inline combine x y = Path.Combine(x, y)

  let inline combineMany xs = Path.Combine <| Seq.toArray xs

  let inline makeRelativeTo parentDir file =
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

  let inline changeExtensionTo extension path =
    Path.ChangeExtension(path, extension)

module File =
  let inline isHidden path =
    File.GetAttributes(path).HasFlag(FileAttributes.Hidden)

module Directory =
  let inline isHidden dir =
    DirectoryInfo(dir).Attributes.HasFlag(FileAttributes.Hidden)
  
  let inline enumerateFilesRecursively includeHidden dir =
    let rec enumRec dir =
      seq {
        for x in Directory.EnumerateFiles dir do
          if includeHidden || not (File.isHidden x) then
            yield x
        for subdir in Directory.EnumerateDirectories dir do
          if includeHidden || not (isHidden subdir) then
            yield! enumRec subdir
      }
    enumRec dir