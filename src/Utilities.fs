namespace Prelude

module Convert =
  let inline hexsToInt (hexs: #seq<char>) =
    let len = Seq.length hexs - 1
    hexs |> Seq.foldi (fun i sum x ->
      let n =
        let n = int x - int '0'
        if n < 10 then n
        else if n < 23 then n - 7
        else n - 44
      sum + n * pown 16 (len - i)) 0

  let inline digitsToInt (digits: #seq<char>) =
    let len = Seq.length digits - 1
    digits |> Seq.foldi (fun i sum x ->
      sum + (int x - int '0') * pown 10 (len - i)) 0

#if !NETSTANDARD1_6
module Shell =
  open System.Diagnostics

  let inline eval cmd args =
    use p = new Process()
    p.EnableRaisingEvents <- false
    p.StartInfo.UseShellExecute <- false
    p.StartInfo.FileName <- cmd
    p.StartInfo.Arguments <- args |> String.concat " "
    p.StartInfo.RedirectStandardInput <- true
    p.StartInfo.RedirectStandardOutput <- true
    p.Start() |> ignore
    p.WaitForExit()
    p.StandardOutput.ReadToEnd()

  let inline evalAsync cmd args =
    async {
      use p = new Process()
      do p.EnableRaisingEvents <- false
      do p.StartInfo.UseShellExecute <- false
      do p.StartInfo.FileName <- cmd
      do p.StartInfo.Arguments <- args |> String.concat " "
      do p.StartInfo.RedirectStandardInput <- true
      do p.StartInfo.RedirectStandardOutput <- true
      do p.Start() |> ignore
      do p.WaitForExit()
      return p.StandardOutput.ReadToEnd()
    }

  let inline pipe cmd args (stdin: string) =
    use p = new Process()
    p.EnableRaisingEvents <- false
    p.StartInfo.UseShellExecute <- false
    p.StartInfo.FileName <- cmd
    p.StartInfo.Arguments <- args |> String.concat " "
    p.StartInfo.RedirectStandardInput <- true
    p.StartInfo.RedirectStandardOutput <- true
    p.Start() |> ignore
    p.StandardInput.WriteLine stdin
    p.WaitForExit()
    p.StandardOutput.ReadToEnd()

  let inline pipeAsync cmd args (stdin: string) =
    async {
      use p = new Process()
      do p.EnableRaisingEvents <- false
      do p.StartInfo.UseShellExecute <- false
      do p.StartInfo.FileName <- cmd
      do p.StartInfo.Arguments <- args |> String.concat " "
      do p.StartInfo.RedirectStandardInput <- true
      do p.StartInfo.RedirectStandardOutput <- true
      do p.Start() |> ignore
      do! p.StandardInput.WriteLineAsync stdin
      do p.WaitForExit()
      return p.StandardOutput.ReadToEnd()
    }

  let inline run cmd args =
    use p = new Process()
    p.EnableRaisingEvents <- false
    p.StartInfo.UseShellExecute <- false
    p.StartInfo.FileName <- cmd
    p.StartInfo.Arguments <- args |> String.concat " "
    p.Start() |> ignore
    p.WaitForExit()
    p.ExitCode

  let inline runAsync cmd args =
    async {
      use p = new Process()
      do p.EnableRaisingEvents <- false
      do p.StartInfo.UseShellExecute <- false
      do p.StartInfo.FileName <- cmd
      do p.StartInfo.Arguments <- args |> String.concat " "
      do p.Start() |> ignore
      do p.WaitForExit() 
      return p.ExitCode
    }
#endif