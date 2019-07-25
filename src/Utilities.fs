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

