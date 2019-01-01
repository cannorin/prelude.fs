namespace Prelude
open System.Threading.Tasks

type task<'t> = Task<'t>
type plaintask = Task

module Task =
  let inline run (t: _ task) = t.Result

  let inline runThunked (thunk: unit -> _ task) =thunk().Result

  let inline tryRun (t: _ task) =
    try Ok <| run t with e -> Error e

  let inline tryRunThunked (thunk: unit -> _ task) =
    try Ok <| runThunked thunk with e -> Error e

  let inline bind (f: 'a -> 'b task) (t: 'a task) : 'b task =
    t.ContinueWith(fun (x: _ task) -> f x.Result).Unwrap()

  let inline returnValue x : _ task=
    let s = TaskCompletionSource()
    s.SetResult x
    s.Task

  let inline returnThunk thunk = new Task<_>(fun () -> thunk ())

  let inline map (f: 'a -> 'b) (t: 'a task) : 'b task =
    t.ContinueWith(fun (x: _ task) -> f x.Result)

  let inline catch (t: _ task) : _ task =
    returnThunk (fun () -> try Ok t.Result with e -> Error e)

  let inline ofAsync (a: Async<_>) = a |> Async.StartAsTask

  let inline ofAsyncThunked (a: Async<_>) () = a |> Async.StartAsTask

  let inline toAsync (t: _ task) = Async.AwaitTask t

  let inline toAsyncThunked (thunk: unit -> _ task) = async { return runThunked thunk }

  let inline ignore (t: _ task) : unit task = t |> map ignore

  let inline ignorePlain (t: _ task) : plaintask = t :> plaintask

  let inline isCompleted (t: #plaintask) = t.IsCompleted
  let inline isCanceled  (t: #plaintask) = t.IsCanceled
  let inline isFaulted   (t: #plaintask) = t.IsFaulted
  let inline exn         (t: #plaintask) = match t.Exception with null -> None | exn -> Some exn
