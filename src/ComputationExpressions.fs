namespace Prelude

[<AutoOpen>]
module ComputationExpressions =
  open System
  open System.Linq

  (*
    // boilerplate for strict monads
    member inline this.Delay f = f : unit -> _
    member inline this.Undelay f = f()
    member inline this.Run f = f()
    member inline this.TryWith (f, h) = try f() with exn -> h exn
    member inline this.TryFinally (f, h) = try f() finally h()
    member inline this.Using (disp: #System.IDisposable, m) =
      this.TryFinally(
        this.Delay(fun () -> m disp),
        fun () -> dispose disp
      )
    member inline this.Combine (a, b) = this.Bind (a, fun () -> b ())
    member inline this.While (cond, body: unit -> _) =
      let rec loop cond m =
        if cond () then this.Combine(this.Undelay m, fun () -> loop cond m)
        else this.Zero ()
      loop cond body

    // boilerplate for delayed monads
    member inline this.Combine (a, b) = this.Bind (a, fun () -> b)
    member inline this.While (cond, m) =
      let rec loop cond m () =
        if cond () then this.Combine(this.Undelay m, loop cond m ())
        else this.Zero ()
      this.Delay (loop cond m

    // boilerplate for any monads
    member inline this.Yield x = this.Return x
    member inline this.Zero () = this.Return ()
    member inline this.For (xs: #seq<_>, exec) =
      this.Using(
        (xs :> seq<_>).GetEnumerator(),
        fun en ->
          this.While(
            en.MoveNext,
            this.Delay(fun () -> exec en.Current))
      )
  *)

  type NothingBuilder() =
    member inline this.Bind(m, f) = f m
    member inline this.Return x = x
    member inline this.ReturnFrom x = x
    member inline this.Delay f = f : unit -> _
    member inline this.Undelay f = f()
    member inline this.Run f = f()
    member inline this.TryWith (f, h) = try f() with exn -> h exn
    member inline this.TryFinally (f, h) = try f() finally h()
    member inline this.Using (disp: #System.IDisposable, m) =
      this.TryFinally(
        this.Delay(fun () -> m disp),
        fun () -> dispose disp
      )
    member inline this.Combine (a, b) = this.Bind (a, fun () -> b ())
    member inline this.Zero () = this.Return ()
    member inline this.While (cond, body: unit -> _) =
      let rec loop cond m =
        if cond () then this.Combine(this.Undelay m, fun () -> loop cond m)
        else this.Zero ()
      loop cond body
    member inline this.Yield x = this.Return x
    member inline this.For (xs: #seq<_>, exec) =
      this.Using(
        (xs :> seq<_>).GetEnumerator(),
        fun en ->
          this.While(
            en.MoveNext,
            this.Delay(fun () -> exec en.Current))
      )

  type OptionBuilder() =
    member inline this.Bind(m, f) = Option.bind f m
    member inline this.Return x = Some x
    member inline this.ReturnFrom (x: _ option) = x
    member inline this.Delay f = f : unit -> _
    member inline this.Undelay f = f()
    member inline this.Run f = f()
    member inline this.TryWith (f, h) = try f() with exn -> h exn
    member inline this.TryFinally (f, h) = try f() finally h()
    member inline this.Using (disp: #System.IDisposable, m) =
      this.TryFinally(
        this.Delay(fun () -> m disp),
        fun () -> dispose disp
      )
    member inline this.Combine (a, b) = this.Bind (a, fun () -> b ())
    member inline this.Zero () = this.Return ()
    member inline this.While (cond, body: unit -> _) =
      let rec loop cond m =
        if cond () then this.Combine(this.Undelay m, fun () -> loop cond m)
        else this.Zero ()
      loop cond body
    member inline this.Yield x = this.Return x
    member inline this.For (xs: #seq<_>, exec) =
      this.Using(
        (xs :> seq<_>).GetEnumerator(),
        fun en ->
          this.While(
            en.MoveNext,
            this.Delay(fun () -> exec en.Current))
      )

  type ResultBuilder() =
    member inline this.Bind(m, f) = Result.bind f m
    member inline this.Return x = Ok x
    member inline this.ReturnFrom (x: Result<_, _>) = x
    member inline this.Delay f = f : unit -> _
    member inline this.Undelay f = f()
    member inline this.Run f = f()
    member inline this.TryWith (f, h) = try f() with exn -> h exn
    member inline this.TryFinally (f, h) = try f() finally h()
    member inline this.Using (disp: #System.IDisposable, m) =
      this.TryFinally(
        this.Delay(fun () -> m disp),
        fun () -> dispose disp
      )
    member inline this.Combine (a, b) = this.Bind (a, fun () -> b ())
    member inline this.Zero () = this.Return ()
    member inline this.While (cond, body: unit -> _) =
      let rec loop cond m =
        if cond () then this.Combine(this.Undelay m, fun () -> loop cond m)
        else this.Zero ()
      loop cond body
    member inline this.Yield x = this.Return x
    member inline this.For (xs: #seq<_>, exec) =
      this.Using(
        (xs :> seq<_>).GetEnumerator(),
        fun en ->
          this.While(
            en.MoveNext,
            this.Delay(fun () -> exec en.Current))
      )

  type LazyBuilder() =
    member inline this.Bind(m, f) = Lazy.bind f m
    member inline this.Return x = lazy x
    member inline this.ReturnFrom m = m

    member inline this.Delay (f: unit -> Lazy<_>) = lazy f().Value
    member inline this.Undelay x = x
    member inline this.TryWith (m, f) =
      lazy (try Lazy.force m with exn -> f exn)
    member inline this.TryFinally (m, f) =
      lazy (try Lazy.force m finally f() )

    member inline this.Combine (a, b) = this.Bind (a, fun () -> b)
    member inline this.Zero () = this.Return ()
    member inline this.While (cond, m) =
      let rec loop cond m () =
        if cond () then this.Combine(this.Undelay m, loop cond m ())
        else this.Zero ()
      this.Delay (loop cond m)
    
    member inline this.Using (disp: #System.IDisposable, m) =
      this.TryFinally(
        this.Delay(fun () -> m disp),
        fun () -> dispose disp
      )
    member inline this.Yield x = this.Return x
    member inline this.For (xs: #seq<_>, exec) =
      this.Using(
        (xs :> seq<_>).GetEnumerator(),
        fun en ->
          this.While(
            en.MoveNext,
            this.Delay(fun () -> exec en.Current))
      )

  type NullSafeBuilder() =
    inherit OptionBuilder()
    member inline this.Bind (m: Nullable<'a>, f: 'a -> 'b option) =
      if m.HasValue then f m.Value else None
    member inline this.Bind (m: Result<'a, _>, f) =
      match m with Ok x -> f x | Error _ -> None
    member inline this.Bind (m: 'a, f: 'a -> 'b option) : _ when 'a: null =
      match m with null -> None | x -> f x

  open System.Threading.Tasks
  type AsyncBuilder with
    member inline this.Bind(t:Task<'T>, f:'T -> Async<'R>) : Async<'R> = 
      async.Bind(Async.AwaitTask t, f)
    member inline this.Bind(t:Task, f:unit -> Async<'R>) : Async<'R> = 
      async.Bind(Async.AwaitTask t, f)

  [<RequireQualifiedAccess>]
  module Do =
    let nothing = NothingBuilder()
    let option  = OptionBuilder()
    let result  = ResultBuilder()
    let lazily  = LazyBuilder()
    let nullsafe = NullSafeBuilder()
