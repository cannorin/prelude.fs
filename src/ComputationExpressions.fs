namespace Prelude

[<AutoOpen>]
module ComputationExpressions =
  open System.Linq

  (*
    // boilerplate for strict monads to add delay/try

    member inline this.Delay f = f
    member inline this.Undelay f = f()
    member inline this.Run f = f()
    member inline this.TryWith (f, h) = try f() with exn -> h exn
    member inline this.TryFinally (f, h) = try f() finally h()
    

    // boilerplate for any monad to add for/while
    
    member inline this.Zero () = this.Return ()
    member inline this.Using (disp: #System.IDisposable, m) =
      this.TryFinally(
        this.Delay(fun () -> m disp),
        fun () -> dispose disp
      )
    member inline this.Combine (a, b) = this.Bind (a, fun () -> b)
    member inline this.While (cond, m) =
      let rec loop cond m =
        if cond () then this.Combine(this.Undelay m, loop cond m)
        else this.Zero ()
      loop cond m
    member inline this.For (xs: #seq<_>, exec) =
      this.Using(
        (xs :> seq<_>).GetEnumerator(),
        fun en ->
          this.While(
            en.MoveNext,
            this.Delay(fun () -> exec en.Current))
      )
  *)

  type OptionBuilder() =
    member inline this.Bind(m, f) = Option.bind f m
    member inline this.Return x = Some x
    member inline this.ReturnFrom x = x
    
    member inline this.Delay f = f
    member inline this.Undelay f = f()
    member inline this.Run f = f()
    member inline this.TryWith (f, h) = try f() with exn -> h exn
    member inline this.TryFinally (f, h) = try f() finally h()

    member inline this.Zero () = this.Return ()
    member inline this.Using (disp: #System.IDisposable, m) =
      this.TryFinally(
        this.Delay(fun () -> m disp),
        fun () -> dispose disp
      )
    member inline this.Combine (a, b) = this.Bind (a, fun () -> b)
    member inline this.While (cond, m) =
      let rec loop cond m =
        if cond () then this.Combine(this.Undelay m, loop cond m)
        else this.Zero ()
      loop cond m
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
    member inline this.ReturnFrom x = x
    
    member inline this.Delay f = f
    member inline this.Undelay f = f()
    member inline this.Run f = f()
    member inline this.TryWith (f, h) = try f() with exn -> h exn
    member inline this.TryFinally (f, h) = try f() finally h()
    
    member inline this.Zero () = this.Return ()
    member inline this.Using (disp: #System.IDisposable, m) =
      this.TryFinally(
        this.Delay(fun () -> m disp),
        fun () -> dispose disp
      )
    member inline this.Combine (a, b) = this.Bind (a, fun () -> b)
    member inline this.While (cond, m) =
      let rec loop cond m =
        if cond () then this.Combine(this.Undelay m, loop cond m)
        else this.Zero ()
      loop cond m
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

    member inline this.Delay f = this.Bind(this.Return (), f)
    member inline this.Undelay x = x
    member inline this.TryWith (m, f) =
      lazy (try Lazy.force m with exn -> f exn)
    member inline this.TryFinally (m, f) =
      lazy (try Lazy.force m finally f() )
    
    member inline this.Zero () = this.Return ()
    member inline this.Using (disp: #System.IDisposable, m) =
      this.TryFinally(
        this.Delay(fun () -> m disp),
        fun () -> dispose disp
      )
    member inline this.Combine (a, b) = this.Bind (a, fun () -> b)
    member inline this.While (cond, m) =
      let rec loop cond m =
        if cond () then this.Combine(this.Undelay m, loop cond m)
        else this.Zero ()
      loop cond m
    member inline this.For (xs: #seq<_>, exec) =
      this.Using(
        (xs :> seq<_>).GetEnumerator(),
        fun en ->
          this.While(
            en.MoveNext,
            this.Delay(fun () -> exec en.Current))
      )

  open System.Threading.Tasks
  type AsyncBuilder with
    member inline this.Bind(t:Task<'T>, f:'T -> Async<'R>) : Async<'R> = 
      async.Bind(Async.AwaitTask t, f)
    member inline this.Bind(t:Task, f:unit -> Async<'R>) : Async<'R> = 
      async.Bind(Async.AwaitTask t, f)

  [<RequireQualifiedAccess>]
  module Do =
    let option = OptionBuilder()
    let result = ResultBuilder()
    let lazy'  = LazyBuilder()

