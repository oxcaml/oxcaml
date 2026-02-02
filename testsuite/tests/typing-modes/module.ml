(* TEST
    flags+="-extension mode_alpha";
   expect;
*)

let portable_use : 'a @ portable -> unit = fun _ -> ()

module type S = sig val x : 'a -> unit end

module type SL = sig type 'a t end

module M = struct
    type 'a t = int
    let x _ = ()
end
let (foo @ nonportable) () = ()
module F (X : S) = struct
    type t = int
    let x = X.x
    let _ = foo
end
[%%expect{|
val portable_use : 'a @ portable -> unit = <fun>
module type S = sig val x : 'a -> unit end
module type SL = sig type 'a t end
module M : sig type 'a t = int val x : 'a -> unit end @@ stateless
val foo : unit -> unit = <fun>
module F : functor (X : S) -> sig type t = int val x : 'a -> unit end
|}]

let u =
    let foo () =
        let module X = struct
            let x _ = ()
        end
        in
        let module R = F(X) in
        ()
    in
    portable_use foo
[%%expect{|
Line 10, characters 17-20:
10 |     portable_use foo
                      ^^^
Error: This value is "nonportable"
         because it closes over the module "F" at line 7, characters 23-24
         which is "nonportable"
         because it closes over the value "foo" at line 15, characters 12-15
         which is "nonportable".
       However, the highlighted expression is expected to be "portable".
|}]

let u =
    let foo () =
        let m = (module struct let x _ = () end : S) in
        let module M = (val m) in
        M.x
    in
    portable_use foo
[%%expect{|
val u : unit = ()
|}]

(* Packing produces first class modules at the same mode as the module *)
let () = portable_use (module M : S)
[%%expect{|
|}]

(* Unpacking produces module at the same mode as the first class module *)
let foo (m : (module S)) =
    let module M @ portable = (val m) in
    portable_use M.x
[%%expect{|
val foo : (module S) @ portable -> unit = <fun>
|}]

let foo () =
    let bar () =
        let _ : F(M).t = 42 in
        ()
    in
    let _ = (bar : _ @ portable) in
    ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo () =
    let bar () =
        let module _ : sig
            open M
        end = struct end
        in
        ()
    in
    let _ = (bar : _ @ portable) in
    ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo () =
    let bar () =
        let module _ : (sig
            module M' : sig  end
        end with module M' := M) = struct
        end
        in
        ()
    in
    let _ = (bar : _ @ portable) in
    ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* Replacing [:=] in the above example with [=] should work similarly, but I
   couldn't construct an example to test this properly. *)

let foo () =
    let bar () =
        let module _ : module type of M = struct
            type 'a t = int
            let x _ = ()
        end
        in
        ()
    in
    let _ = (bar : _ @ portable) in
    ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo () =
    let bar () =
        let module _ : (sig
            module M' := M
        end) = struct
        end
        in
        ()
    in
    let _ = (bar : _ @ portable) in
    ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* Pmty_alias is not testable *)

(* module alias *)
module type S = sig
    val foo : 'a -> 'a
    val baz : 'a -> 'a @@ portable
end

module M : S = struct
    let foo @ nonportable = fun x -> x
    let baz = fun x -> x
end
[%%expect{|
module type S = sig val foo : 'a -> 'a val baz : 'a -> 'a @@ portable end
module M : S @@ stateless nonportable
|}]

let (bar @ portable) () =
    let module N = M in
    M.baz ();
    N.baz ()
[%%expect{|
val bar : unit -> unit = <fun>
|}]

let (bar @ portable) () =
    let module N = M in
    N.foo ()
[%%expect{|
Line 3, characters 4-9:
3 |     N.foo ()
        ^^^^^
Error: The value "N.foo" is "nonportable"
       but is expected to be "portable"
         because it is used inside the function at lines 1-3, characters 21-12
         which is expected to be "portable".
|}]

let (bar @ portable) () =
    let module N = M in
    M.foo ()
[%%expect{|
Line 3, characters 4-9:
3 |     M.foo ()
        ^^^^^
Error: The value "M.foo" is "nonportable"
       but is expected to be "portable"
         because it is used inside the function at lines 1-3, characters 21-12
         which is expected to be "portable".
|}]

(* chained aliases. Creating alias of alias is fine. *)
let (bar @ portable) () =
    let module N = M in
    let module N' = N in
    M.baz ();
    N.baz ();
    N'.baz ()
[%%expect{|
val bar : unit -> unit = <fun>
|}]

(* locks are accumulated and not lost *)
let (bar @ portable) () =
    let module N = M in
    let module N' = N in
    N'.foo ()
[%%expect{|
Line 4, characters 4-10:
4 |     N'.foo ()
        ^^^^^^
Error: The value "N'.foo" is "nonportable"
       but is expected to be "portable"
         because it is used inside the function at lines 1-4, characters 21-13
         which is expected to be "portable".
|}]

(* module aliases in structures still walk locks. *)
let (bar @ portable) () =
    let module N = struct
        module L = M
    end in
    N.L.foo ()
[%%expect{|
Line 3, characters 19-20:
3 |         module L = M
                       ^
Error: The module "M" is "nonportable"
       but is expected to be "portable"
         because it is used inside the function at lines 1-5, characters 21-14
         which is expected to be "portable".
|}]

module F (X : S @ portable) = struct
end
[%%expect{|
module F : functor (X : S @ portable) -> sig end @@ stateless
|}]

module type S = functor () (M : S @ portable) (_ : S @ portable) -> S
[%%expect{|
module type S = functor () (M : S @ portable) -> S @ portable -> S
|}]

module type S = functor () (M : S) (_ : S) -> S @ portable
[%%expect{|
module type S = functor () (M : S) -> S -> S @ portable
|}]

module F () = struct
    let (foo @ yielding) () = ()
end
[%%expect{|
module F :
  functor () -> sig val foo : unit -> unit @@ stateless end @ yielding @@
  stateless
|}]

module type Empty = sig end
[%%expect{|
module type Empty = sig end
|}]

let _ =
    let module F (X : Empty) = struct end in
    let module M @ local = struct end in
    let module _ = F(M) in
    ()
[%%expect{|
Line 4, characters 19-23:
4 |     let module _ = F(M) in
                       ^^^^
Error: Modules do not match: sig end @ local is not included in
       Empty @ global
     Got "local" but expected "global".
|}]

let _ =
    let module F (X : Empty) (Y : Empty) = struct end in
    let module M = struct end in
    let module N @ local = struct end in
    let module _ = F(M)(N) in
    ()
[%%expect{|
Line 5, characters 19-26:
5 |     let module _ = F(M)(N) in
                       ^^^^^^^
Error: This application of the functor "F" is ill-typed.
       These arguments:
         M N
       do not match these parameters:
         functor (X : Empty) (Y : Empty) -> ...
       1. Module M matches the expected module type Empty
       2. Modules do not match:
            N : sig end @ local
          is not included in
            Empty @ global
          Got "local" but expected "global".
|}]

(* [include] should rebase modalities relative to the current structure *)
module Test_incl = struct
    module M = struct
        let foo x = x
    end
    module type S = module type of M
    (* [M] is portable, so inside [S] there is no [portable] modality on [foo] *)
    module N = struct
        let x  : int ref = ref 42
        let f () = x := 24
        (* [N] cannot be [portable] due to [f] *)

        include M
    end

    let () = portable_use N.foo
end
[%%expect{|
module Test_incl :
  sig
    module M : sig val foo : 'a -> 'a @@ stateless end
    module type S = sig val foo : 'a -> 'a @@ stateless end
    module N :
      sig
        val x : int ref @@ stateless
        val f : unit -> unit
        val foo : 'a -> 'a @@ stateless
      end
  end
|}]

let use_unique : 'a @ unique -> unit = fun _ -> ()

(* Functors are [many], and can't close over unique values*)

let foo (x @ unique) =
  let module Foo (_ : sig end) = struct
    let () = use_unique x
  end in
  let module _ = Foo(struct end) in
  ()
[%%expect{|
val use_unique : 'a @ unique -> unit = <fun>
Line 7, characters 24-25:
7 |     let () = use_unique x
                            ^
Error: This value is aliased but used as unique.
Hint: This value comes from outside the current module or class.
|}]

let foo (x @ unique) =
  let module Foo () = struct
    let () = use_unique x
  end in
  let module _ = Foo() in
  ()
[%%expect{|
Line 3, characters 24-25:
3 |     let () = use_unique x
                            ^
Error: This value is aliased but used as unique.
Hint: This value comes from outside the current module or class.
|}]

let (foo @ nonportable) () = ()

module (F @ portable) () = struct
    let bar = foo
end
[%%expect{|
val foo : unit -> unit = <fun>
Lines 3-5, characters 22-3:
3 | ......................() = struct
4 |     let bar = foo
5 | end
Error: The module is "nonportable"
         because it closes over the value "foo" at line 4, characters 14-17
         which is "nonportable".
       However, the module highlighted is expected to be "portable".
|}]

module (F @ portable) (X : sig val x : int -> int end) = struct
    let bar = X.x
end
[%%expect{|
module F :
  functor (X : sig val x : int -> int end) -> sig val bar : int -> int end @@
  stateless
|}]


module type S = sig
    module F (X : sig end) : sig end
    module G (X : sig end) : sig
        module type T = module type of (F (X))
    end
end
[%%expect{|
module type S =
  sig
    module F : functor (X : sig end) -> sig end
    module G : functor (X : sig end) -> sig module type T = sig end end
  end
|}]

module type S = sig
    module F (X : sig end) : sig end
    module rec M : sig
        module N : sig
        end
        include module type of F(N)
    end
end
[%%expect{|
module type S =
  sig
    module F : functor (X : sig end) -> sig end
    module rec M : sig module N : sig end end
  end
|}]

module rec Foo : sig
    val bar : unit -> unit
end = struct
include (Foo : module type of struct
    include Foo
end)
let (bar @ stateful) () = ()
end
[%%expect{|
module rec Foo : sig val bar : unit -> unit end
|}]

module type S = sig
    module type S = sig end

    module type Key = sig
    module M0 : S
    end

    module L : sig
    module M : Key

    module N : sig
        module Label : Key with M

        include sig
            module Key : S
        end
        with module Key = Label
    end
    end

    include sig
        module L' : S
    end
    with module L' = L

end
[%%expect{|
module type S =
  sig
    module type S = sig end
    module type Key = sig module M0 : S end
    module L :
      sig
        module M : Key
        module N :
          sig
            module Label : sig module M0 = M.M0 end
            module Key : sig module M0 = M.M0 end
          end
      end
    module L' :
      sig
        module M : sig module M0 : sig end end
        module N :
          sig
            module Label : sig module M0 = M.M0 end
            module Key : sig module M0 = M.M0 end
          end
      end
  end
|}]

(* CR zqian: fix [make_aliases_absent]. *)
(* CR lmaurer: Disabling this test until it is rewritten without a line number
   in it. *)
(*
module type S = sig
    module type S = sig end

    module type Key = sig
    module M0 : S
    end

    module L : sig
    module M : Key

    module N : sig
        module Label : Key with M

        include sig
            module Key : S
        end
        with module Key = Label
        @@ portable
    end
    end

    include sig
        module L' : S
    end
    with module L' = L
    @@ portable
end
[%%expect{|
Uncaught exception: File "typing/env.ml", line 2155, characters 13-19: Assertion failed

|}]
*)

(* Since uniqueness analysis is not implemented for modules,
   they are required to be at modes [aliased many]. *)
(* CR modes: implement uniqueness analysis for modules. *)

(* Cannot create [once] or [unique] modules *)
let _ =
    let module M @ once = struct end in
    ()
[%%expect{|
Line 2, characters 17-36:
2 |     let module M @ once = struct end in
                     ^^^^^^^^^^^^^^^^^^^
Error: The module is "once"
       but is expected to be "many"
         because modules are always required to be many.
|}]

let _ =
    let module M @ unique = struct end in
    ()
[%%expect{|
Line 2, characters 28-38:
2 |     let module M @ unique = struct end in
                                ^^^^^^^^^^
Error: The module is "aliased"
         because modules are always required to be aliased.
       However, the module highlighted is expected to be "unique".
|}]

module M @ once = struct end
[%%expect{|
Line 1, characters 9-28:
1 | module M @ once = struct end
             ^^^^^^^^^^^^^^^^^^^
Error: The module is "once"
       but is expected to be "many"
         because modules are always required to be many.
|}]

module M @ unique = struct end
[%%expect{|
Line 1, characters 20-30:
1 | module M @ unique = struct end
                        ^^^^^^^^^^
Error: The module is "aliased"
         because modules are always required to be aliased.
       However, the module highlighted is expected to be "unique".
|}]

(* Setup: Module type and functors for testing usage *)
module type Empty = sig end
module F_once (M : Empty @ once) = struct end
module F_unique (M : Empty @ unique) = struct end
module G (X : Empty) = struct end
[%%expect{|
module type Empty = sig end
module F_once : functor (M : Empty @ once) -> sig end @@ stateless
module F_unique : functor (M : Empty @ unique) -> sig end @@ stateless
module G : functor (X : Empty) -> sig end @@ stateless
|}]

(* Module alias - cannot use [once], cannot use as [unique] *)
module Alias_once (M : Empty @ once) = struct
    module N = M
end
[%%expect{|
Line 2, characters 15-16:
2 |     module N = M
                   ^
Error: The module is "once"
       but is expected to be "many"
         because modules are always required to be many.
|}]

module Alias_unique (M : Empty) = struct
    module N @ unique = M
end
[%%expect{|
Line 2, characters 24-25:
2 |     module N @ unique = M
                            ^
Error: The module is "aliased" but is expected to be "unique".
|}]

(* Pack into first-class module - cannot use [once], cannot use as [unique] *)
module Pack_once (M : Empty @ once) = struct
    let m = (module M : Empty)
end
[%%expect{|
Line 2, characters 20-21:
2 |     let m = (module M : Empty)
                        ^
Error: The module is "once"
       but is expected to be "many"
         because modules are always required to be many.
|}]

module Pack_unique (M : Empty @ unique) = struct
    let m @ unique = (module M : Empty)
end
[%%expect{|
Line 2, characters 21-39:
2 |     let m @ unique = (module M : Empty)
                         ^^^^^^^^^^^^^^^^^^
Error: This value is "aliased"
         because it is a module and thus required to be aliased.
       However, the highlighted expression is expected to be "unique".
|}]

(* Pass into functor as argument - cannot use [once], cannot use as [unique] *)
module Apply_once (M : Empty @ once) = struct
    module N = G(M)
end
[%%expect{|
Line 2, characters 17-18:
2 |     module N = G(M)
                     ^
Error: The module is "once"
       but is expected to be "many"
         because modules are always required to be many.
|}]

module Apply_unique (M : Empty @ unique) = struct
    module N = F_unique(M)
end
[%%expect{|
Line 2, characters 15-26:
2 |     module N = F_unique(M)
                   ^^^^^^^^^^^
Error: Modules do not match: sig end @ aliased is not included in
       Empty @ unique
     Got "aliased" because it is a module and thus required to be aliased.
     However, expected "unique".
|}]

(* Include - cannot use [once] *)
module Include_once (M : Empty @ once) = struct
    include M
end
[%%expect{|
Line 2, characters 12-13:
2 |     include M
                ^
Error: The module is "once"
       but is expected to be "many"
         because modules are always required to be many.
|}]

(* Including a [unique] module and trying to use a value as [unique] *)
module type With_string = sig val s : string end
module Include_unique (M : With_string @ unique) = struct
    include M
    let () = use_unique s
end
[%%expect{|
module type With_string = sig val s : string end
Line 4, characters 24-25:
4 |     let () = use_unique s
                            ^
Error: This value is "aliased" but is expected to be "unique".
|}]

(* Seemingly sound since [open] is just syntactic. *)
module Open_once (M : Empty @ once) = struct
    open M
    let x = ()
end
[%%expect{|
module Open_once :
  functor (M : Empty @ once) -> sig val x : unit @@ stateless end @@
  stateless
|}]

(* [open] keeps track of the origin. *)
module Use_twice (M : sig val f : unit -> unit @@ once end @ once) = struct
  open! M

  let a = f
  let b = M.f
end
[%%expect{|
Line 4, characters 10-11:
4 |   let a = f
              ^
Error: This value is once but used as many.
Hint: This value comes from another module or class.
|}]


(* Unpack first-class module - cannot use [once], cannot use as [unique] *)
let unpack_once (m : (module Empty) @ once) =
    let module M = (val m) in
    ()
[%%expect{|
Line 2, characters 24-25:
2 |     let module M = (val m) in
                            ^
Error: This value is once but used as many.
Hint: This value comes from outside the current module or class.
|}]

let unpack_unique (m : (module Empty) @ unique) =
    let module M @ unique = (val m) in
    ()
[%%expect{|
Line 2, characters 28-35:
2 |     let module M @ unique = (val m) in
                                ^^^^^^^
Error: The module is "aliased"
         because modules are always required to be aliased.
       However, the module highlighted is expected to be "unique".
|}]

(* [module type of] is type-level allowed on [once] modules. *)
module Typeof_once (M : Empty @ once) = struct
    module type T = module type of M
end
[%%expect{|
module Typeof_once :
  functor (M : Empty @ once) -> sig module type T = Empty end @@ stateless
|}]

(* Referring to a type from a [once] module is ok *)
module type With_type = sig type t end
module Use_type_once (M : With_type @ once) = struct
    type t = M.t
end
[%%expect{|
module type With_type = sig type t end
module Use_type_once : functor (M : With_type @ once) -> sig type t = M.t end
  @@ stateless
|}]
