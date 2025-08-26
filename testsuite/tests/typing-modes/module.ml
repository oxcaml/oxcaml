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
Error: This value is "nonportable" but expected to be "portable".
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
Error: The value "N.foo" is nonportable, so cannot be used inside a function that is portable.
|}]

let (bar @ portable) () =
    let module N = M in
    M.foo ()
[%%expect{|
Line 3, characters 4-9:
3 |     M.foo ()
        ^^^^^
Error: The value "M.foo" is nonportable, so cannot be used inside a function that is portable.
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
Error: The value "N'.foo" is nonportable, so cannot be used inside a function that is portable.
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
Error: The module "M" is nonportable, so cannot be used inside a function that is portable.
|}]

module F (X : S @ portable) = struct
end
[%%expect{|
Line 1, characters 18-26:
1 | module F (X : S @ portable) = struct
                      ^^^^^^^^
Error: Mode annotations on functor parameters are not supported yet.
|}]

module type S = functor () (M : S @ portable) (_ : S @ portable) -> S
[%%expect{|
Line 1, characters 36-44:
1 | module type S = functor () (M : S @ portable) (_ : S @ portable) -> S
                                        ^^^^^^^^
Error: Mode annotations on functor parameters are not supported yet.
|}]

module type S = functor () (M : S) (_ : S) -> S @ portable
[%%expect{|
Line 1, characters 50-58:
1 | module type S = functor () (M : S) (_ : S) -> S @ portable
                                                      ^^^^^^^^
Error: Mode annotations on functor return are not supported yet.
|}]

module F () = struct
    let (foo @ once) () = ()
end
[%%expect{|
Line 2, characters 9-12:
2 |     let (foo @ once) () = ()
             ^^^
Error: This is "once", but expected to be "many" because it is inside a "many" structure.
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
Error: Modules do not match: sig end (* in a structure at local *)
     is not included in Empty (* in a structure at global *)
     This escapes its region.
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
            N : sig end (* in a structure at local *)
          is not included in
            Empty (* in a structure at global *)
          This escapes its region.
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
Line 4, characters 14-17:
4 |     let bar = foo
                  ^^^
Error: The value "foo" is nonportable, so cannot be used inside a functor that is portable.
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
