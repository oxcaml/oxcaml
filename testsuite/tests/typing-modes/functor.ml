(* TEST
  flags+="-extension mode_alpha";
  expect;
*)

let use_portable (_ @ portable) = ()
[%%expect{|
val use_portable : 'a @ portable -> unit = <fun>
|}]

module type S = sig
    val f : unit -> unit
end
[%%expect{|
module type S = sig val f : unit -> unit end
|}]

module type T = sig
    val g : unit -> unit
end

(* functor parameter's unspecified mode axes default to legacy *)
module F (M : S) = struct
    let () = use_portable M.f
end
[%%expect{|
module type T = sig val g : unit -> unit end
Line 7, characters 26-29:
7 |     let () = use_portable M.f
                              ^^^
Error: This value is "nonportable" but is expected to be "portable".
|}]

module F (M : S) @ portable = struct
    let g = M.f
end
[%%expect{|
Line 2, characters 8-9:
2 |     let g = M.f
            ^
Error: This is "nonportable", but expected to be "portable" because it is inside a "portable" structure.
|}]

module F (M : S) : T @ portable = struct
    let g = M.f
end
[%%expect{|
Line 2, characters 8-9:
2 |     let g = M.f
            ^
Error: This is "nonportable", but expected to be "portable" because it is inside a "portable" structure.
|}]

module F : S -> T = functor (M : S) -> struct
  let g = M.f
end
[%%expect{|
module F : S -> T @@ stateless
|}]

module F (M : S @ portable) (M' : S) = struct
end
[%%expect{|
module F : functor (M : S @ portable) (M' : S) -> sig end @@ stateless
|}]

module F (M : S) (M' : S @ portable) @ portable = struct
end
[%%expect{|
module F : functor (M : S) (M' : S @ portable) -> sig end @@ stateless
|}]

module F (M : S @ portable) (M' : S @ portable) : T @ portable = struct
  let g = M.f
end
[%%expect{|
module F : functor (M : S @ portable) (M' : S @ portable) -> T @@ stateless
|}]

(* In REPL (called "toplevel" in the compiler source code), functors, just like
functions, will have their parameter mode and return mode zapped to legacy. In
the example below, the functor's return mode is zapped to [nonportable], even
though the implementation allows returning [portable]. *)
module F (M : S @ portable) @ portable = struct
    let g = M.f
    let () = use_portable M.f
    let () = use_portable g
end
[%%expect{|
module F : functor (M : S @ portable) -> sig val g : unit -> unit end
|}]

(* ..and as a result, the following is mode error *)
module M = struct
    let f () = ()
end
[%%expect{|
module M : sig val f : unit -> unit end @@ stateless
|}]

module _ @ portable = F(M)
[%%expect{|
Line 1, characters 22-26:
1 | module _ @ portable = F(M)
                          ^^^^
Error: This is "nonportable", but expected to be "portable".
|}]

(* To avoid zapping, explicitly annotate the functor type like below. *)
module F : S @ portable -> T @ portable = functor (M : S @ portable) -> struct
    let g = M.f
    let () = use_portable M.f
    let () = use_portable g
end
[%%expect{|
module F : S @ portable -> T @ portable
|}]

module M' @ portable = F(M)
[%%expect{|
module M' : sig val g : unit -> unit end @@ portable
|}]

(* Alternatively, define the functor and the use-sites in a structure.
The use-site will constrain the functor's type as needed. *)
module Workaround = struct
  module F (M : S @ portable) = struct
      let g = M.f
  end
  module M' @ portable = F(M)
end
[%%expect{|
module Workaround :
  sig
    module F :
      functor (M : S @ portable) -> sig val g : unit -> unit end @ portable
      @@ stateless nonportable
    module M' : sig val g : unit -> unit end
  end @@ portable
|}]

let () =
    let module M' @ portable = F (M) in
    use_portable M'.g
[%%expect{|
|}]

module M = struct
    let f () = ()
    let f' = let r = ref 42 in fun () -> r := 24; ()
end
[%%expect{|
module M : sig val f : unit -> unit @@ stateless val f' : unit -> unit end
|}]

(* Note that M is a nonportable module containing both portable and nonportable items.
However, F only needs the item [f], which is portable. *)
let () =
    let module M' = F (M) in
    ()
[%%expect{|
|}]

module M = struct
    let f = let r = ref 42 in fun () -> r := 24; ()
end
[%%expect{|
module M : sig val f : unit -> unit end
|}]

let () =
    let module M' = F (M) in
    ()
[%%expect{|
Line 2, characters 20-25:
2 |     let module M' = F (M) in
                        ^^^^^
Error: Modules do not match: sig val f : unit -> unit end @ nonportable
     is not included in S @ portable
     Values do not match:
       val f : unit -> unit (* in a structure at nonportable *)
     is not included in
       val f : unit -> unit (* in a structure at portable *)
     The left-hand side is "nonportable"
     because it contains a usage (of the value "r" at Line 2, characters 40-41)
     which is expected to be "uncontended".
     However, the right-hand side is "portable".
|}]

(* testing generative functor *)
let () =
    let module F () : sig
        val f : unit -> unit
    end = struct
        let f () = ()
    end in
    let module M = F () in
    use_portable M.f
[%%expect{|
|}]

(* testing include functor in signature *)
module type FT = S @ portable -> T @ portable

module type U = sig
    val f : unit -> unit
    include functor FT
end
[%%expect{|
module type FT = S @ portable -> T @ portable
Line 5, characters 20-22:
5 |     include functor FT
                        ^^
Error: Signature mismatch in included functor's parameter:
       Values do not match:
         val f : unit -> unit (* in a structure at nonportable *)
       is not included in
         val f : unit -> unit (* in a structure at portable *)
       The left-hand side is "nonportable"
       but the right-hand side is "portable".
|}]

module type U = sig
    val f : unit -> unit @@ portable
    val g : unit -> unit
    include functor FT
end
[%%expect{|
module type U =
  sig val f : unit -> unit @@ portable val g : unit -> unit @@ portable end
|}]

(* CR-soon zqian: the following should be allowed - [transl_signature] should
   know that the signature is on a portable structure. *)
module F (M : sig val f : unit -> unit include functor FT end @ portable) = struct
end
[%%expect{|
Line 1, characters 55-57:
1 | module F (M : sig val f : unit -> unit include functor FT end @ portable) = struct
                                                           ^^
Error: Signature mismatch in included functor's parameter:
       Values do not match:
         val f : unit -> unit (* in a structure at nonportable *)
       is not included in
         val f : unit -> unit (* in a structure at portable *)
       The left-hand side is "nonportable"
       but the right-hand side is "portable".
|}]

(* CR-soon zqian: the following should be allowed *)
module Foo : sig val f : unit -> unit include functor FT end @ portable = struct
end
[%%expect{|
Line 1, characters 54-56:
1 | module Foo : sig val f : unit -> unit include functor FT end @ portable = struct
                                                          ^^
Error: Signature mismatch in included functor's parameter:
       Values do not match:
         val f : unit -> unit (* in a structure at nonportable *)
       is not included in
         val f : unit -> unit (* in a structure at portable *)
       The left-hand side is "nonportable"
       but the right-hand side is "portable".
|}]


(* include functor in structure *)
module M = struct
    let f = let r = ref 42 in fun () -> r := 24; ()
    include functor F
end
[%%expect{|
Line 3, characters 20-21:
3 |     include functor F
                        ^
Error: Signature mismatch in included functor's parameter:
       Values do not match:
         val f : unit -> unit (* in a structure at nonportable *)
       is not included in
         val f : unit -> unit (* in a structure at portable *)
       The left-hand side is "nonportable"
       because it contains a usage (of the value "r" at Line 2, characters 40-41)
       which is expected to be "uncontended".
       However, the right-hand side is "portable".
|}]

module M = struct
    let f () = ()
    let f' = let r = ref 42 in fun () -> r := 24; ()
    include functor F
end
[%%expect{|
module M :
  sig
    val f : unit -> unit @@ stateless
    val f' : unit -> unit
    val g : unit -> unit @@ portable
  end
|}]

(* Test currying constraints on modes.

   This section compares functions and functors in similar situations. *)

(* Partial application: given [f : (A -> B -> C) @ mode], we assume that the
   the closure for [f x] contains a pointer to [f] and therfore must close over the [mode].
   Only relevant for comonadic axes since functions cross monadic axes. *)

let f (k : (unit -> unit -> unit) @ once ) =
  let (k' @ many) = k () in
  k' ()
[%%expect{|
Line 2, characters 20-24:
2 |   let (k' @ many) = k () in
                        ^^^^
Error: This value is "once" but is expected to be "many".
|}]

(* However, for [f : (A -> (B -> C)) @ once], we presume that [f] itself returns
   a new closure which can be applied [many] times. *)

let f' (k : (unit -> (unit -> unit)) @ once ) =
  let (k' @ many) = k () in
  k' ()
[%%expect{|
val f' : (unit -> (unit -> unit)) @ once -> unit = <fun>
|}]

(* CR modes: Functors don't have yet syntactic arity,
   and for them we must pick one single behavior.

   We pick the former, more restrictive behavior, for reasons given below.

   Internal ticket 1534. *)

module F (K : (functor () () -> sig end) @ once) = struct
  module (K' @ many) = K ()
end
[%%expect{|
Line 2, characters 23-27:
2 |   module (K' @ many) = K ()
                           ^^^^
Error: This is "once", but expected to be "many".
|}]

(* For functions we can infer the more permissive choice. *)

let () =
  let (f @ once) () @ many = fun () -> () in
  let (_f @ many) = f () in
  ()
[%%expect{|
|}]

let () =
  let (f @ once) : (unit -> unit -> unit) = fun () () -> () in
  let (_f @ many) = f () in
  ()
[%%expect{|
Line 3, characters 20-24:
3 |   let (_f @ many) = f () in
                        ^^^^
Error: This value is "once" but is expected to be "many".
|}]

(* Functor behavior here is stricter. *)

let () =
  (* CR modes: this line should raise the error eagerly.

     Internal ticket 6260. *)
  let module (F @ once) () @ many = (functor () -> struct end) in
  let module (M' @ many) = F () in
  ()
[%%expect{|
Line 6, characters 27-31:
6 |   let module (M' @ many) = F () in
                               ^^^^
Error: This is "once", but expected to be "many".
|}]

(* Closing over arguments: given [f : A -> B -> C] and [a : A @ mode], [f a] must close
   over [a]. The multi-argument function syntax handles this implicitly.  *)

let f_once (x @ once) () = ()
[%%expect{|
val f_once : 'a @ once -> unit -> unit = <fun>
|}]

(* CR modes: For functor, the behavior is the same but explicit
   since currying is not yet implemented.

   Internal ticket 6259. *)

module F (M : S @ once) () = struct end
[%%expect{|
module F : functor (M : S @ once) -> (functor () -> sig end) @ once @@
  stateless
|}]

(* Demonstration. *)

let f_once_app (x @ once) =
  let (f_app @ many) = f_once x in
  f_app ()
[%%expect{|
Line 2, characters 23-31:
2 |   let (f_app @ many) = f_once x in
                           ^^^^^^^^
Error: This value is "once" but is expected to be "many".
|}]

module F_once_app (M : S @ once) = struct
  module (F_app @ many) = F (M)
end
[%%expect{|
Line 2, characters 26-31:
2 |   module (F_app @ many) = F (M)
                              ^^^^^
Error: This is "once", but expected to be "many".
|}]

module F : functor (M : S @ once) () -> sig end @ many =
  functor (M : S @ once) () -> struct end
[%%expect{|
Line 2, characters 10-41:
2 |   functor (M : S @ once) () -> struct end
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         functor (M : S @ once) -> (functor () -> sig end) @ once
       is not included in
         functor (M : S @ once) () -> sig end
       Got "once"
       but expected "many".
|}]

let f_once_app2 (x @ once) =
  let f_app = f_once x in
  let (f_app' @ many) = f_app in
  f_app' ()
[%%expect{|
Line 3, characters 24-29:
3 |   let (f_app' @ many) = f_app in
                            ^^^^^
Error: This value is "once" but is expected to be "many".
|}]

module F_once_app2 (M : S @ once) = struct
  module F_app = F (M)
  module (F_app' @ many) = F_app
end
[%%expect{|
Line 3, characters 27-32:
3 |   module (F_app' @ many) = F_app
                               ^^^^^
Error: This is "once", but expected to be "many".
|}]

module F : functor (M : S @ once) -> (functor () -> sig end) @ many =
  functor (M : S @ once) () -> struct end
[%%expect{|
Line 2, characters 10-41:
2 |   functor (M : S @ once) () -> struct end
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         functor (M : S @ once) -> (functor () -> sig end) @ once
       is not included in
         functor (M : S @ once) () -> sig end
       Got "once"
       but expected "many".
|}]

let f_unique (x @ unique) () = ()
[%%expect{|
val f_unique : 'a @ unique -> unit -> unit = <fun>
|}]

module F (M : S @ unique) () = struct end
[%%expect{|
module F : functor (M : S @ unique) -> (functor () -> sig end) @ once @@
  stateless
|}]

let f_unique_app (x @ unique) =
  let (f_app @ many) = f_unique x in
  f_app ()
[%%expect{|
Line 2, characters 23-33:
2 |   let (f_app @ many) = f_unique x in
                           ^^^^^^^^^^
Error: This value is "once" but is expected to be "many".
|}]

module F_unique_app (M : S @ unique) = struct
  module (F_app @ many) = F (M)
end
[%%expect{|
Line 2, characters 26-31:
2 |   module (F_app @ many) = F (M)
                              ^^^^^
Error: This is "once", but expected to be "many".
|}]

module F : functor (M : S @ unique) () -> sig end @ many =
  functor (M : S @ unique) () -> struct end
[%%expect{|
Line 2, characters 10-43:
2 |   functor (M : S @ unique) () -> struct end
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         functor (M : S @ unique) -> (functor () -> sig end) @ once
       is not included in
         functor (M : S @ unique) () -> sig end
       Got "once"
       but expected "many".
|}]

let f_unique_app2 (x @ unique) =
  let f_app = f_unique x in
  let (f_app' @ many) = f_app in
  f_app' ()
[%%expect{|
Line 3, characters 24-29:
3 |   let (f_app' @ many) = f_app in
                            ^^^^^
Error: This value is "once" but is expected to be "many".
|}]

module F_unique_app2 (M : S @ unique) = struct
  module F_app = F (M)
  module (F_app' @ many) = F_app
end
[%%expect{|
Line 3, characters 27-32:
3 |   module (F_app' @ many) = F_app
                               ^^^^^
Error: This is "once", but expected to be "many".
|}]

module F : functor (_ : S @ unique) -> (functor () -> sig end) @ many =
  functor (_ : S @ unique) () -> struct end
[%%expect{|
Line 2, characters 10-43:
2 |   functor (_ : S @ unique) () -> struct end
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         S @ unique -> (functor () -> sig end) @ once
       is not included in
         S @ unique -> functor () -> sig end
       Got "once"
       but expected "many".
|}]

let f_unique_ret (_x @ unique) =
  ((fun () -> ()) : (unit -> unit) @ many)
[%%expect{|
val f_unique_ret : 'a @ unique -> (unit -> unit) = <fun>
|}]

(* Here, the inner [many] value is submoded into a [once] value. *)
module F (M : S @ unique) =
  ((functor () -> struct end) : (functor () -> sig end) @ many)
[%%expect{|
module F : functor (M : S @ unique) -> (functor () -> sig end) @ once @@
  stateless
|}]

let f1 (x1 @ once) x2 x3 =
  x1 (); x2 (); x3 ()
[%%expect{|
val f1 : (unit -> 'a) @ once -> (unit -> 'b) -> (unit -> 'c) -> 'c = <fun>
|}]

module F1 (M1 : S @ unique) (M2 : S) (M3 : S) = struct
  let () = M1.f ()
  let () = M2.f ()
  let () = M3.f ()
end
[%%expect{|
module F1 :
  functor (M1 : S @ unique) -> (functor (M2 : S) (M3 : S) -> sig end) @ once
  @@ stateless
|}]

let f1_flip x2 (x1 @ once) = f1 x1 x2
[%%expect{|
val f1_flip : (unit -> 'a) -> (unit -> 'b) @ once -> (unit -> 'c) -> 'c =
  <fun>
|}]

module F1_flip (M2 : S) (M1 : S @ unique) = F1 (M1) (M2)
[%%expect{|
module F1_flip :
  functor (M2 : S) (M1 : S @ unique) -> (functor (M3 : S) -> sig end) @ once
  @@ stateless
|}]

(* This example explains why we need the stricter partial application
   behavior for functors. [(functor (M2 : S) (M3 : S) -> sig end) @ once],
   returned by [F1], should be still [once] when partially applied for soundness. *)

let a (x1 @ once) x2 x3 =
  let f1_app = f1 x1 in
  let (f1_app_2 @ many) = f1_app x2 in
  f1_app_2 x3
[%%expect{|
Line 3, characters 26-35:
3 |   let (f1_app_2 @ many) = f1_app x2 in
                              ^^^^^^^^^
Error: This value is "once" but is expected to be "many".
|}]

module A (M1 : S @ unique) (M2 : S) (M3 : S) = struct
  module F1_applied = F1 (M1)
  module (F1_applied_2 @ many) = F1_applied (M2)
end
[%%expect{|
Line 3, characters 33-48:
3 |   module (F1_applied_2 @ many) = F1_applied (M2)
                                     ^^^^^^^^^^^^^^^
Error: This is "once", but expected to be "many".
|}]

let f2 x1 (x2 @ once) x3 =
  x1 (); x2 (); x3 ()
[%%expect{|
val f2 : (unit -> 'a) -> (unit -> 'b) @ once -> (unit -> 'c) -> 'c = <fun>
|}]

module F2 (M1 : S) (M2 : S @ unique) (M3 : S) = struct
  let () = M1.f ()
  let () = M2.f ()
  let () = M3.f ()
end
[%%expect{|
module F2 :
  functor (M1 : S) (M2 : S @ unique) -> (functor (M3 : S) -> sig end) @ once
  @@ stateless
|}]

let f3 x1 x2 (x3 @ once) =
  x1 (); x2 (); x3 ()
[%%expect{|
val f3 : (unit -> 'a) -> (unit -> 'b) -> (unit -> 'c) @ once -> 'c = <fun>
|}]

module F3 (M1 : S) (M2 : S) (M3 : S @ unique) = struct
  let () = M1.f ()
  let () = M2.f ()
  let () = M3.f ()
end
[%%expect{|
module F3 : functor (M1 : S) (M2 : S) (M3 : S @ unique) -> sig end @@
  stateless
|}]

let test1 (_x @ once) : (unit -> unit) @ many = fun () -> ()
[%%expect{|
val test1 : 'a @ once -> (unit -> unit) = <fun>
|}]

module F1 (M1 : S @ once) : (functor () -> sig end) @ many =
  functor () -> struct end
[%%expect{|
module F1 : functor (M1 : S @ once) -> (functor () -> sig end) @ once @@
  stateless
|}]

let test2 (x @ once) : (unit -> unit) @ many =
  fun () -> let _x = x in ()
[%%expect{|
Line 2, characters 21-22:
2 |   fun () -> let _x = x in ()
                         ^
Error: The value "x" is "once" but is expected to be "many"
       because it is used inside the function at Line 2, characters 2-28
       which is expected to be "many".
|}]

module F2 (M1 : S @ once) : (functor () -> sig end) @ many =
  functor () -> struct module M2 = M1 end
[%%expect{|
Line 2, characters 35-37:
2 |   functor () -> struct module M2 = M1 end
                                       ^^
Error: The module "M1" is "once" but is expected to be "many"
       because it is used inside the functor at Line 2, characters 10-41
       which is expected to be "many".
|}]

(* testing functor type inclusion *)
module type Portable_Portable = sig
    module F : functor (M : S @ portable) -> T @ portable
end
module type Nonportable_Portable = sig
    module F : functor (M : S @ nonportable) -> T @ portable
end
module type Portable_Nonportable = sig
    module F : functor (M : S @ portable) -> T @ nonportable
end
module type Nonportable_Nonportable = sig
    module F : functor (M : S @ nonportable) -> T @ nonportable
end
[%%expect{|
module type Portable_Portable =
  sig module F : functor (M : S @ portable) -> T @ portable end
module type Nonportable_Portable =
  sig module F : functor (M : S) -> T @ portable end
module type Portable_Nonportable =
  sig module F : functor (M : S @ portable) -> T end
module type Nonportable_Nonportable = sig module F : functor (M : S) -> T end
|}]

module F (M : Nonportable_Portable) = (M : Portable_Portable)
[%%expect{|
module F : functor (M : Nonportable_Portable) -> Portable_Portable @@
  stateless
|}]

module F (M : Portable_Portable) = (M : Nonportable_Portable)
[%%expect{|
Line 1, characters 36-37:
1 | module F (M : Portable_Portable) = (M : Nonportable_Portable)
                                        ^
Error: Signature mismatch:
       Modules do not match:
         sig
           module F :
             functor (M : S @ portable) ->
               sig val g : unit -> unit end @ portable
         end
       is not included in
         Nonportable_Portable
       In module "F":
       Modules do not match:
         functor (M : S @ portable) -> ...
       is not included in
         functor (M : S @ nonportable) -> ...
       Module types do not match:
         S @ portable
       does not include
         S @ nonportable
       Values do not match:
         val f : unit -> unit (* in a structure at nonportable *)
       is not included in
         val f : unit -> unit (* in a structure at portable *)
       The left-hand side is "nonportable"
       but the right-hand side is "portable".
|}]

module F (M : Portable_Nonportable) = (M : Portable_Portable)
[%%expect{|
Line 1, characters 39-40:
1 | module F (M : Portable_Nonportable) = (M : Portable_Portable)
                                           ^
Error: Signature mismatch:
       Modules do not match:
         sig
           module F :
             functor (M : S @ portable) -> sig val g : unit -> unit end
         end
       is not included in
         Portable_Portable
       In module "F":
       Modules do not match:
         functor (M : S @ portable) -> sig val g : unit -> unit end
       is not included in
         functor (M : S @ portable) -> T @ portable
       In module "F":
       Modules do not match:
         sig val g : unit -> unit end @ nonportable
       is not included in
         T @ portable
       In module "F":
       Values do not match:
         val g : unit -> unit (* in a structure at nonportable *)
       is not included in
         val g : unit -> unit (* in a structure at portable *)
       The left-hand side is "nonportable"
       but the right-hand side is "portable".
|}]

module F (M : Portable_Portable) = (M : Portable_Nonportable)
[%%expect{|
module F : functor (M : Portable_Portable) -> Portable_Nonportable @@
  stateless
|}]

module F (M : S @ shareable -> S) = (M : S -> S)
[%%expect{|
Line 1, characters 37-38:
1 | module F (M : S @ shareable -> S) = (M : S -> S)
                                         ^
Error: Signature mismatch:
       Modules do not match:
         functor (Arg : S @ shareable) -> ...
       is not included in
         functor S @ nonportable -> ...
       Module types do not match:
         S @ shareable
       does not include
         S @ nonportable
       Values do not match:
         val f : unit -> unit (* in a structure at nonportable *)
       is not included in
         val f : unit -> unit (* in a structure at shareable *)
       The left-hand side is "nonportable"
       but the right-hand side is "shareable".
|}]

module F (M : S -> S) = (M : S @ shareable -> S)
[%%expect{|
module F : functor (M : S -> S) -> S @ shareable -> S @@ stateless
|}]

module F (M : S -> S @ shareable) = (M : S -> S)
[%%expect{|
module F : functor (M : S -> S @ shareable) -> S -> S @@ stateless
|}]

module F (M : S -> S) = (M : S -> S @ shareable)
[%%expect{|
Line 1, characters 25-26:
1 | module F (M : S -> S) = (M : S -> S @ shareable)
                             ^
Error: Signature mismatch:
       Modules do not match:
         functor (Arg : S) -> sig val f : unit -> unit end
       is not included in
         S -> S @ shareable
       Modules do not match:
         sig val f : unit -> unit end @ nonportable
       is not included in
         S @ shareable
       Values do not match:
         val f : unit -> unit (* in a structure at nonportable *)
       is not included in
         val f : unit -> unit (* in a structure at shareable *)
       The left-hand side is "nonportable"
       but the right-hand side is "shareable".
|}]

module type T = sig val t : int * int end

module F (M : T @ unique -> S) = (M : T -> S)
[%%expect{|
module type T = sig val t : int * int end
Line 3, characters 34-35:
3 | module F (M : T @ unique -> S) = (M : T -> S)
                                      ^
Error: Signature mismatch:
       Modules do not match:
         functor (Arg : T @ unique) -> ...
       is not included in
         functor T @ aliased -> ...
       Module types do not match:
         T @ unique
       does not include
         T @ aliased
       Got "aliased" but expected "unique".
|}]

module F (M : T -> S) = (M : T @ unique -> S)
[%%expect{|
module F : functor (M : T -> S) -> T @ unique -> S @@ stateless
|}]

module F (M : S -> T @ unique) = (M : S -> T)
[%%expect{|
module F : functor (M : S -> T @ unique) -> S -> T @@ stateless
|}]

module F (M : S -> T) = (M : S -> T @ unique)
[%%expect{|
Line 1, characters 25-26:
1 | module F (M : S -> T) = (M : S -> T @ unique)
                             ^
Error: Signature mismatch:
       Modules do not match:
         functor (Arg : S) -> sig val t : int * int end
       is not included in
         S -> T @ unique
       Got "aliased"
       but expected "unique".
|}]

(* refering to [F(M).t] is allowed even if [M] is weaker than what [F] wants *)
module F (X : S @ portable) = struct
  type t = int
end
module M = struct
    let f = let r = ref 42 in fun () -> r := 24; ()
end
type t' = F(M).t
[%%expect{|
module F : functor (X : S @ portable) -> sig type t = int end @@ stateless
module M : sig val f : unit -> unit end
type t' = F(M).t
|}]

module F @ nonportable = F
module M @ nonportable = M

(* Similarly, [F(M).t] is not closing over [F] or [M] *)
let (foo @ portable) () =
  let _ : F(M).t = 42 in
  ()
[%%expect{|
module F = F @@ stateless nonportable
module M = M
val foo : unit -> unit = <fun>
|}]

(* CR-someday zqian: this should be allowed *)
(* CR-soon zqian: the error message should refer to [M.f] as the violation. *)
module F (M : S @ local) : S @ global = struct include M end
[%%expect{|
Line 1, characters 47-56:
1 | module F (M : S @ local) : S @ global = struct include M end
                                                   ^^^^^^^^^
Error: This is "local", but expected to be "global" because it is inside a structure.
|}]

(* some higher order functor *)
module F(G : S -> S @ portable) = struct
  module H : S -> S @ portable
    = functor (X : S) -> struct include G(X) end
end
[%%expect{|
module F :
  functor (G : S -> S @ portable) -> sig module H : S -> S @ portable end @@
  stateless
|}]

module F(G : S -> S) = struct
  module H : S -> S @ portable
    = functor (X : S) -> struct include G(X) end
end
[%%expect{|
Line 3, characters 14-48:
3 |     = functor (X : S) -> struct include G(X) end
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         functor (X : S) -> sig val f : unit -> unit end
       is not included in
         S -> S @ portable
       Modules do not match:
         sig val f : unit -> unit end @ nonportable
       is not included in
         S @ portable
       Values do not match:
         val f : unit -> unit (* in a structure at nonportable *)
       is not included in
         val f : unit -> unit (* in a structure at portable *)
       The left-hand side is "nonportable"
       but the right-hand side is "portable".
|}]

module F(G : S @ portable-> S) = struct
  module H : S -> S
    = functor (X : S) -> struct include G(X) end
end
[%%expect{|
Line 3, characters 40-44:
3 |     = functor (X : S) -> struct include G(X) end
                                            ^^^^
Error: Modules do not match: sig val f : unit -> unit end @ nonportable
     is not included in S @ portable
     Values do not match:
       val f : unit -> unit (* in a structure at nonportable *)
     is not included in
       val f : unit -> unit (* in a structure at portable *)
     The left-hand side is "nonportable"
     but the right-hand side is "portable".
|}]

module F(G : S -> S) = struct
  module H : S @ portable -> S
    = functor (X : S) -> struct include G(X) end
end
[%%expect{|
module F : functor (G : S -> S) -> sig module H : S @ portable -> S end @@
  stateless
|}]

module F (M : (S @ portable -> S) -> S) = (M : (S -> S) -> S)
[%%expect{|
module F : functor (M : (S @ portable -> S) -> S) -> (S -> S) -> S @@
  stateless
|}]

module F (M : (S -> S) -> S) = (M : (S @ portable -> S) -> S)
[%%expect{|
Line 1, characters 32-33:
1 | module F (M : (S -> S) -> S) = (M : (S @ portable -> S) -> S)
                                    ^
Error: Signature mismatch:
       Modules do not match:
         functor (Arg : $S1) -> ...
       is not included in
         functor $T1 -> ...
       Module types do not match:
         $S1 = S -> S
       does not include
         $T1 = S @ portable -> S
       Modules do not match:
         functor S @ portable -> ...
       is not included in
         functor S @ nonportable -> ...
       Module types do not match:
         S @ portable
       does not include
         S @ nonportable
       Values do not match:
         val f : unit -> unit (* in a structure at nonportable *)
       is not included in
         val f : unit -> unit (* in a structure at portable *)
       The left-hand side is "nonportable"
       but the right-hand side is "portable".
|}]

module F (M : (S -> S @ portable) -> S) = (M : (S -> S) -> S)
[%%expect{|
Line 1, characters 43-44:
1 | module F (M : (S -> S @ portable) -> S) = (M : (S -> S) -> S)
                                               ^
Error: Signature mismatch:
       Modules do not match:
         functor (Arg : $S1) -> ...
       is not included in
         functor $T1 -> ...
       Module types do not match:
         $S1 = S -> S @ portable
       does not include
         $T1 = S -> S
       Modules do not match: S @ nonportable is not included in S @ portable
       Values do not match:
         val f : unit -> unit (* in a structure at nonportable *)
       is not included in
         val f : unit -> unit (* in a structure at portable *)
       The left-hand side is "nonportable"
       but the right-hand side is "portable".
|}]

module F (M : (S -> S) -> S) = (M : (S -> S @ portable) -> S)
[%%expect{|
module F : functor (M : (S -> S) -> S) -> (S -> S @ portable) -> S @@
  stateless
|}]
