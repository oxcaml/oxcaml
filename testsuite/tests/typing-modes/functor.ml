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
Lines 1-3, characters 30-3:
1 | ..............................struct
2 |     let g = M.f
3 | end
Error: The module is "nonportable"
         because it contains the value "g" defined as the expression at line 2, characters 8-9
         which is "nonportable".
       However, the module highlighted is expected to be "portable".
|}]

module F (M : S) : T @ portable = struct
    let g = M.f
end
[%%expect{|
Lines 1-3, characters 34-3:
1 | ..................................struct
2 |     let g = M.f
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig val g : unit -> unit end @ nonportable
       is not included in
         T @ portable
       Values do not match:
         val g : unit -> unit (* in a structure at nonportable *)
       is not included in
         val g : unit -> unit (* in a structure at portable *)
       The left-hand side is "nonportable"
       but the right-hand side is "portable".
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
Error: The module is "nonportable" but is expected to be "portable".
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
       because it contains a usage (of the value "r" at line 2, characters 40-41)
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
         because it contains a usage (of the value "r" at line 2, characters 40-41)
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

let f (k : (unit -> unit -> unit) @ local ) =
  let (k' @ global) = k () in
  k' ()
[%%expect{|
Line 2, characters 22-26:
2 |   let (k' @ global) = k () in
                          ^^^^
Error: This value is "local" but is expected to be "global".
  Hint: This is a partial application
        Adding 1 more argument will make the value non-local
|}]

(* However, for [f : (A -> (B -> C) @ mode1) @ mode2], we presume that [f]
   itself returns a new closure at [mode1]. *)

let f' (k : (unit -> (unit -> unit)) @ local) =
  let (k' @ global) = k () in
  k' ()
[%%expect{|
val f' : (unit -> (unit -> unit)) @ local -> unit = <fun>
|}]

(* CR modes: Functors don't have yet syntactic arity,
   and for them we must pick one single behavior.

   We pick the former, more restrictive behavior, for reasons given below.

   Internal ticket 1534. *)

module F (K : (functor () () -> sig end) @ local) = struct
  module (K' @ global) = K ()
end
[%%expect{|
Line 2, characters 25-29:
2 |   module (K' @ global) = K ()
                             ^^^^
Error: The module is "local" but is expected to be "global".
|}]

(* For functions we can infer the more permissive choice. *)

let () =
  let (f @ stateful) () @ stateless = fun () -> () in
  let (_f @ stateless) = f () in
  ()
[%%expect{|
|}]

let () =
  let (f @ stateful) : (unit -> unit -> unit) = fun () () -> () in
  let (_f @ stateless) = f () in
  ()
[%%expect{|
Line 3, characters 25-29:
3 |   let (_f @ stateless) = f () in
                             ^^^^
Error: This value is "stateful" but is expected to be "stateless".
|}]

(* Functor behavior here is stricter. *)

let () =
  (* CR modes: this line should raise the error eagerly.

     Internal ticket 6260. *)
  let module (F @ stateful) () @ stateless = (functor () -> struct end) in
  let module (M' @ stateless) = F () in
  ()
[%%expect{|
Line 6, characters 32-36:
6 |   let module (M' @ stateless) = F () in
                                    ^^^^
Error: The module is "stateful" but is expected to be "stateless".
|}]

(* Closing over arguments: given [f : A -> B -> C] and [a : A @ mode], [f a] must close
   over [a]. The multi-argument function syntax handles this implicitly.  *)

let f_local (x @ local) () = ()
[%%expect{|
val f_local : 'a @ local -> unit -> unit = <fun>
|}]

(* CR modes: For functor, the behavior is the same but explicit
   since currying is not yet implemented.

   Internal ticket 6259. *)

module F (M : S @ local) () = struct end
[%%expect{|
module F : functor (M : S @ local) -> (functor () -> sig end) @ local @@
  stateless
|}]

(* Demonstration. *)

let f_stateful (x @ stateful) () = ()
module F (M : S @ stateful) () = struct end
[%%expect{|
val f_stateful : 'a -> unit -> unit = <fun>
module F : functor (M : S) () -> sig end @@ stateless
|}]

let f_stateful_app (x @ stateful) =
  let (f_app @ stateless) = f_stateful x in
  f_app ()
[%%expect{|
Line 2, characters 28-40:
2 |   let (f_app @ stateless) = f_stateful x in
                                ^^^^^^^^^^^^
Error: This value is "stateful" but is expected to be "stateless".
|}]

module F_stateful_app (M : S @ stateful) = struct
  module (F_app @ stateless) = F (M)
end
[%%expect{|
Line 2, characters 31-36:
2 |   module (F_app @ stateless) = F (M)
                                   ^^^^^
Error: The module is "stateful" but is expected to be "stateless".
|}]

module F : functor (M : S @ stateful) -> (functor () -> sig end) @ stateless =
  functor (M : S @ stateful) () -> struct end
[%%expect{|
Line 2, characters 10-45:
2 |   functor (M : S @ stateful) () -> struct end
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         functor (M : S) () -> sig end
       is not included in
         functor (M : S) -> (functor () -> sig end) @ stateless
       Got "stateful"
       but expected "stateless".
|}]

let f_stateful_app2 (x @ stateful) =
  let f_app = f_stateful x in
  let (f_app' @ stateless) = f_app in
  f_app' ()
[%%expect{|
Line 3, characters 29-34:
3 |   let (f_app' @ stateless) = f_app in
                                 ^^^^^
Error: This value is "stateful" but is expected to be "stateless".
|}]

module F_stateful_app2 (M : S @ stateful) = struct
  module F_app = F (M)
  module (F_app' @ stateless) = F_app
end
[%%expect{|
Line 3, characters 32-37:
3 |   module (F_app' @ stateless) = F_app
                                    ^^^^^
Error: The module is "stateful" but is expected to be "stateless".
|}]

module F : functor (M : S @ stateful) -> (functor () -> sig end) @ stateless =
  functor (M : S @ stateful) () -> struct end
[%%expect{|
Line 2, characters 10-45:
2 |   functor (M : S @ stateful) () -> struct end
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         functor (M : S) () -> sig end
       is not included in
         functor (M : S) -> (functor () -> sig end) @ stateless
       Got "stateful"
       but expected "stateless".
|}]

let f_read_write (x @ read_write) () = ()
[%%expect{|
val f_read_write : 'a -> unit -> unit = <fun>
|}]

module F (M : S @ read_write) () = struct end
[%%expect{|
module F : functor (M : S) () -> sig end @@ stateless
|}]

let f_read_write_app (x @ read_write) =
  let (f_app @ stateless) = f_read_write x in
  f_app ()
[%%expect{|
Line 2, characters 28-42:
2 |   let (f_app @ stateless) = f_read_write x in
                                ^^^^^^^^^^^^^^
Error: This value is "stateful" but is expected to be "stateless".
|}]

module F_read_write_app (M : S @ read_write) = struct
  module (F_app @ stateless) = F (M)
end
[%%expect{|
Line 2, characters 31-36:
2 |   module (F_app @ stateless) = F (M)
                                   ^^^^^
Error: The module is "stateful" but is expected to be "stateless".
|}]

module F : functor (M : S @ read_write) -> (functor () -> sig end) @ stateless =
  functor (M : S @ read_write) () -> struct end
[%%expect{|
Line 2, characters 10-47:
2 |   functor (M : S @ read_write) () -> struct end
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         functor (M : S) () -> sig end
       is not included in
         functor (M : S) -> (functor () -> sig end) @ stateless
       Got "stateful"
       but expected "stateless".
|}]

let f_read_write_app2 (x @ read_write) =
  let f_app = f_read_write x in
  let (f_app' @ stateless) = f_app in
  f_app' ()
[%%expect{|
Line 3, characters 29-34:
3 |   let (f_app' @ stateless) = f_app in
                                 ^^^^^
Error: This value is "stateful" but is expected to be "stateless".
|}]

module F_read_write_app2 (M : S @ read_write) = struct
  module F_app = F (M)
  module (F_app' @ stateless) = F_app
end
[%%expect{|
Line 3, characters 32-37:
3 |   module (F_app' @ stateless) = F_app
                                    ^^^^^
Error: The module is "stateful" but is expected to be "stateless".
|}]

module F : functor (_ : S @ read_write) -> (functor () -> sig end) @ stateless =
  functor (_ : S @ read_write) () -> struct end
[%%expect{|
Line 2, characters 10-47:
2 |   functor (_ : S @ read_write) () -> struct end
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         S -> functor () -> sig end
       is not included in
         S -> (functor () -> sig end) @ stateless
       Got "stateful"
       but expected "stateless".
|}]

let f_read_write_ret (_x @ read_write) =
  ((fun () -> ()) : (unit -> unit) @ stateless)
[%%expect{|
val f_read_write_ret : 'a -> unit -> unit = <fun>
|}]

(* Here, the inner [stateless] value is submoded into a [stateful] value. *)
module F (M : S @ read_write) =
  ((functor () -> struct end) : (functor () -> sig end) @ stateless)
[%%expect{|
module F : functor (M : S) () -> sig end @@ stateless
|}]

let f1 (x1 @ stateful) (x2 @ stateless) (x3 @ stateless) =
  x1 (); x2 (); x3 ()
[%%expect{|
val f1 :
  (unit -> 'a) -> (unit -> 'b) @ stateless -> (unit -> 'c) @ stateless -> 'c =
  <fun>
|}]

module F1 (M1 : S @ read_write) (M2 : S @ stateless) (M3 : S @ stateless) = struct
  let () = M1.f ()
  let () = M2.f ()
  let () = M3.f ()
end
[%%expect{|
module F1 :
  functor (M1 : S) (M2 : S @ stateless) (M3 : S @ stateless) -> sig end @@
  stateless
|}]

let f1_flip (x2 @ stateless) (x1 @ stateful) = f1 x1 x2
[%%expect{|
val f1_flip :
  (unit -> 'a) @ stateless -> (unit -> 'b) -> (unit -> 'c) @ stateless -> 'c =
  <fun>
|}]

module F1_flip (M2 : S @ stateless) (M1 : S @ read_write) = F1 (M1) (M2)
[%%expect{|
module F1_flip :
  functor (M2 : S @ stateless) (M1 : S) (M3 : S @ stateless) -> sig end @@
  stateless
|}]

(* This example explains why we need the stricter partial application
   behavior for functors. [(functor (M2 : S) (M3 : S) -> sig end) @ stateful],
   returned by [F1], should be still [stateful] when partially applied for soundness. *)

let a (x1 @ stateful) (x2 @ stateless) (x3 @ stateless) =
  let f1_app = f1 x1 in
  let (f1_app_2 @ stateless) = f1_app x2 in
  f1_app_2 x3
[%%expect{|
Line 3, characters 31-40:
3 |   let (f1_app_2 @ stateless) = f1_app x2 in
                                   ^^^^^^^^^
Error: This value is "stateful" but is expected to be "stateless".
|}]

module A (M1 : S @ read_write) (M2 : S @ stateless) (M3 : S @ stateless) = struct
  module F1_applied = F1 (M1)
  module (F1_applied_2 @ stateless) = F1_applied (M2)
end
[%%expect{|
Line 3, characters 38-53:
3 |   module (F1_applied_2 @ stateless) = F1_applied (M2)
                                          ^^^^^^^^^^^^^^^
Error: The module is "stateful" but is expected to be "stateless".
|}]

let f2 (x1 @ stateless) (x2 @ stateful) (x3 @ stateless) =
  x1 (); x2 (); x3 ()
[%%expect{|
val f2 :
  (unit -> 'a) @ stateless -> (unit -> 'b) -> (unit -> 'c) @ stateless -> 'c =
  <fun>
|}]

module F2 (M1 : S @ stateless) (M2 : S @ read_write) (M3 : S @ stateless) = struct
  let () = M1.f ()
  let () = M2.f ()
  let () = M3.f ()
end
[%%expect{|
module F2 :
  functor (M1 : S @ stateless) (M2 : S) (M3 : S @ stateless) -> sig end @@
  stateless
|}]

let f3 (x1 @ stateless) (x2 @ stateless) (x3 @ stateful) =
  x1 (); x2 (); x3 ()
[%%expect{|
val f3 :
  (unit -> 'a) @ stateless -> (unit -> 'b) @ stateless -> (unit -> 'c) -> 'c =
  <fun>
|}]

module F3 (M1 : S @ stateless) (M2 : S @ stateless) (M3 : S @ read_write) = struct
  let () = M1.f ()
  let () = M2.f ()
  let () = M3.f ()
end
[%%expect{|
module F3 :
  functor (M1 : S @ stateless) (M2 : S @ stateless) (M3 : S) -> sig end @@
  stateless
|}]

let test1 (_x @ stateful) : (unit -> unit) @ stateless = fun () -> ()
[%%expect{|
val test1 : 'a -> (unit -> unit) @ stateless = <fun>
|}]

module F1 (M1 : S @ stateful) : (functor () -> sig end) @ stateless =
  functor () -> struct end
[%%expect{|
module F1 : functor (M1 : S) () -> sig end @@ stateless
|}]

let test2 (x @ stateful) : (unit -> unit) @ stateless =
  fun () -> let _x = x in ()
[%%expect{|
Line 2, characters 21-22:
2 |   fun () -> let _x = x in ()
                         ^
Error: The value "x" is "stateful"
       but is expected to be "stateless"
         because it is used inside the function at line 2, characters 2-28
         which is expected to be "stateless".
|}]

module F2 (M1 : S @ stateful) : (functor () -> sig end) @ stateless =
  functor () -> struct module M2 = M1 end
[%%expect{|
Line 2, characters 10-41:
2 |   functor () -> struct module M2 = M1 end
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Got "stateful"
         because it closes over the module "M1" at line 2, characters 35-37
         which is "stateful".
       However, expected "stateless".
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

module type T = sig val t : int * int ref end

module F (M : T -> S) = (M : T @ immutable -> S)
[%%expect{|
module type T = sig val t : int * int ref end
Line 3, characters 25-26:
3 | module F (M : T -> S) = (M : T @ immutable -> S)
                             ^
Error: Signature mismatch:
       Modules do not match:
         functor (Arg : T @ read_write) -> ...
       is not included in
         functor T @ immutable -> ...
       Module types do not match:
         T @ read_write
       does not include
         T @ immutable
       Values do not match:
         val t : int * int ref (* in a structure at immutable *)
       is not included in
         val t : int * int ref (* in a structure at read_write *)
       The left-hand side is "immutable"
       but the right-hand side is "read_write".
|}]

module F (M : T @ immutable -> S) = (M : T -> S)
[%%expect{|
module F : functor (M : T @ immutable -> S) -> T -> S @@ stateless
|}]

module F (M : S -> T) = (M : S -> T @ immutable)
[%%expect{|
module F : functor (M : S -> T) -> S -> T @ immutable @@ stateless
|}]

module F (M : S -> T @ immutable) = (M : S -> T)
[%%expect{|
Line 1, characters 37-38:
1 | module F (M : S -> T @ immutable) = (M : S -> T)
                                         ^
Error: Signature mismatch:
       Modules do not match:
         functor (Arg : S) -> sig val t : int * int ref end @ immutable
       is not included in
         S -> T
       Modules do not match:
         sig val t : int * int ref end @ immutable
       is not included in
         T @ read_write
       Values do not match:
         val t : int * int ref (* in a structure at immutable *)
       is not included in
         val t : int * int ref (* in a structure at read_write *)
       The left-hand side is "immutable"
       but the right-hand side is "read_write".
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
module F (M : S @ local) : S @ global = struct include M end
[%%expect{|
Line 1, characters 55-56:
1 | module F (M : S @ local) : S @ global = struct include M end
                                                           ^
Error: The value "f" in the structure is "local"
         because it is the value "f" in the structure at line 1, characters 55-56
         which is "local".
       However, the value "f" in the structure highlighted is expected to be "global"
         because it is the value "f" in the structure at line 1, characters 47-56
         which is expected to be "global"
         because modules always need to be allocated on the heap.
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
         because it is the value "f" in the structure at line 3, characters 40-44
         which is "nonportable".
       However, the right-hand side is "portable".
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

(* Testing modalities on functors in signatures.

   From the syntax documentation, there are two ways to add modalities to module
   declarations in signatures:
   1. `module (M @@ portable) ...` - modality on the name
   2. `module M : ... @@ portable` - modality at the end of the type

   Unlike signatures where modalities are applied deeply to values and then
   reset, functor modalities are kept on [md_modalities] and applied when the
   functor is accessed. *)

(* Test 1: `module (F @@ portable)` syntax - modality on name *)
module type SigWithModalFunctor1 = sig
  module (F @@ portable) : S @ portable -> S @ portable
end

module TestModalFunctor1 : SigWithModalFunctor1 = struct
  module F (M : S @ portable) = M
end
[%%expect{|
module type SigWithModalFunctor1 =
  sig module F : S @ portable -> S @ portable @@ portable end
module TestModalFunctor1 : SigWithModalFunctor1 @@ stateless
|}]

(* Test 2: `module F : ... @@ portable` syntax - modality at end *)
module type SigWithModalFunctor2 = sig
  module F : S @ portable -> S @ portable @@ portable
end

module TestModalFunctor2 : SigWithModalFunctor2 = struct
  module F (M : S @ portable) = M
end
[%%expect{|
module type SigWithModalFunctor2 =
  sig module F : S @ portable -> S @ portable @@ portable end
module TestModalFunctor2 : SigWithModalFunctor2 @@ stateless
|}]

(* Test 3: When a functor has @@ portable, it can be accessed as portable
   even from a nonportable module. Without the modality, this fails.
   We use first-class modules to avoid REPL zapping. *)
let use_functor_with_modality1
    ((module M) : (module SigWithModalFunctor1) @ nonportable) =
  let module _ @ portable = M.F in ()
[%%expect{|
val use_functor_with_modality1 : (module SigWithModalFunctor1) -> unit =
  <fun>
|}]

let use_functor_with_modality2
    ((module M) : (module SigWithModalFunctor2) @ nonportable) =
  let module _ @ portable = M.F in ()
[%%expect{|
val use_functor_with_modality2 : (module SigWithModalFunctor2) -> unit =
  <fun>
|}]

module type SigWithFunctorNoModality = sig
  module F : S @ portable -> S @ portable
end

let use_functor_without_modality
    ((module M) : (module SigWithFunctorNoModality) @ nonportable) =
  let module _ @ portable = M.F in ()
[%%expect{|
module type SigWithFunctorNoModality =
  sig module F : S @ portable -> S @ portable end
Line 7, characters 28-31:
7 |   let module _ @ portable = M.F in ()
                                ^^^
Error: This is "nonportable", but expected to be "portable".
|}]

(* Test 4: Compare type inclusion with and without modalities on functors.
   A signature with @@ portable is stronger than one without. *)
module type SigWithModalFunctor3 = sig
  module F : S @ portable -> S @ portable @@ portable
end

module type SigWithFunctorNoModal = sig
  module F : S @ portable -> S @ portable
end

module F (M : SigWithModalFunctor3) = (M : SigWithFunctorNoModal)
[%%expect{|
module type SigWithModalFunctor3 =
  sig module F : S @ portable -> S @ portable @@ portable end
module type SigWithFunctorNoModal =
  sig module F : S @ portable -> S @ portable end
module F : functor (M : SigWithModalFunctor3) -> SigWithFunctorNoModal @@
  stateless
|}]

module F (M : SigWithFunctorNoModal) = (M : SigWithModalFunctor3)
[%%expect{|
Line 1, characters 40-41:
1 | module F (M : SigWithFunctorNoModal) = (M : SigWithModalFunctor3)
                                            ^
Error: Signature mismatch:
       Modules do not match:
         sig
           module F :
             functor (Arg : S @ portable) ->
               sig val f : unit -> unit end @ portable
         end @ nonportable
       is not included in
         SigWithModalFunctor3 @ nonportable
       In module "F":
       Got "nonportable"
       but expected "portable".
|}]

(* Test 5: Default modalities like `sig @@ portable` apply to the functor. *)
module type SigWithDefaultModality = sig @@ portable
  module F : S -> S
end
[%%expect{|
module type SigWithDefaultModality = sig module F : S -> S @@ portable end
|}]

(* The functor F is portable. *)
let test_default_modality
    ((module M) : (module SigWithDefaultModality) @ nonportable) =
  (* F itself can be accessed as portable *)
  let module _ @ portable = M.F in
  ()
[%%expect{|
val test_default_modality : (module SigWithDefaultModality) -> unit = <fun>
|}]
