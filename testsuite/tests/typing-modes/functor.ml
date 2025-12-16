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

(* At top-level, functors, just like functions, will have their parameter mode and return
mode zapped to legacy. Therefore, we explicitly annotate the functor type. Alternatively,
put the functor at a deeper level, and its return mode can be infered. *)

module F : S @ portable -> T @ portable = functor (M : S @ portable) -> struct
    let g = M.f
    let () = use_portable M.f
    let () = use_portable g
end
[%%expect{|
module F : S @ portable -> T @ portable
|}]

module M = struct
    let f () = ()
end
[%%expect{|
module M : sig val f : unit -> unit end @@ stateless
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

(* CR-soon zqian: the inner functor should be forced to once as well*)
let () =
    let module (F @ once) () @ many = (functor () -> struct end) in
    ()
[%%expect{|
|}]

(* CR-soon zqian: the inner functor should be forced to once as well *)
module F (M : S @ once) () = struct end
[%%expect{|
module F : functor (M : S @ once) () -> sig end @@ stateless
|}]

(* CR-soon zqian: the inner functor should be forced to once as well *)
module F (M : S @ unique) () = struct end
[%%expect{|
module F : functor (M : S @ unique) () -> sig end @@ stateless
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
