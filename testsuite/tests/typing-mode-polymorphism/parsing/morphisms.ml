(* TEST
 flags = "-extension unique -extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

(*
 * This file tests parsing of morphisms in bounds: [past('m)], [close('m)],
 * and postfix [mod c]
*)

(* the implicit curry mode of [fst] can now be written explicitly, and the
   identity implementation is accepted *)

module type Explicit_close = sig
  val fst : 'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | local once]
end
[%%expect{|
module type Explicit_close =
  sig
    val fst :
      'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | local once]
  end
|}]

module M_explicit_close : Explicit_close = struct
  let fst x _ = x
end
[%%expect{|
module M_explicit_close : Explicit_close
|}]

(* [past('m)] relates only the comonadic fragments *)

module type Past_upper = sig
  val f : 'a @ [< past('m)] -> 'b @ 'n -> unit @ 'k
end
[%%expect{|
module type Past_upper = sig val f : 'a @ 'o -> 'b @ 'n -> unit @ 'm end
|}]

module M_past_upper : Past_upper = struct
  let f _ _ = ()
end
[%%expect{|
module M_past_upper : Past_upper
|}]

module type Past_lower = sig
  val f : 'a @ 'n -> 'a @ [> past('n)]
end
[%%expect{|
module type Past_lower =
  sig val f : 'a @ [< past('m)] -> 'a @ [> past('m)] end
|}]

module M_bad_past_lower : Past_lower = struct
  let f x = x
end
[%%expect{|
Lines 1-3, characters 39-3:
1 | .......................................struct
2 |   let f x = x
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a @ [< 'm] -> 'a @ [> 'm] end
       is not included in
         Past_lower
       Values do not match:
         val f : 'a @ [< 'm] -> 'a @ [> 'm]
       is not included in
         val f : 'a @ [< past('m)] -> 'a @ [> past('m)]
       The type "'a @ [< 'm > past('n)] -> 'a @ [> 'm]"
       is not compatible with the type
         "'a @ [< past('o) & past('n)] -> 'a @ [> past('o)]"
|}]

(* [mod c] drops the axes [c] mentions from the inequality *)

module type Mod_upper_comonadic = sig
  val f : 'a @ [< 'm mod portable] -> 'a @ [> 'm]
end
[%%expect{|
module type Mod_upper_comonadic =
  sig val f : 'a @ [< 'm] -> 'a @ [> 'm mod portable] end
|}]

module M_bad_mod_upper_comonadic : Mod_upper_comonadic = struct
  let f x = x
end
[%%expect{|
Lines 1-3, characters 57-3:
1 | .........................................................struct
2 |   let f x = x
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a @ [< 'm] -> 'a @ [> 'm] end
       is not included in
         Mod_upper_comonadic
       Values do not match:
         val f : 'a @ [< 'm] -> 'a @ [> 'm]
       is not included in
         val f : 'a @ [< 'm] -> 'a @ [> 'm mod portable]
       The type "'a @ [< 'm > past('n)] -> 'a @ [> 'm]"
       is not compatible with the type
         "'a @ [< 'o & past('n)] -> 'a @ [> 'o mod portable]"
|}]

module type Mod_lower_monadic = sig
  val f : 'a @ [< 'm] -> 'a @ [> 'm mod aliased]
end
[%%expect{|
module type Mod_lower_monadic =
  sig val f : 'a @ [< 'm mod aliased] -> 'a @ [> 'm] end
|}]

module type Mod_upper_monadic = sig
  val f : 'a @ [< 'm mod contended] -> 'a @ [> 'm]
end
[%%expect{|
module type Mod_upper_monadic =
  sig val f : 'a @ [< 'm mod contended] -> 'a @ [> 'm] end
|}]

module type Mod_lower_comonadic = sig
  val f : 'a @ [< 'm] -> 'a @ [> 'm mod global]
end
[%%expect{|
module type Mod_lower_comonadic =
  sig val f : 'a @ [< 'm] -> 'a @ [> 'm mod global] end
|}]

module type Mod_mixed = sig
  val f : 'a @ [< 'm mod portable contended] -> 'a @ [> 'm mod many aliased]
end
[%%expect{|
module type Mod_mixed =
  sig
    val f :
      'a @ [< 'm mod aliased contended] -> 'a @ [> 'm mod many portable]
  end
|}]

module M_bad_mod_mixed : Mod_mixed = struct
  let f x = x
end
[%%expect{|
Lines 1-3, characters 37-3:
1 | .....................................struct
2 |   let f x = x
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a @ [< 'm] -> 'a @ [> 'm] end
       is not included in
         Mod_mixed
       Values do not match:
         val f : 'a @ [< 'm] -> 'a @ [> 'm]
       is not included in
         val f :
           'a @ [< 'm mod aliased contended] -> 'a @ [> 'm mod many portable]
       The type "'a @ [< 'm > past('n)] -> 'a @ [> 'm]"
       is not compatible with the type
         "'a @ [< 'o mod aliased contended & past('n)] ->
         'a @ [> 'o mod many portable]"
|}]

(* [mod] on an upper bound strengthens the signature: the function must
   accept arguments whose contention is unrelated to ['m] while still
   returning at [> 'm], so the identity no longer suffices; the [mod]
   signature subsumes the plain one, and not conversely *)

module M_bad_mod_upper_monadic : Mod_upper_monadic = struct
  let f x = x
end
[%%expect{|
Lines 1-3, characters 53-3:
1 | .....................................................struct
2 |   let f x = x
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a @ [< 'm] -> 'a @ [> 'm] end
       is not included in
         Mod_upper_monadic
       Values do not match:
         val f : 'a @ [< 'm] -> 'a @ [> 'm]
       is not included in
         val f : 'a @ [< 'm mod contended] -> 'a @ [> 'm]
       The type "'a @ [< 'm > past('n)] -> 'a @ [> 'm]"
       is not compatible with the type
         "'a @ [< 'o mod contended & past('n)] -> 'a @ [> 'o]"
|}]

module type Plain = sig
  val f : 'a @ [< 'm] -> 'a @ [> 'm]
end
[%%expect{|
module type Plain = sig val f : 'a @ [< 'm] -> 'a @ [> 'm] end
|}]

module F_mod_subsumes_plain (X : Mod_upper_monadic) : Plain = X
[%%expect{|
module F_mod_subsumes_plain : functor (X : Mod_upper_monadic) -> Plain
|}]

module F_bad_plain_insufficient (X : Plain) : Mod_upper_monadic = X
[%%expect{|
Line 1, characters 66-67:
1 | module F_bad_plain_insufficient (X : Plain) : Mod_upper_monadic = X
                                                                      ^
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a @ [< 'm] -> 'a @ [> 'm] end
       is not included in
         Mod_upper_monadic
       Values do not match:
         val f : 'a @ [< 'm] -> 'a @ [> 'm]
       is not included in
         val f : 'a @ [< 'm mod contended] -> 'a @ [> 'm]
       The type "'a @ [< 'm > past('n)] -> 'a @ [> 'm]"
       is not compatible with the type
         "'a @ [< 'o mod contended & past('n)] -> 'a @ [> 'o]"
|}]

(* [mod c] applied to [close('m)] *)

module type Close_mod = sig
  val fst : 'a @ [< 'm]
            -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) mod portable | local once]
end
[%%expect{|
module type Close_mod =
  sig
    val fst :
      'a @ [< 'm] ->
      ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) mod portable | local once]
  end
|}]

module M_bad_close_mod : Close_mod = struct
  let fst x _ = x
end
[%%expect{|
Lines 1-3, characters 37-3:
1 | .....................................struct
2 |   let fst x _ = x
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val fst :
             'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | local]
         end
       is not included in
         Close_mod
       Values do not match:
         val fst :
           'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | local]
       is not included in
         val fst :
           'a @ [< 'm] ->
           ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) mod portable | local once]
       The type
         "'a @ [< 'm > past('o)] ->
         ('b @ [> past('n)] -> 'a @ [> 'm]) @ [> close('m) | local]"
       is not compatible with the type
         "'a @ [< 'p & past('o)] ->
         ('b @ [< past('n)] -> 'a @ [> 'p]) @ [> close('p) mod portable | local once]"
|}]

(* [mod many] on [close('m)] weakens the curry's floor below what a
   capturing implementation delivers *)

module type Close_mod_many = sig
  val fst : 'a @ [< 'm]
            -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) mod many | local]
end
[%%expect{|
module type Close_mod_many =
  sig
    val fst :
      'a @ [< 'm] ->
      ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) mod many | local]
  end
|}]

module M_bad_close_mod_many : Close_mod_many = struct
  let fst x _ = x
end
[%%expect{|
Lines 1-3, characters 47-3:
1 | ...............................................struct
2 |   let fst x _ = x
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val fst :
             'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | local]
         end
       is not included in
         Close_mod_many
       Values do not match:
         val fst :
           'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | local]
       is not included in
         val fst :
           'a @ [< 'm] ->
           ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) mod many | local]
       The type
         "'a @ [< 'm > past('o)] ->
         ('b @ [> past('n)] -> 'a @ [> 'm]) @ [> close('m) | local]"
       is not compatible with the type
         "'a @ [< 'p & past('o)] ->
         ('b @ [< past('n)] -> 'a @ [> 'p]) @ [> close('p) mod many | local]"
|}]

(* [mod] required by implementations *)

(* a mutable field requires [mod aliased dynamic] on the argument's upper
   bound *)

type 'a myref = { mutable i : 'a }
[%%expect{|
type 'a myref = { mutable i : 'a; }
|}]

module M_ref : sig
  val alloc :
    'a @ [< 'm mod aliased dynamic & global many] ->
    'a myref @ [> 'm | stateful]
end = struct
  let alloc x = { i = x }
end
[%%expect{|
Lines 5-7, characters 6-3:
5 | ......struct
6 |   let alloc x = { i = x }
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val alloc :
             'a @ [< 'm mod aliased dynamic & global many] ->
             'a myref @ [> 'm | stateful]
         end
       is not included in
         sig
           val alloc :
             'a @ [< 'm mod aliased dynamic & global many] ->
             'a myref @ [> 'm | stateful]
         end
       Values do not match:
         val alloc :
           'a @ [< 'm mod aliased dynamic & global many] ->
           'a myref @ [> 'm | stateful]
       is not included in
         val alloc :
           'a @ [< 'm mod aliased dynamic & global many] ->
           'a myref @ [> 'm | stateful]
       The type
         "'a @ [< 'm mod aliased dynamic & global many] ->
         'a myref @ [> 'm | stateful]"
       is not compatible with the type
         "'a @ [< 'n mod aliased dynamic & global many] ->
         'a myref @ [> 'n | stateful]"
|}]

module M_ref_no_mod : sig
  val alloc : 'a @ [< 'm & global many] -> 'a myref @ [> 'm | stateful]
end = struct
  let alloc x = { i = x }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let alloc x = { i = x }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val alloc :
             'a @ [< 'm mod aliased dynamic & global many] ->
             'a myref @ [> 'm | stateful]
         end
       is not included in
         sig
           val alloc :
             'a @ [< 'm & global many] -> 'a myref @ [> 'm | stateful]
         end
       Values do not match:
         val alloc :
           'a @ [< 'm mod aliased dynamic & global many] ->
           'a myref @ [> 'm | stateful]
       is not included in
         val alloc :
           'a @ [< 'm & global many] -> 'a myref @ [> 'm | stateful]
       The type
         "'a @ [< 'm mod aliased dynamic & global many] ->
         'a myref @ [> 'm | stateful]"
       is not compatible with the type
         "'a @ [< 'n & global many] -> 'a myref @ [> 'n | stateful]"
|}]

let use_unique (_ @ unique) = ()

let ok (x @ aliased) = use_unique (M_ref.alloc x)
[%%expect{|
val use_unique : 'a @ [< unique] -> unit @ 'm = <fun>
Line 3, characters 35-40:
3 | let ok (x @ aliased) = use_unique (M_ref.alloc x)
                                       ^^^^^
Error: Unbound module "M_ref"
|}]

let bad (x @ aliased) = use_unique (M_ref_no_mod.alloc x)
[%%expect{|
Line 1, characters 36-48:
1 | let bad (x @ aliased) = use_unique (M_ref_no_mod.alloc x)
                                        ^^^^^^^^^^^^
Error: Unbound module "M_ref_no_mod"
|}]

(* an [@@ contended] modality requires [mod contended] on the argument's
   upper bound *)

module T : sig
  type t

  val v : t
end = struct
  type t = unit

  let v = ()
end
[%%expect{|
module T : sig type t val v : t end
|}]

type 'a cbox = { c : 'a @@ contended; other : T.t }
[%%expect{|
type 'a cbox = { c : 'a @@ contended; other : T.t; }
|}]

module M_cbox : sig
  val cbox : 'a @ [< 'm mod contended & global] -> 'a cbox @ [> 'm | aliased]
end = struct
  let cbox x = { c = x; other = T.v }
end
[%%expect{|
module M_cbox :
  sig
    val cbox :
      'a @ [< 'm mod contended & global] -> 'a cbox @ [> 'm | aliased]
  end
|}]

module M_cbox_no_mod : sig
  val cbox : 'a @ [< 'm & global] -> 'a cbox @ [> 'm | aliased]
end = struct
  let cbox x = { c = x; other = T.v }
end
[%%expect{|
module M_cbox_no_mod :
  sig val cbox : 'a @ [< 'm & global] -> 'a cbox @ [> 'm | aliased] end
|}]

let use_uncontended (_ @ uncontended) = ()

let ok2 (x @ contended) = use_uncontended (M_cbox.cbox x)
[%%expect{|
val use_uncontended : 'a @ [< uncontended] -> unit @ 'm = <fun>
val ok2 : 'a @ [< global > contended] -> unit @ [> dynamic] = <fun>
|}]

let bad2 (x @ contended) = use_uncontended (M_cbox_no_mod.cbox x)
[%%expect{|
Line 1, characters 43-65:
1 | let bad2 (x @ contended) = use_uncontended (M_cbox_no_mod.cbox x)
                                               ^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* an [@@ portable] modality requires [mod portable] on the result's lower
   bound *)

type 'a pbox = { p : 'a @@ portable; app : 'a -> 'a }
[%%expect{|
type 'a pbox = { p : 'a @@ portable; app : 'a -> 'a; }
|}]

module M_pget : sig
  val pget : 'a pbox @ [< 'm] -> 'a @ [> 'm mod portable]
end = struct
  let pget b = b.p
end
[%%expect{|
module M_pget :
  sig val pget : 'a pbox @ [< 'm] -> 'a @ [> 'm mod portable] end
|}]

module M_pget_no_mod : sig
  val pget : 'a pbox @ [< 'm] -> 'a @ [> 'm]
end = struct
  let pget b = b.p
end
[%%expect{|
module M_pget_no_mod : sig val pget : 'a pbox @ [< 'm] -> 'a @ [> 'm] end
|}]

let use_portable (_ @ portable) = ()

let ok3 (b @ nonportable) = use_portable (M_pget.pget b)
[%%expect{|
val use_portable : 'a @ [< portable] -> unit @ 'm = <fun>
val ok3 : 'a pbox @ [< global > nonportable] -> unit @ [> dynamic] = <fun>
|}]

let bad3 (b @ nonportable) = use_portable (M_pget_no_mod.pget b)
[%%expect{|
Line 1, characters 42-64:
1 | let bad3 (b @ nonportable) = use_portable (M_pget_no_mod.pget b)
                                              ^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* Invalid signatures *)

(* Invalid: [close] may only appear in a lower bound *)

module type Bad = sig
  val bad : 'a @ [< close('m)] -> 'a @ [> 'm]
end
[%%expect{|
Line 2, characters 20-25:
2 |   val bad : 'a @ [< close('m)] -> 'a @ [> 'm]
                        ^^^^^
Error: The mode morphism "close" may only appear in a lower bound.
|}]

(* Invalid: unknown morphism *)

module type Bad = sig
  val bad : 'a @ [< dual('m)] -> 'a @ [> 'm]
end
[%%expect{|
Line 2, characters 20-24:
2 |   val bad : 'a @ [< dual('m)] -> 'a @ [> 'm]
                        ^^^^
Error: Unrecognized mode morphism "dual".
|}]

(* Invalid: duplicated axes within a [mod] group *)

module type Bad = sig
  val bad : 'a @ [< 'm mod portable nonportable] -> 'a @ [> 'm]
end
[%%expect{|
Line 2, characters 36-47:
2 |   val bad : 'a @ [< 'm mod portable nonportable] -> 'a @ [> 'm]
                                        ^^^^^^^^^^^
Error: The portability axis has already been specified.
|}]
