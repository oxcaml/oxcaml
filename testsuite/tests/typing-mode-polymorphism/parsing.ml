(* TEST
 flags = "-extension unique -extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

(*
 * This file tests parsing of polymorphic mode variables and bounds
*)

(* Simple signatures *)

module type Id = sig
  val id : 'a @ [< 'm] -> 'a @ [> 'm]
end
[%%expect{|
module type Id = sig val id : 'a @ [< 'm] -> 'a @ [> 'm] end
|}]

module M_id : Id = struct
  let id x = x
end
[%%expect{|
module M_id : Id
|}]

let use_portable (_ @ portable) = ()
let f (x @ portable) = use_portable (M_id.id x)
[%%expect{|
val use_portable : 'a @ [< portable] -> unit @ 'm = <fun>
val f : 'a @ [< global portable] -> unit @ [> dynamic] = <fun>
|}]

module type Const = sig
  val const : 'a @ 'm -> unit @ 'n
end
[%%expect{|
module type Const = sig val const : 'a @ 'n -> unit @ 'm end
|}]

module M_const : Const = struct
  let const _ = ()
end
[%%expect{|
module M_const : Const
|}]

(* An unconstrained signature is not subsumed by a constrained
   implementation *)

module M_bad : Const = struct
  let const x = x
end
[%%expect{|
Lines 1-3, characters 23-3:
1 | .......................struct
2 |   let const x = x
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig val const : 'a @ [< 'm] -> 'a @ [> 'm] end
       is not included in
         Const
       Values do not match:
         val const : 'a @ [< 'm] -> 'a @ [> 'm]
       is not included in
         val const : 'a @ 'n -> unit @ 'm
       The type "'a @ [< 'm] -> 'a @ [> 'm]" is not compatible with the type
         "'a @ 'o -> unit @ 'n"
       Type "'a" is not compatible with type "unit"
|}]

(* Bounds *)

(* A bare mode variable may be shared between positions *)

module type Shared = sig
  val shared : 'a @ 'm -> 'b @ 'm -> unit @ 'n
end
[%%expect{|
module type Shared =
  sig
    val shared :
      'a @ [< 'm > 'o] ->
      ('b @ [< 'o > 'm] -> unit @ 'n) @ [> close('m) | local stateful]
  end
|}]

(* Bounds do not need a counterpart *)

module type No_counterpart = sig
  val f : 'a @ [< 'm] -> 'b @ 'n
end
[%%expect{|
module type No_counterpart = sig val f : 'a @ 'n -> 'b @ 'm end
|}]

(* Bounds are not restricted by variance *)

module type No_variance = sig
  val f : 'a @ [> 'm] -> 'a @ [< 'm]
end
[%%expect{|
module type No_variance = sig val f : 'a @ [> 'm] -> 'a @ [< 'm] end
|}]

module Bad_variance : No_variance = struct
  let f x = x
end
[%%expect{|
Lines 1-3, characters 36-3:
1 | ....................................struct
2 |   let f x = x
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a @ [< 'm] -> 'a @ [> 'm] end
       is not included in
         No_variance
       Values do not match:
         val f : 'a @ [< 'm] -> 'a @ [> 'm]
       is not included in
         val f : 'a @ [> 'm] -> 'a @ [< 'm]
       The type "'a @ [< 'm > past('n)] -> 'a @ [> 'm]"
       is not compatible with the type "'a @ [< past('n) > 'o] -> 'a @ [< 'o]"
|}]

module No_variance_inhabited : sig
  val f : 'a @ [> 'm] -> int * int @ [< 'm]
end = struct
  let f x = (41, 42)
end
[%%expect{|
module No_variance_inhabited :
  sig val f : 'a @ [> 'm] -> int * int @ [< 'm] end
|}]

(* Combined bounds *)

module type Combined = sig
  val f : 'a @ [< 'm > 'n] -> 'a @ [< 'n > 'm]
end
[%%expect{|
module type Combined = sig val f : 'a @ [< 'n > 'm] -> 'a @ [< 'm > 'n] end
|}]

(* notice how the [dynamic] lower bound propagates to the result:
   let p1/p2 denote the mode variables in the argument/result.
   [f] collects the following constraints:
   - p1 < 'm, p1 < portable
   - 'n < p1, dynamic < p1
   - 'm < p2
   by transitivity, we get: dynamic < p2 *)
module type Combined_consts = sig
  val f : 'a @ [< 'm & portable many > 'n | dynamic] -> 'a @ [> 'm | aliased]
end
[%%expect{|
module type Combined_consts =
  sig
    val f : 'a @ [< 'm & many portable] -> 'a @ [> 'm | aliased dynamic]
  end
|}]

let use_static (x @ static) = ()
[%%expect{|
val use_static : 'a @ [< static] -> unit @ 'm = <fun>
|}]

module Good_combined_consts : Combined_consts = struct
  let f x = x
end
[%%expect{|
module Good_combined_consts : Combined_consts
|}]

let foo x =
  let y = Good_combined_consts.f x in
  use_static y
[%%expect{|
Line 3, characters 13-14:
3 |   use_static y
                 ^
Error: This value is "dynamic" but is expected to be "static".
|}]

module Bad_constant : Combined_consts = struct
  let f x = use_static x; x
end
[%%expect{|
Lines 1-3, characters 40-3:
1 | ........................................struct
2 |   let f x = use_static x; x
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a @ [< 'm & many static] -> 'a @ [> 'm | aliased] end
       is not included in
         Combined_consts
       Values do not match:
         val f : 'a @ [< 'm & many static] -> 'a @ [> 'm | aliased]
       is not included in
         val f : 'a @ [< 'm & many portable] -> 'a @ [> 'm | aliased dynamic]
       The type "'a @ [< 'm & many static] -> 'a @ [> 'm | aliased]"
       is not compatible with the type
         "'a @ [< 'n & many portable] -> 'a @ [> 'n | aliased dynamic]"
|}]

(* Constant bounds *)

module M_expect_portable : sig
  val expect_portable : 'a @ [< portable] -> unit @ 'n
end = struct
  let expect_portable _ = ()
end

let ok (x @ portable) = M_expect_portable.expect_portable x
[%%expect{|
module M_expect_portable :
  sig val expect_portable : 'a @ [< portable] -> unit @ 'm end
val ok : 'a @ [< portable] -> unit @ [> dynamic] = <fun>
|}]

let bad (x @ nonportable) = M_expect_portable.expect_portable x
[%%expect{|
Line 1, characters 62-63:
1 | let bad (x @ nonportable) = M_expect_portable.expect_portable x
                                                                  ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

module type Id_portable = sig
  val id_portable : 'a @ [< 'm & portable] -> 'a @ [> 'm]
end
[%%expect{|
module type Id_portable =
  sig val id_portable : 'a @ [< 'm & portable] -> 'a @ [> 'm] end
|}]

module type Lower_const = sig
  val lower_const : 'a @ 'n -> 'a @ [> dynamic]
end
[%%expect{|
module type Lower_const =
  sig val lower_const : 'a @ 'm -> 'a @ [> dynamic] end
|}]

(* A constant bound is a single constant mode annotation at the end of the
   bound *)

module type Multi_const = sig
  val multi : 'a @ [< 'm & portable global] -> 'a @ [> 'm]
end
[%%expect{|
module type Multi_const =
  sig val multi : 'a @ [< 'm & global portable] -> 'a @ [> 'm] end
|}]

(* Currying *)

module type Fst = sig
  val fst : 'a @ [< 'm] -> 'b @ 'n -> 'a @ [> 'm]
end
[%%expect{|
module type Fst =
  sig
    val fst :
      'a @ [< 'm] ->
      ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | local stateful]
  end
|}]

module M_fst : Fst = struct
  let fst a _ = a
end
[%%expect{|
module M_fst : Fst
|}]

module M_fst_global : sig
  val fst : 'a @ [< 'm & global] -> 'b @ 'n -> 'a @ [> 'm]
end = struct
  let fst a _ = a
end
[%%expect{|
module M_fst_global :
  sig
    val fst :
      'a @ [< 'm & global] ->
      ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | stateful]
  end
|}]

module type Local_id = sig
  val local_id : 'a @ local -> 'b @ [< 'm] -> 'b @ [> 'm]
end
[%%expect{|
module type Local_id =
  sig val local_id : 'a @ local -> 'b @ [< 'm] -> 'b @ [> 'm] end
|}]

module M_local_id : Local_id = struct
  let local_id x y = y
end
[%%expect{|
module M_local_id : Local_id
|}]

module type Local_prefix = sig
  val local_prefix : 'a @ local -> 'b @ [< 'm & global] -> 'c @ 'n -> 'b @ [> 'm]
end
[%%expect{|
module type Local_prefix =
  sig
    val local_prefix :
      'a @ local ->
      'b @ [< 'm & global] ->
      ('c @ 'n -> 'b @ [> 'm]) @ [> close('m) | local stateful]
  end
|}]

module M_local_prefix : Local_prefix = struct
  let local_prefix x y z = y
end
[%%expect{|
module M_local_prefix : Local_prefix
|}]

module type Many = sig
  val many : 'a @ 'm -> 'b @ 'n -> 'c @ 'k -> unit @ 's
end
[%%expect{|
module type Many =
  sig
    val many :
      'a @ [< past('n)] ->
      ('b @ [< past('o)] ->
       ('c @ 'q -> unit @ 'p) @ [> past('o) | past('m) | local stateful]) @ [< past('m) > past('n) | local stateful]
  end
|}]

module M_many : Many = struct
  let many _ _ _ = ()
end
[%%expect{|
module M_many : Many
|}]

module type Apply = sig
  val apply : ('a @ [> 'm] -> 'b @ [< 'n]) @ 'k -> 'a @ [< 'm] -> 'b @ [> 'n]
end
[%%expect{|
module type Apply =
  sig
    val apply :
      ('a @ [> 'n] -> 'b @ [< 'm]) @ [< past('o)] ->
      ('a @ [< 'n] -> 'b @ [> 'm]) @ [> past('o) | local stateful]
  end
|}]

(* currying in argument position *)

module type Inner_local = sig
  val f : (int -> int -> int) @ local -> unit
end
[%%expect{|
module type Inner_local = sig val f : (int -> int -> int) @ local -> unit end
|}]

module M_inner_local : Inner_local = struct
  let f g = let _ =  (g 1) in ()
end
[%%expect{|
module M_inner_local : Inner_local
|}]

module M_bad_inner_local : Inner_local = struct
  let use_global (x @ global) = ()

  let f g = use_global (g 1)
end
[%%expect{|
Lines 1-5, characters 41-3:
1 | .........................................struct
2 |   let use_global (x @ global) = ()
3 |
4 |   let f g = use_global (g 1)
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val use_global : 'a @ [< global] -> unit @ 'm
           val f : (int @ 'm -> 'a @ [< global]) @ 'n -> unit @ [> dynamic]
         end
       is not included in
         Inner_local
       Values do not match:
         val f : (int @ 'm -> 'a @ [< global]) @ 'n -> unit @ [> dynamic]
       is not included in
         val f : (int -> int -> int) @ local -> unit
       The type
         "(int @ 'm -> (int -> int) @ [< global]) @ 'n -> unit @ [> dynamic]"
       is not compatible with the type "(int -> int -> int) @ local -> unit"
       Type "int @ 'm -> (int -> int) @ 'o" is not compatible with type
         "int -> (int -> int) @ local"
|}]

module type Inner_local_poly = sig
  val f : (int @ [> 'm] -> int @ 'n -> int @ [< 'm]) @ local -> unit
end
[%%expect{|
module type Inner_local_poly =
  sig
    val f :
      (int @ [< past('m) > 'n] -> (int @ 'o -> int @ [< 'n]) @ [> past('m)]) @ local ->
      unit
  end
|}]

module M_inner_local_poly : Inner_local_poly = struct
  let f g = let _ =  (g 1) in ()
end
[%%expect{|
module M_inner_local_poly : Inner_local_poly
|}]

module M_bad_inner_local_poly : Inner_local_poly = struct
  let use_global (x @ global) = ()

  let f g = use_global (g 1)
end
[%%expect{|
Lines 1-5, characters 51-3:
1 | ...................................................struct
2 |   let use_global (x @ global) = ()
3 |
4 |   let f g = use_global (g 1)
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val use_global : 'a @ [< global] -> unit @ 'm
           val f : (int @ 'm -> 'a @ [< global]) @ 'n -> unit @ [> dynamic]
         end
       is not included in
         Inner_local_poly
       Values do not match:
         val f : (int @ 'm -> 'a @ [< global]) @ 'n -> unit @ [> dynamic]
       is not included in
         val f :
           (int @ [< past('m) > 'n] ->
            (int @ 'o -> int @ [< 'n]) @ [> past('m)]) @ local ->
           unit
       The type
         "(int @ 'o -> (int @ 'n -> int @ [< 'm]) @ [< global]) @ 'p ->
         unit @ [> dynamic]"
       is not compatible with the type
         "(int @ [< past('q) > 'm] ->
          (int @ 'n -> int @ [< 'm]) @ [> past('q)]) @ local ->
         unit"
       Type "int @ 'o -> (int @ 'n -> int @ [< 'm]) @ 'mm0"
       is not compatible with type
         "int @ [< past('q) > 'm] ->
         (int @ 'n -> int @ [< 'm]) @ [> past('q) | local stateful]"
|}]

(* Explicitly written curry modes *)

(* [local once] falls within the implicit default curry mode for an unbounded
   argument, so the printer omits the annotation and the identity
   implementation is accepted *)

module type Explicit_curry_default = sig
  val fst : 'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ local once
end
[%%expect{|
module type Explicit_curry_default =
  sig val fst : 'a @ [< 'm] -> 'b @ 'n -> 'a @ [> 'm] end
|}]

module M_explicit_curry_default : Explicit_curry_default = struct
  let fst x _ = x
end
[%%expect{|
module M_explicit_curry_default : Explicit_curry_default
|}]

(* a written [local] curry promises a [many] closure, but the partial
   application may capture a unique value: [close('m)] can be [once].
   [> close('m)] is thus not more general than @ local
   (which implicitly is @ many) *)

module type Explicit_curry_many = sig
  val fst : 'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ local
end
[%%expect{|
module type Explicit_curry_many =
  sig val fst : 'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ local end
|}]

module M_bad_explicit_curry_many : Explicit_curry_many = struct
  let fst x _ = x
end
[%%expect{|
Lines 1-3, characters 57-3:
1 | .........................................................struct
2 |   let fst x _ = x
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val fst :
             'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | local]
         end
       is not included in
         Explicit_curry_many
       Values do not match:
         val fst :
           'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | local]
       is not included in
         val fst : 'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ local
       The type
         "'a @ [< 'm > past('o)] ->
         ('b @ [> past('n)] -> 'a @ [> 'm]) @ [> close('m) | local]"
       is not compatible with the type
         "'a @ [< 'p & past('o)] -> ('b @ [< past('n)] -> 'a @ [> 'p]) @ local"
|}]

(* an [aliased] argument is not sufficient; the argument can be [once] *)

module type Explicit_curry_aliased = sig
  val fst : 'a @ [< 'm > aliased] -> ('b @ 'n -> 'a @ [> 'm]) @ local
end
[%%expect{|
module type Explicit_curry_aliased =
  sig val fst : 'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm | aliased]) @ local end
|}]

module M_bad_explicit_curry_aliased : Explicit_curry_aliased = struct
  let fst x _ = x
end
[%%expect{|
Lines 1-3, characters 63-3:
1 | ...............................................................struct
2 |   let fst x _ = x
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val fst :
             'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | local]
         end
       is not included in
         Explicit_curry_aliased
       Values do not match:
         val fst :
           'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | local]
       is not included in
         val fst : 'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm | aliased]) @ local
       The type
         "'a @ [< 'm > past('o)] ->
         ('b @ [> past('n)] -> 'a @ [> 'm | aliased]) @ [> close('m) | local]"
       is not compatible with the type
         "'a @ [< 'p & past('o)] ->
         ('b @ [< past('n)] -> 'a @ [> 'p | aliased]) @ local"
|}]

(* a [once] argument is not sufficient; the argument can be [unique] which gets
   closed over as [once] *)

module type Explicit_curry_many_arg = sig
  val fst : 'a @ [< 'm & many] -> ('b @ 'n -> 'a @ [> 'm]) @ local
end
[%%expect{|
module type Explicit_curry_many_arg =
  sig val fst : 'a @ [< 'm & many] -> 'b @ 'n -> 'a @ [> 'm] end
|}]

module M_bad_explicit_curry_many_arg : Explicit_curry_many_arg = struct
  let fst x _ = x
end
[%%expect{|
Lines 1-3, characters 65-3:
1 | .................................................................struct
2 |   let fst x _ = x
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val fst :
             'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | local]
         end
       is not included in
         Explicit_curry_many_arg
       Values do not match:
         val fst :
           'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | local]
       is not included in
         val fst : 'a @ [< 'm & many] -> 'b @ 'n -> 'a @ [> 'm]
       The type
         "'a @ [< 'm > past('o)] ->
         ('b @ [> past('n)] -> 'a @ [> 'm]) @ [> close('m) | local]"
       is not compatible with the type
         "'a @ [< 'p & past('o) & many] -> 'b @ [< past('n)] -> 'a @ [> 'p]"
|}]

(* if the argument is both [many] and [aliased], then the curry mode is
  guaranteed to be [many], and [> close('m)] is more general than @ many *)

module type Explicit_curry_many_aliased = sig
  val fst : 'a @ [< 'm & many > aliased] -> ('b @ 'n -> 'a @ [> 'm]) @ local
end
[%%expect{|
module type Explicit_curry_many_aliased =
  sig val fst : 'a @ [< 'm & many] -> 'b @ 'n -> 'a @ [> 'm | aliased] end
|}]

module M_explicit_curry_many_aliased : Explicit_curry_many_aliased = struct
  let fst x _ = x
end
[%%expect{|
module M_explicit_curry_many_aliased : Explicit_curry_many_aliased
|}]

(* a written [global] curry cannot be provided when the argument may be
   local *)

module type Explicit_curry_global = sig
  val fst : 'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ global once
end
[%%expect{|
module type Explicit_curry_global =
  sig val fst : 'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ once end
|}]

module M_bad_explicit_curry_global : Explicit_curry_global = struct
  let fst x _ = x
end
[%%expect{|
Lines 1-3, characters 61-3:
1 | .............................................................struct
2 |   let fst x _ = x
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val fst :
             'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | local]
         end
       is not included in
         Explicit_curry_global
       Values do not match:
         val fst :
           'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | local]
       is not included in
         val fst : 'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ once
       The type
         "'a @ [< 'm > past('o)] ->
         ('b @ [> past('n)] -> 'a @ [> 'm]) @ [> close('m) | local]"
       is not compatible with the type
         "'a @ [< 'p & past('o)] -> ('b @ [< past('n)] -> 'a @ [> 'p]) @ once"
|}]

(* but if the argument is bound by [< global], we can write the following *)

module type Explicit_curry_global_arg = sig
  val fst : 'a @ [< 'm & global] -> ('b @ 'n -> 'a @ [> 'm]) @ global once
end
[%%expect{|
module type Explicit_curry_global_arg =
  sig val fst : 'a @ [< 'm & global] -> 'b @ 'n -> 'a @ [> 'm] end
|}]

module M_good_explicit_curry_global : Explicit_curry_global_arg = struct
  let fst x _ = x
end
[%%expect{|
module M_good_explicit_curry_global : Explicit_curry_global_arg
|}]

(* a bare variable curry parses, but no implementation can promise an
   arbitrary curry mode *)

module type Explicit_curry_var = sig
  val fst : 'a @ 'm -> ('b @ 'n -> 'a @ [> 'm]) @ 'k
end
[%%expect{|
module type Explicit_curry_var =
  sig val fst : 'a @ [< 'n] -> ('b @ 'o -> 'a @ [> 'n]) @ 'm end
|}]

module M_bad_explicit_curry_var : Explicit_curry_var = struct
  let fst x _ = x
end
[%%expect{|
Lines 1-3, characters 55-3:
1 | .......................................................struct
2 |   let fst x _ = x
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val fst :
             'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | local]
         end
       is not included in
         Explicit_curry_var
       Values do not match:
         val fst :
           'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | local]
       is not included in
         val fst : 'a @ [< 'n] -> ('b @ 'o -> 'a @ [> 'n]) @ 'm
       The type
         "'a @ [< 'm > past('o)] ->
         ('b @ [> past('n)] -> 'a @ [> 'm]) @ [> close('m) | local]"
       is not compatible with the type
         "'a @ [< 'q & past('o)] -> ('b @ [< past('n)] -> 'a @ [> 'q]) @ 'p"
|}]

(* [> close('m)] is not more general than [> 'm] *)

module type Explicit_curry_bound = sig
  val fst : 'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> 'm | local]
end
[%%expect{|
module type Explicit_curry_bound =
  sig
    val fst : 'a @ [< 'n & 'm] -> ('b @ 'o -> 'a @ [> 'n]) @ [> 'm | local]
  end
|}]

module M_bad_explicit_curry_bound : Explicit_curry_bound = struct
  let fst x _ = x
end
[%%expect{|
Lines 1-3, characters 59-3:
1 | ...........................................................struct
2 |   let fst x _ = x
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val fst :
             'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | local]
         end
       is not included in
         Explicit_curry_bound
       Values do not match:
         val fst :
           'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | local]
       is not included in
         val fst :
           'a @ [< 'n & 'm] -> ('b @ 'o -> 'a @ [> 'n]) @ [> 'm | local]
       The type
         "'a @ [< 'm > past('o)] ->
         ('b @ [> past('n)] -> 'a @ [> 'm]) @ [> close('m) | local]"
       is not compatible with the type
         "'a @ [< 'q & 'p & past('o)] ->
         ('b @ [< past('n)] -> 'a @ [> 'q]) @ [> 'p | local]"
|}]

(* a written curry mode is the base of the deeper implicit curries: the
   [local once] below folds into the curry mode between ['n] and ['k] *)

module type Explicit_curry_base = sig
  val f : 'a @ 'm -> ('b @ 'n -> 'c @ 'k -> 'a @ [> 'm]) @ local once
end
[%%expect{|
module type Explicit_curry_base =
  sig
    val f :
      'a @ [< 'q & past('p)] ->
      'b @ [< past('o)] ->
      ('c @ [< past('n)] -> 'a @ [< past('m) > 'q]) @ [> past('m) | past('n) | past('o) | past('p) | local once stateful]
  end
|}]

module M_explicit_curry_base : Explicit_curry_base = struct
  let f x _ _ = x
end
[%%expect{|
module M_explicit_curry_base : Explicit_curry_base
|}]

(* Invalid signatures *)

(* Invalid: constant modes cannot be mixed with mode variables *)

module type Bad = sig
  val bad : 'a @ local 'm -> 'a @ [> 'm]
end
[%%expect{|
Line 2, characters 17-22:
2 |   val bad : 'a @ local 'm -> 'a @ [> 'm]
                     ^^^^^
Error: Constant modes and mode variables cannot be mixed in a mode annotation.
|}]

(* Invalid: a mode annotation is a single variable or a single bounds
   annotation *)

module type Bad = sig
  val bad : 'a @ 'm 'n -> unit @ 'k
end
[%%expect{|
Line 795, characters 17-19:
795 |   val bad : 'a @ 'm 'n -> unit @ 'k
                       ^^
Error: A mode annotation must be a single mode variable or a single bounds annotation.
|}]

module type Bad = sig
  val bad : 'a @ [< 'm] [> 'n] -> unit @ 'k
end
[%%expect{|
Line 2, characters 17-23:
2 |   val bad : 'a @ [< 'm] [> 'n] -> unit @ 'k
                     ^^^^^^
Error: A mode annotation must be a single mode variable or a single bounds annotation.
|}]

(* Invalid: duplicated axes within a constant bound *)

module type Bad = sig
  val bad : 'a @ [< 'm & portable nonportable] -> 'a @ [> 'm]
end
[%%expect{|
Line 817, characters 34-45:
817 |   val bad : 'a @ [< 'm & portable nonportable] -> 'a @ [> 'm]
                                        ^^^^^^^^^^^
Error: The portability axis has already been specified.
|}]

(* Invalid: mode variables are only allowed on function types *)

let (x @ 'm) = fun y -> y
[%%expect{|
Line 1, characters 9-11:
1 | let (x @ 'm) = fun y -> y
             ^^
Error: Mode variables and mode bounds are only allowed on function types.
|}]

(* Invalid: mode variables in let bindings and expression annotations *)

let f : 'a @ [< 'm] -> 'a @ [> 'm] = fun x -> x
[%%expect{|
Line 1, characters 13-19:
1 | let f : 'a @ [< 'm] -> 'a @ [> 'm] = fun x -> x
                 ^^^^^^
Error: Mode variables are only allowed in the types of signature items.
|}]

let i = (fun x -> x : 'a @ [< 'm] -> 'a @ [> 'm])
[%%expect{|
Line 1, characters 27-33:
1 | let i = (fun x -> x : 'a @ [< 'm] -> 'a @ [> 'm])
                               ^^^^^^
Error: Mode variables are only allowed in the types of signature items.
|}]

(* Invalid: mode variables in type declarations *)

type ('a, 'b) bad = { f : 'a @ [< 'm] -> 'b @ [> 'm] }
[%%expect{|
Line 1, characters 31-37:
1 | type ('a, 'b) bad = { f : 'a @ [< 'm] -> 'b @ [> 'm] }
                                   ^^^^^^
Error: Mode variables are only allowed in the types of signature items.
|}]
