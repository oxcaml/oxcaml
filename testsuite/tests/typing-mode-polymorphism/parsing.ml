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

(* Morphisms in bounds: [past('m)], [close('m)], and postfix [mod c] *)

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
module type Past_upper =
  sig
    val f :
      'a @ [< past('m)] ->
      ('b @ 'o -> unit @ 'n) @ [> past('m) | local stateful]
  end
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
  sig val f : 'a @ [< 'm] -> 'a @ [> 'm mod global forkable unyielding] end
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
module M_ref :
  sig
    val alloc :
      'a @ [< 'm mod aliased dynamic & global many] ->
      'a myref @ [> 'm | stateful]
  end
|}]

module M_ref_no_mod : sig
  val alloc : 'a @ [< 'm & global many] -> 'a myref @ [> 'm | stateful]
end = struct
  let alloc x = { i = x }
end
[%%expect{|
module M_ref_no_mod :
  sig
    val alloc : 'a @ [< 'm & global many] -> 'a myref @ [> 'm | stateful]
  end
|}]

let use_unique (_ @ unique) = ()

let ok (x @ aliased) = use_unique (M_ref.alloc x)
[%%expect{|
val use_unique : 'a @ [< unique] -> unit @ 'm = <fun>
val ok : 'a @ [< global many] -> unit @ [> dynamic] = <fun>
|}]

let bad (x @ aliased) = use_unique (M_ref_no_mod.alloc x)
[%%expect{|
Line 1, characters 35-57:
1 | let bad (x @ aliased) = use_unique (M_ref_no_mod.alloc x)
                                       ^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "aliased" but is expected to be "unique".
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
val ok2 : 'a @ [< global] -> unit @ [> dynamic] = <fun>
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

let ok3 (b @ nonportable) = use_portable (M_pget.pget b)
[%%expect{|
val ok3 : 'a pbox @ [< global] -> unit @ [> dynamic] = <fun>
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
Line 2, characters 17-19:
2 |   val bad : 'a @ 'm 'n -> unit @ 'k
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
Line 2, characters 34-45:
2 |   val bad : 'a @ [< 'm & portable nonportable] -> 'a @ [> 'm]
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
