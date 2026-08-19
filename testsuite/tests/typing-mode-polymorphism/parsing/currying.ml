(* TEST
 flags = "-extension unique -extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

(*
 * This file tests parsing of curry modes of polymorphic function types
*)

(* Currying *)

module type Fst = sig
  val fst : 'a @ [< 'm] -> 'b @ 'n -> 'a @ [> 'm]
end
[%%expect{|
module type Fst = sig val fst : 'a @ [< 'm] -> 'b @ 'n -> 'a @ [> 'm] end
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
  sig val fst : 'a @ [< 'm & global] -> 'b @ 'n -> 'a @ [> 'm] end
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
      'a @ local -> 'b @ [< 'm & global] -> 'c @ 'n -> 'b @ [> 'm]
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
  sig val many : 'a @ 'p -> 'b @ 'o -> 'c @ 'n -> unit @ 'm end
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
      ('a @ [> 'n] -> 'b @ [< 'm]) @ 'o -> 'a @ [< 'n] -> 'b @ [> 'm]
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
       Type "int @ 'm -> (int -> int) @ [< global]" is not compatible with type
         "int -> (int -> int) @ local"
|}]

module type Inner_local_poly = sig
  val f : (int @ [> 'm] -> int @ 'n -> int @ [< 'm]) @ local -> unit
end
[%%expect{|
module type Inner_local_poly =
  sig val f : (int @ [> 'm] -> int @ 'n -> int @ [< 'm]) @ local -> unit end
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
         val f : (int @ [> 'm] -> int @ 'n -> int @ [< 'm]) @ local -> unit
       The type
         "(int @ 'o -> (int @ 'n -> int @ [< 'm]) @ [< global]) @ 'p ->
         unit @ [> dynamic]"
       is not compatible with the type
         "(int @ [> 'm] -> int @ 'n -> int @ [< 'm]) @ local -> unit"
       Type "int @ 'o -> (int @ 'n -> int @ [< 'm]) @ [< global]"
       is not compatible with type
         "int @ [> 'm] ->
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
  sig val fst : 'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ local once end
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
  sig
    val fst :
      'a @ [< 'm > aliased] -> ('b @ 'n -> 'a @ [> 'm | aliased]) @ local
  end
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
         val fst :
           'a @ [< 'm > aliased] ->
           ('b @ 'n -> 'a @ [> 'm | aliased]) @ local
       The type
         "'a @ [< 'm > past('o) | aliased] ->
         ('b @ [> past('n)] -> 'a @ [> 'm | aliased]) @ [> close('m) | local]"
       is not compatible with the type
         "'a @ [< 'p & past('o) > aliased] ->
         ('b @ [< past('n)] -> 'a @ [> 'p | aliased]) @ local"
|}]

(* a [once] argument is not sufficient; the argument can be [unique] which gets
   closed over as [once] *)

module type Explicit_curry_many_arg = sig
  val fst : 'a @ [< 'm & many] -> ('b @ 'n -> 'a @ [> 'm]) @ local
end
[%%expect{|
module type Explicit_curry_many_arg =
  sig val fst : 'a @ [< 'm & many] -> ('b @ 'n -> 'a @ [> 'm]) @ local end
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
         val fst : 'a @ [< 'm & many] -> ('b @ 'n -> 'a @ [> 'm]) @ local
       The type
         "'a @ [< 'm > past('o)] ->
         ('b @ [> past('n)] -> 'a @ [> 'm]) @ [> close('m) | local]"
       is not compatible with the type
         "'a @ [< 'p & past('o) & many] ->
         ('b @ [< past('n)] -> 'a @ [> 'p]) @ local"
|}]

(* if the argument is both [many] and [aliased], then the curry mode is
  guaranteed to be [many], and [> close('m)] is more general than @ many *)

module type Explicit_curry_many_aliased = sig
  val fst : 'a @ [< 'm & many > aliased] -> ('b @ 'n -> 'a @ [> 'm]) @ local
end
[%%expect{|
module type Explicit_curry_many_aliased =
  sig
    val fst :
      'a @ [< 'm & many > aliased] ->
      ('b @ 'n -> 'a @ [> 'm | aliased]) @ local
  end
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
  sig val fst : 'a @ [< 'm & global] -> ('b @ 'n -> 'a @ [> 'm]) @ once end
|}]

module M_good_explicit_curry_global : Explicit_curry_global_arg = struct
  let fst x _ = x
end
[%%expect{|
Lines 1-3, characters 66-3:
1 | ..................................................................struct
2 |   let fst x _ = x
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val fst : 'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)]
         end
       is not included in
         Explicit_curry_global_arg
       Values do not match:
         val fst : 'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)]
       is not included in
         val fst : 'a @ [< 'm & global] -> ('b @ 'n -> 'a @ [> 'm]) @ once
       The type
         "'a @ [< 'm > past('o)] ->
         ('b @ [> past('n)] -> 'a @ [> 'm]) @ [> close('m)]"
       is not compatible with the type
         "'a @ [< 'p & past('o) & global] ->
         ('b @ [< past('n)] -> 'a @ [> 'p]) @ once"
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
      ('b @ [< past('o)] ->
       ('c @ [< past('n)] -> 'a @ [< past('m) > 'q]) @ [> past('m) | past('n) | past('o) | past('p) | local once stateful]) @ local
      once
  end
|}]

module M_explicit_curry_base : Explicit_curry_base = struct
  let f x _ _ = x
end
[%%expect{|
module M_explicit_curry_base : Explicit_curry_base
|}]
