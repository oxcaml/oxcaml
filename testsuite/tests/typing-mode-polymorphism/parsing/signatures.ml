(* TEST
 flags = "-extension unique -extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

(*
 * This file tests parsing of polymorphic mode variables and bounds in
 * signatures
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
  sig val shared : 'a @ [< 'o > 'n] -> 'b @ [< 'n > 'o] -> unit @ 'm end
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
    val f :
      'a @ [< 'm & many portable > dynamic] -> 'a @ [> 'm | aliased dynamic]
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
         val f :
           'a @ [< 'm & many portable > dynamic] ->
           'a @ [> 'm | aliased dynamic]
       The type "'a @ [< 'm & many static] -> 'a @ [> 'm | aliased]"
       is not compatible with the type
         "'a @ [< 'n & many portable > dynamic] ->
         'a @ [> 'n | aliased dynamic]"
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
