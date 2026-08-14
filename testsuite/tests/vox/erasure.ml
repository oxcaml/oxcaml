(* TEST
 expect;
*)

(* The erasure axis: Retained < Erased, legacy Retained. An erased value does
   not exist at run time; it may only be used where an erased value is
   expected. *)

(* Defaults: unannotated code is retained and prints as today. *)
let id x = x
[%%expect{|
val id : 'a -> 'a = <fun>
|}]

(* A retained value is accepted where erased is expected. *)
let g (x : int @ erased) = ()
let () = g 5
[%%expect{|
val g : int @ erased -> unit = <fun>
|}]

(* An erased value is rejected where retained is expected. *)
let bad () =
  let x = erased_ 5 in
  x + 1
[%%expect{|
Line 3, characters 2-3:
3 |   x + 1
      ^
Error: This value is "erased" but is expected to be "retained".
|}]

(* An erased value returned from a retained function. *)
let ret x = erased_ (x + 1)
[%%expect{|
val ret : int -> int @ erased = <fun>
|}]

(* Erased values are rejected as if conditions and match scrutinees in
   retained code... *)
let bad () =
  let b = erased_ true in
  if b then 1 else 2
[%%expect{|
Line 3, characters 5-6:
3 |   if b then 1 else 2
         ^
Error: This value is "erased" but is expected to be "retained".
|}]

let bad () =
  let b = erased_ true in
  match b with true -> 1 | false -> 2
[%%expect{|
Line 3, characters 15-19:
3 |   match b with true -> 1 | false -> 2
                   ^^^^
Error: This value is "erased" but is expected to be "retained".
|}]

(* ...and accepted inside erased_. *)
let ok () =
  let b = erased_ true in
  erased_ (if b then 1 else 2)
[%%expect{|
val ok : unit -> int @ erased = <fun>
|}]

let ok () =
  let b = erased_ true in
  erased_ (match b with true -> 1 | false -> 2)
[%%expect{|
val ok : unit -> int @ erased = <fun>
|}]

(* Binding patterns do not read: variables, wildcards and aliases are fine,
   destructuring is not. *)
let ok () =
  let p = erased_ (1, 2) in
  let _q = p in
  ()
[%%expect{|
val ok : unit -> unit = <fun>
|}]

let bad () =
  let p = erased_ (1, 2) in
  let (a, b) = p in
  a + b
[%%expect{|
Line 3, characters 15-16:
3 |   let (a, b) = p in
                   ^
Error: This value is "erased" but is expected to be "retained".
|}]

(* Application: an erased function cannot be called... *)
let bad () =
  let f = erased_ (fun x -> x + 1) in
  f 3
[%%expect{|
Line 3, characters 2-3:
3 |   f 3
      ^
Error: This value is "erased" but is expected to be "retained".
|}]

(* ...but erased_ (f x) is fine for the same f. *)
let ok () =
  let f = erased_ (fun x -> x + 1) in
  erased_ (f 3)
[%%expect{|
val ok : unit -> int @ erased = <fun>
|}]

(* An erased parameter and an erased function are independent. *)
let takes_erased_param (x : int @ erased) (y : int) = y
let apply_it (f : (int @ erased -> int -> int)) x y = f x y
[%%expect{|
val takes_erased_param : int @ erased -> int -> int = <fun>
val apply_it : (int @ erased -> int -> int) -> int -> int -> int = <fun>
|}]

(* Arguments are not erased silently: a retained argument is usable at an
   erased parameter (evaluated for effect, dropped at the boundary), and an
   erased argument passes as-is. *)
let call () =
  let x = erased_ 5 in
  takes_erased_param x 1 + takes_erased_param 42 2
[%%expect{|
val call : unit -> int = <fun>
|}]

(* Closures: a retained closure may capture an erased value and stays
   callable. *)
let f (u : unit) (z : int @ erased) = ()
let mk () =
  let x = erased_ 42 in
  let clo = fun y -> f y x in
  clo ()
[%%expect{|
val f : unit -> int @ erased -> unit = <fun>
val mk : unit -> unit = <fun>
|}]

(* The same closure is rejected when it uses the capture at a retained
   position. *)
let bad () =
  let x = erased_ 42 in
  fun y -> x + y
[%%expect{|
Line 3, characters 11-12:
3 |   fun y -> x + y
               ^
Error: This value is "erased" but is expected to be "retained".
|}]

(* The body rule: erased_ over a lambda makes the body an erased context. *)
let ok () =
  let g = erased_ (fun (y : int) -> y + 1) in
  let _k = erased_ (fun y -> g y) in
  ()
[%%expect{|
val ok : unit -> unit = <fun>
|}]

(* An erased closure may capture a retained value. *)
let ok (r : int) =
  let _k = erased_ (fun y -> r + y) in
  ()
[%%expect{|
val ok : int -> unit = <fun>
|}]

(* Partial application across an erased parameter does not erase the
   result. *)
let use () =
  let x = erased_ 5 in
  let h = takes_erased_param x in
  h 3
[%%expect{|
val use : unit -> int = <fun>
|}]

(* Inference direction: modes zap to legacy, so an unannotated binding cannot
   drift to Erased and vanish. *)
let quiet = 5
let quiet_use (h : int -> int) = h quiet
[%%expect{|
val quiet : int = 5
val quiet_use : (int -> int) -> int = <fun>
|}]

(* A structure cannot hold an erased binding: nothing would exist to put in
   the module block. *)
module M = struct
  let x = erased_ 5
end
[%%expect{|
File "_none_", line 1:
Error: The structure is "erased"
         because it contains the module "M" defined as the module at lines 1-3, characters 11-3
         which is "erased"
         because it contains the value "x" defined as the expression at line 2, characters 6-7
         which is "erased".
       However, the structure highlighted is expected to be "retained"
         because it is a top-level clause and thus always at the legacy modes.
|}]

(* Erasure does not cross: even immediates stay erased. If int crossed
   erasure this would be accepted. *)
let bad (x : int @ erased) = x * 2
[%%expect{|
Line 1, characters 29-30:
1 | let bad (x : int @ erased) = x * 2
                                 ^
Error: This value is "erased" but is expected to be "retained".
|}]

(* An erased result may be returned from a retained function: return position
   is covariant. *)
let ok (x : int @ erased) : int = x
[%%expect{|
val ok : int @ erased -> int @ erased = <fun>
|}]

(* mod erased is not a thing: types cannot cross erasure. *)
type t : value mod erased
[%%expect{|
Line 1, characters 19-25:
1 | type t : value mod erased
                       ^^^^^^
Error: Unrecognized modifier erased.
|}]

(* Sealing, return position: a retained-returning implementation is accepted
   against an erased-returning signature, the reverse is rejected. *)
module Ok : sig
  val f : int -> int @ erased
end = struct
  let f x = x + 1
  let _force = f 0 + 1 (* really retained: the result is used *)
end
[%%expect{|
module Ok : sig val f : int -> int @ erased end
|}]

module Bad : sig
  val f : int -> int
end = struct
  let f x = erased_ (x + 1)
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f x = erased_ (x + 1)
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> int @ erased end
       is not included in
         sig val f : int -> int end
       Values do not match:
         val f : int -> int @ erased
       is not included in
         val f : int -> int
       The type "int -> int @ erased" is not compatible with the type
         "int -> int"
|}]

(* Sealing, argument position: invariance. A retained-parameter signature
   over an erased-parameter implementation is the ABI-unsafe direction that
   contravariance would otherwise permit. *)
module Bad_abi : sig
  val f : int -> unit
end = struct
  let f (x : int @ erased) = ()
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : int @ erased) = ()
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : int @ erased -> unit end
       is not included in
         sig val f : int -> unit end
       Values do not match:
         val f : int @ erased -> unit
       is not included in
         val f : int -> unit
       The type "int @ erased -> unit" is not compatible with the type
         "int -> unit"
|}]

(* ...and the reverse is rejected too, since the rule is invariance. *)
module Bad_rev : sig
  val f : int @ erased -> unit
end = struct
  let r = ref 0
  let f (x : int) = r := x
end
[%%expect{|
Lines 3-6, characters 6-3:
3 | ......struct
4 |   let r = ref 0
5 |   let f (x : int) = r := x
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig val r : int ref val f : int -> unit end
       is not included in
         sig val f : int @ erased -> unit end
       Values do not match:
         val f : int -> unit
       is not included in
         val f : int @ erased -> unit
       The type "int -> unit" is not compatible with the type
         "int @ erased -> unit"
|}]

(* The same directions through an explicit coercion. *)
let bad (f : (int @ erased -> unit)) = (f :> int -> unit)
[%%expect{|
Line 1, characters 39-57:
1 | let bad (f : (int @ erased -> unit)) = (f :> int -> unit)
                                           ^^^^^^^^^^^^^^^^^^
Error: Type "int @ erased -> unit" is not a subtype of "int -> unit"
|}]

let bad (f : int -> unit) = (f :> (int @ erased -> unit))
[%%expect{|
Line 1, characters 28-57:
1 | let bad (f : int -> unit) = (f :> (int @ erased -> unit))
                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "int -> unit" is not a subtype of "int @ erased -> unit"
|}]

(* Return position through coercion: retained-returning to erased-returning
   is accepted, the reverse rejected. *)
let ok (f : int -> int) = (f :> (int -> int @ erased))
[%%expect{|
val ok : (int -> int) -> int -> int @ erased = <fun>
|}]

let bad (f : (int -> int @ erased)) = (f :> int -> int)
[%%expect{|
Line 1, characters 38-55:
1 | let bad (f : (int -> int @ erased)) = (f :> int -> int)
                                          ^^^^^^^^^^^^^^^^^
Error: Type "int -> int @ erased" is not a subtype of "int -> int"
|}]
