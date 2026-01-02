(* TEST
 expect;
*)

(* This is a regression test for the bug fixed by
   https://github.com/oxcaml/oxcaml/pull/5176 *)

type t = int or_null

module type S = sig
  type t : any mod separable
end

[%%expect{|
type t = int or_null
module type S = sig type t : any separable end
|}]


module type S' = S with type t = t

[%%expect{|
module type S' = sig type t = t end
|}]


module M : sig
  type t : immediate_or_null & value
end = struct
  type t = #(int or_null * string)
end
[%%expect{|
module M : sig type t : value_or_null non_pointer & value end
|}]

(* This is a regression test for an intermediate version of the bug fix that
   errored when estimating the kind of ['a portended or_null] because ['a
   portended] is [maybe_null] before ['a] is lowered (and thus can't be an
   argument to [or_null]). *)
type ('a : value_or_null) portended = { a : 'a } [@@unboxed]
let peek  (aon : 'a portended or_null) =
  match aon with
  | This _ -> assert false
  | Null -> assert false
;;
[%%expect{|
type ('a : value_or_null) portended = { a : 'a; } [@@unboxed]
val peek : ('a : value maybe_separable) 'b. 'a portended or_null -> 'b =
  <fun>
|}]


(* These are regression tests for an intermediate version of the bug fix that
   assumed that the type parameter of [or_null] is always [non_null]
   throughout typechecking. Although this is eventually enforced, it may be
   momentarily untrue when typechecking recursive functions due to the use of
   [type_approx]. *)
let rec ok () : 'a or_null = Null
[%%expect{|
val ok : ('a : value maybe_separable). unit -> 'a or_null = <fun>
|}]
let rec bad () : float# or_null = Null
[%%expect{|
Line 1, characters 17-23:
1 | let rec bad () : float# or_null = Null
                     ^^^^^^
Error: This type "float#" should be an instance of type
         "('a : value maybe_separable)"
       The layout of float# is float64
         because it is the unboxed version of the primitive type float.
       But the layout of float# must be a value layout
         because the type argument of or_null has layout value.
|}]
let rec bad () : 'a or_null or_null = Null
[%%expect{|
Line 1, characters 17-27:
1 | let rec bad () : 'a or_null or_null = Null
                     ^^^^^^^^^^
Error: This type "'a or_null" should be an instance of type
         "('b : value maybe_separable)"
       The layout of 'a or_null is value maybe_separable maybe_null
         because it is the primitive type or_null.
       But the layout of 'a or_null must be a sublayout of
           value maybe_separable
         because the type argument of or_null has layout value.
|}]
