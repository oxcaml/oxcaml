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
