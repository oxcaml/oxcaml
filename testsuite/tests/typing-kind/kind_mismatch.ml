(* TEST
   expect;
*)

(** Error messages for kind mismatches. *)

module T0 : sig
  type t
end = struct
  type t = unit
end

type t0 = T0.t = { a0 : int }

[%%expect
{|
module T0 : sig type t end
Line 9, characters 0-29:
9 | type t0 = T0.t = { a0 : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "T0.t"
       The original is abstract, but this is a record.
|}]

type t2a = ..

type t2b = t2a =
  | A2
  | B2

[%%expect
{|
type t2a = ..
Lines 3-5, characters 0-6:
3 | type t2b = t2a =
4 |   | A2
5 |   | B2
Error: This variant or record definition does not match that of type "t2a"
       The original is an extensible variant, but this is a variant.
|}]
