(* TEST
 expect;
*)

type dense =
  | A [@tag 2]
  | B
  | C [@tag 1]

[%%expect {|
type dense = A [@tag 2] | B | C [@tag 1]
|}]

module Matching_annotation : sig
  type t =
    | A
    | B
end = struct
  type t =
    | A [@tag 0]
    | B [@tag 1]
end

[%%expect {|
module Matching_annotation : sig type t = A | B end
|}]

type duplicate =
  | D0 [@tag 0]
  | D1 [@tag 0]

[%%expect {|
Line 3, characters 7-15:
3 |   | D1 [@tag 0]
           ^^^^^^^^
Error: Two constructors cannot use the same [@tag] value 0
|}]

type duplicate_attribute =
  | DA [@tag 0] [@tag 1]

[%%expect {|
Line 2, characters 18-21:
2 |   | DA [@tag 0] [@tag 1]
                      ^^^
Error: The [@tag] attribute cannot be repeated
|}]

type negative = N [@tag (-1)]

[%%expect {|
Line 1, characters 18-29:
1 | type negative = N [@tag (-1)]
                      ^^^^^^^^^^^
Error: Negative [@tag] constructor tags are not supported yet
|}]

type sparse =
  | S0 [@tag 3]
  | S1

[%%expect {|
Line 2, characters 7-15:
2 |   | S0 [@tag 3]
           ^^^^^^^^
Error: This [@tag] constructor tag is sparse; in this version tags must form the dense range 0 to 1
|}]

type bad_payload = Bad [@tag "not an int"]

[%%expect {|
Line 1, characters 23-42:
1 | type bad_payload = Bad [@tag "not an int"]
                           ^^^^^^^^^^^^^^^^^^^
Error: Invalid [@tag] attribute: expected an integer payload
|}]

type payload = Payload : int -> payload [@tag 0]

[%%expect {|
Line 1, characters 40-48:
1 | type payload = Payload : int -> payload [@tag 0]
                                            ^^^^^^^^
Error: The [@tag] attribute can only be used on constructors without runtime fields
|}]

type poly = [ `A [@tag 0] | `B ]

[%%expect {|
Line 1, characters 14-25:
1 | type poly = [ `A [@tag 0] | `B ]
                  ^^^^^^^^^^^
Error: The [@tag] attribute is not supported on polymorphic variant constructors
|}]

type ext = ..
type ext += E [@tag 0]

[%%expect {|
type ext = ..
Line 2, characters 14-22:
2 | type ext += E [@tag 0]
                  ^^^^^^^^
Error: The [@tag] attribute can only be used on constant constructors of ordinary variant types
|}]

type ('a : value) or_null : value_or_null =
  | Null [@tag 0]
  | This of 'a
  [@@or_null]

[%%expect {|
Line 2, characters 9-17:
2 |   | Null [@tag 0]
             ^^^^^^^^
Error: The [@tag] attribute is not supported on or_null variants
|}]

(* GADT constant constructors accept explicit tags (positive). *)
type _ gadt =
  | GI : int gadt [@tag 1]
  | GB : bool gadt

[%%expect {|
type _ gadt = GI : int gadt [@tag 1] | GB : bool gadt
|}]

let gadt_tags = (Obj.magic GI : int), (Obj.magic GB : int)

[%%expect {|
val gadt_tags : int * int = (1, 0)
|}]

(* Larger permutation exercises the implicit-fill loop across >3 constants. *)
type perm =
  | P0 [@tag 4]
  | P1
  | P2 [@tag 0]
  | P3
  | P4 [@tag 2]

[%%expect {|
type perm = P0 [@tag 4] | P1 | P2 [@tag 0] | P3 | P4 [@tag 2]
|}]

let perm_tags =
  (Obj.magic P0 : int), (Obj.magic P1 : int), (Obj.magic P2 : int),
  (Obj.magic P3 : int), (Obj.magic P4 : int)

[%%expect {|
val perm_tags : int * int * int * int * int = (4, 1, 0, 3, 2)
|}]
