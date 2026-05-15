(* TEST
 expect;
*)

type dense =
  | A [@immediate 2]
  | B
  | C [@immediate 1]

[%%expect {|
type dense = A [@immediate 2] | B | C [@immediate 1]
|}]

module Matching_annotation : sig
  type t =
    | A
    | B
end = struct
  type t =
    | A [@immediate 0]
    | B [@immediate 1]
end

[%%expect {|
module Matching_annotation : sig type t = A | B end
|}]

type duplicate =
  | D0 [@immediate 0]
  | D1 [@immediate 0]

[%%expect {|
Line 3, characters 7-21:
3 |   | D1 [@immediate 0]
           ^^^^^^^^^^^^^^
Error: Two constructors cannot use the same [@immediate] tag 0
|}]

type duplicate_attribute =
  | DA [@immediate 0] [@immediate 1]

[%%expect {|
Line 2, characters 24-33:
2 |   | DA [@immediate 0] [@immediate 1]
                            ^^^^^^^^^
Error: The [@immediate] attribute cannot be repeated
|}]

type negative = N [@immediate (-1)]

[%%expect {|
Line 1, characters 18-35:
1 | type negative = N [@immediate (-1)]
                      ^^^^^^^^^^^^^^^^^
Error: Negative [@immediate] constructor tags are not supported yet
|}]

type sparse =
  | S0 [@immediate 3]
  | S1

[%%expect {|
Line 2, characters 7-21:
2 |   | S0 [@immediate 3]
           ^^^^^^^^^^^^^^
Error: This [@immediate] constructor tag is sparse; in this version tags must form the dense range 0 to 1
|}]

type bad_payload = Bad [@immediate "not an int"]

[%%expect {|
Line 1, characters 23-48:
1 | type bad_payload = Bad [@immediate "not an int"]
                           ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid [@immediate] attribute: expected an integer payload
|}]

type payload = Payload : int -> payload [@immediate 0]

[%%expect {|
Line 1, characters 40-54:
1 | type payload = Payload : int -> payload [@immediate 0]
                                            ^^^^^^^^^^^^^^
Error: The [@immediate] attribute can only be used on constructors without runtime fields
|}]

type poly = [ `A [@immediate 0] | `B ]

[%%expect {|
Line 1, characters 14-31:
1 | type poly = [ `A [@immediate 0] | `B ]
                  ^^^^^^^^^^^^^^^^^
Error: The [@immediate] attribute is not supported on polymorphic variant constructors
|}]

type ext = ..
type ext += E [@immediate 0]

[%%expect {|
type ext = ..
Line 2, characters 14-28:
2 | type ext += E [@immediate 0]
                  ^^^^^^^^^^^^^^
Error: The [@immediate] attribute can only be used on constant constructors of ordinary variant types
|}]

type ('a : value) or_null : value_or_null =
  | Null [@immediate 0]
  | This of 'a
  [@@or_null]

[%%expect {|
Line 2, characters 9-23:
2 |   | Null [@immediate 0]
             ^^^^^^^^^^^^^^
Error: The [@immediate] attribute is not supported on or_null variants
|}]

(* GADT constant constructors accept explicit tags (positive). *)
type _ gadt =
  | GI : int gadt [@immediate 1]
  | GB : bool gadt

[%%expect {|
type _ gadt = GI : int gadt [@immediate 1] | GB : bool gadt
|}]

let gadt_tags = (Obj.magic GI : int), (Obj.magic GB : int)

[%%expect {|
val gadt_tags : int * int = (1, 0)
|}]

(* Larger permutation exercises the implicit-fill loop across >3 constants. *)
type perm =
  | P0 [@immediate 4]
  | P1
  | P2 [@immediate 0]
  | P3
  | P4 [@immediate 2]

[%%expect {|
type perm =
    P0
  [@immediate 4]
  | P1
  | P2
  [@immediate 0]
  | P3
  | P4
  [@immediate 2]
|}]

let perm_tags =
  (Obj.magic P0 : int), (Obj.magic P1 : int), (Obj.magic P2 : int),
  (Obj.magic P3 : int), (Obj.magic P4 : int)

[%%expect {|
val perm_tags : int * int * int * int * int = (4, 1, 0, 3, 2)
|}]
