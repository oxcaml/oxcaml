(* TEST
 expect;
*)

type dense =
  | A [@tag 2]
  | B
  | C [@tag 1]

let int_of_dense (x : dense) : int = Obj.magic x

let observed = List.map int_of_dense [ A; B; C ]

let matched = List.map (function A -> "A" | B -> "B" | C -> "C") [ A; B; C ]

[%%expect {|
type dense = A [@tag 2] | B | C [@tag 1]
val int_of_dense : dense -> int = <fun>
val observed : int list = [2; 0; 1]
val matched : string list = ["A"; "B"; "C"]
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

module Mismatching_annotation : sig
  type t =
    | A
    | B
end = struct
  type t =
    | A [@tag 1]
    | B
end

[%%expect {|
Lines 5-9, characters 6-3:
5 | ......struct
6 |   type t =
7 |     | A [@tag 1]
8 |     | B
9 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A [@tag 1] | B end
       is not included in
         sig type t = A | B end
       Type declarations do not match:
         type t = A [@tag 1] | B
       is not included in
         type t = A | B
       Their internal representations differ:
       constructor "A" has tag 1 in the first declaration, but tag 0 in the second declaration.
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

type signed_sparse =
  | Neg [@tag (-1)]
  | Zero
  | High [@tag 4]

let int_of_signed_sparse (x : signed_sparse) : int = Obj.magic x

let signed_observed =
  List.map int_of_signed_sparse [ Neg; Zero; High ]

let signed_matched =
  List.map
    (function Neg -> "Neg" | Zero -> "Zero" | High -> "High")
    [ Neg; Zero; High ]

[%%expect {|
type signed_sparse = Neg [@tag -1] | Zero | High [@tag 4]
val int_of_signed_sparse : signed_sparse -> int = <fun>
val signed_observed : int list = [-1; 0; 4]
val signed_matched : string list = ["Neg"; "Zero"; "High"]
|}]

type printed_negative = PN [@tag -1]

[%%expect {|
type printed_negative = PN [@tag -1]
|}]

type boundary =
  | Min [@tag (-4611686018427387904)]
  | Max [@tag 4611686018427387903]

let int_of_boundary (x : boundary) : int = Obj.magic x

let boundary_observed = List.map int_of_boundary [ Min; Max ]

[%%expect {|
type boundary =
    Min
  [@tag -4611686018427387904]
  | Max
  [@tag 4611686018427387903]
val int_of_boundary : boundary -> int = <fun>
val boundary_observed : int list =
  [-4611686018427387904; 4611686018427387903]
|}]

module Matching_signed_sparse_annotation : sig
  type t =
    | Neg [@tag (-1)]
    | Zero
    | High [@tag 4]
end = struct
  type t =
    | Neg [@tag -1]
    | Zero
    | High [@tag 4]
end

[%%expect {|
module Matching_signed_sparse_annotation :
  sig type t = Neg [@tag -1] | Zero | High [@tag 4] end
|}]

module Mismatching_signed_sparse_annotation : sig
  type t =
    | Neg [@tag (-1)]
    | Zero
    | High [@tag 4]
end = struct
  type t =
    | Neg [@tag (-1)]
    | Zero [@tag 4]
    | High
end

[%%expect {|
Lines 6-11, characters 6-3:
 6 | ......struct
 7 |   type t =
 8 |     | Neg [@tag (-1)]
 9 |     | Zero [@tag 4]
10 |     | High
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = Neg [@tag -1] | Zero [@tag 4] | High end
       is not included in
         sig type t = Neg [@tag -1] | Zero | High [@tag 4] end
       Type declarations do not match:
         type t = Neg [@tag -1] | Zero [@tag 4] | High
       is not included in
         type t = Neg [@tag -1] | Zero | High [@tag 4]
       Their internal representations differ:
       constructor "Zero" has tag 4 in the first declaration, but tag 0 in the second declaration.
|}]

type duplicate_negative =
  | DN0 [@tag (-1)]
  | DN1 [@tag (-1)]

[%%expect {|
Line 3, characters 8-19:
3 |   | DN1 [@tag (-1)]
            ^^^^^^^^^^^
Error: Two constructors cannot use the same [@tag] value -1
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
