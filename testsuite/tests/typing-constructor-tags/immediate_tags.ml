(* TEST
 expect;
*)

type dense =
  | A [@immediate 2]
  | B
  | C [@immediate 1]

let int_of_dense (x : dense) : int = Obj.magic x

let observed = List.map int_of_dense [ A; B; C ]

let matched = List.map (function A -> "A" | B -> "B" | C -> "C") [ A; B; C ]

[%%expect {|
type dense = A [@immediate 2] | B | C [@immediate 1]
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
    | A [@immediate 0]
    | B [@immediate 1]
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
    | A [@immediate 1]
    | B
end

[%%expect {|
Lines 5-9, characters 6-3:
5 | ......struct
6 |   type t =
7 |     | A [@immediate 1]
8 |     | B
9 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A [@immediate 1] | B end
       is not included in
         sig type t = A | B end
       Type declarations do not match:
         type t = A [@immediate 1] | B
       is not included in
         type t = A | B
       Their internal representations differ:
       constructor "A" has immediate tag 1 in the first declaration, but immediate tag 0 in the second declaration.
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

type signed_sparse =
  | Neg [@immediate (-1)]
  | Zero
  | High [@immediate 4]

let int_of_signed_sparse (x : signed_sparse) : int = Obj.magic x

let signed_observed =
  List.map int_of_signed_sparse [ Neg; Zero; High ]

let signed_matched =
  List.map
    (function Neg -> "Neg" | Zero -> "Zero" | High -> "High")
    [ Neg; Zero; High ]

[%%expect {|
type signed_sparse = Neg [@immediate -1] | Zero | High [@immediate 4]
val int_of_signed_sparse : signed_sparse -> int = <fun>
val signed_observed : int list = [-1; 0; 4]
val signed_matched : string list = ["Neg"; "Zero"; "High"]
|}]

type printed_negative = PN [@immediate -1]

[%%expect {|
type printed_negative = PN [@immediate -1]
|}]

type boundary =
  | Min [@immediate (-4611686018427387904)]
  | Max [@immediate 4611686018427387903]

let int_of_boundary (x : boundary) : int = Obj.magic x

let boundary_observed = List.map int_of_boundary [ Min; Max ]

[%%expect {|
type boundary =
    Min
  [@immediate -4611686018427387904]
  | Max
  [@immediate 4611686018427387903]
val int_of_boundary : boundary -> int = <fun>
val boundary_observed : int list =
  [-4611686018427387904; 4611686018427387903]
|}]

module Matching_signed_sparse_annotation : sig
  type t =
    | Neg [@immediate (-1)]
    | Zero
    | High [@immediate 4]
end = struct
  type t =
    | Neg [@immediate -1]
    | Zero
    | High [@immediate 4]
end

[%%expect {|
module Matching_signed_sparse_annotation :
  sig type t = Neg [@immediate -1] | Zero | High [@immediate 4] end
|}]

module Mismatching_signed_sparse_annotation : sig
  type t =
    | Neg [@immediate (-1)]
    | Zero
    | High [@immediate 4]
end = struct
  type t =
    | Neg [@immediate (-1)]
    | Zero [@immediate 4]
    | High
end

[%%expect {|
Lines 6-11, characters 6-3:
 6 | ......struct
 7 |   type t =
 8 |     | Neg [@immediate (-1)]
 9 |     | Zero [@immediate 4]
10 |     | High
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = Neg [@immediate -1] | Zero [@immediate 4] | High end
       is not included in
         sig type t = Neg [@immediate -1] | Zero | High [@immediate 4] end
       Type declarations do not match:
         type t = Neg [@immediate -1] | Zero [@immediate 4] | High
       is not included in
         type t = Neg [@immediate -1] | Zero | High [@immediate 4]
       Their internal representations differ:
       constructor "Zero" has immediate tag 4 in the first declaration, but immediate tag 0 in the second declaration.
|}]

type duplicate_negative =
  | DN0 [@immediate (-1)]
  | DN1 [@immediate (-1)]

[%%expect {|
Line 3, characters 8-25:
3 |   | DN1 [@immediate (-1)]
            ^^^^^^^^^^^^^^^^^
Error: Two constructors cannot use the same [@immediate] tag -1
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
