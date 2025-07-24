(* TEST
   expect;
*)

[@@@ocaml.warning "+4"]

module rec X : sig
  type t = int * bool
end = struct
  type t =
    | A
    | B

  let f = function A | B -> 0
end

[%%expect
{|
Lines 5-11, characters 6-3:
 5 | ......struct
 6 |   type t =
 7 |     | A
 8 |     | B
 9 |
10 |   let f = function A | B -> 0
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = X.t = A | B val f : t -> int end
       is not included in
         sig type t = int * bool end
       Type declarations do not match:
         type t = X.t = A | B
       is not included in
         type t = int * bool
       The type "X.t" is not equal to the type "int * bool"
|}]
