(* TEST
 flags = "-extension small_numbers";
 expect;
*)

type ('a : value_or_null) t : value_or_null mod aliased =
  { x : 'a @@ aliased; y : 'a @@ aliased }
[@@unboxed]

[%%expect{|
Lines 1-3, characters 0-11:
1 | type ('a : value_or_null) t : value_or_null mod aliased =
2 |   { x : 'a @@ aliased; y : 'a @@ aliased }
3 | [@@unboxed]
Error: This type cannot be unboxed because it has more than one field.
|}]
