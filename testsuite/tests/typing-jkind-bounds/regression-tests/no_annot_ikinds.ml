(* TEST
 flags = "-extension small_numbers";
 expect;
*)

type ('a : value_or_null) aliased_t : value_or_null mod aliased =
  { aliased : 'a }
[@@unboxed]

[%%expect{|
Lines 1-3, characters 0-11:
1 | type ('a : value_or_null) aliased_t : value_or_null mod aliased =
2 |   { aliased : 'a }
3 | [@@unboxed]
Error: This type definition does not satisfy its kind annotation
         value_or_null mod aliased,
       because 'a is not mod aliased.
|}]
