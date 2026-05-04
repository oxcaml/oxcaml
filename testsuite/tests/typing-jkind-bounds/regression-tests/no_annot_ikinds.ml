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
Error: The kind of type "aliased_t" is value_or_null
         because of the annotation on 'a in the declaration of the type
                                      aliased_t.
       But the kind of type "aliased_t" must be a subkind of
           value_or_null mod aliased
         because of the annotation on the declaration of the type aliased_t.
|}]
