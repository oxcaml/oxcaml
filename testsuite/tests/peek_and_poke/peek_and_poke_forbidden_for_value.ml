(* TEST
   expect;
*)

type ('a : any) t

external read : ('a : any mod external_). 'a t -> 'a = "%peek"
  [@@layout_poly]

external write : ('a : any mod external_). 'a t -> 'a -> unit = "%poke"
  [@@layout_poly]

[%%expect {|
type ('a : any) t
external read : ('a : any mod external_). 'a t -> 'a = "%peek"
  [@@layout_poly]
external write : ('a : any mod external_). 'a t -> 'a -> unit = "%poke"
  [@@layout_poly]
|}]

let bad_read p : string = read p
[%%expect {|
Line 1, characters 26-32:
1 | let bad_read p : string = read p
                              ^^^^^^
Error: This expression has type "('a : value_or_null mod external_)"
       but an expression was expected of type "string"
       The kind of string is immutable_data
         because it is the primitive type string.
       But the kind of string must be a subkind of
           value_or_null mod external_
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}]

let bad_write p (s : string) = write p s
[%%expect {|
Line 1, characters 39-40:
1 | let bad_write p (s : string) = write p s
                                           ^
Error: This expression has type "string" but an expression was expected of type
         "('a : value_or_null mod external_)"
       The kind of string is immutable_data
         because it is the primitive type string.
       But the kind of string must be a subkind of
           value_or_null mod external_
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}]
