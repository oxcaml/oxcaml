(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

(* Test the completeness holes of type inference with [eval]. *)

(* [eval] stub *)
open (struct
  let eval x = x |> Obj.magic_many |> Obj.magic
end : sig
  val eval : 'a expr @ once -> 'a eval
end)
[%%expect {|
val eval : 'a expr @ once -> 'a eval = <fun>
|}]

(** Unification **)

(* Type constructors *)
let f x : int = eval x
[%%expect {|
Line 4, characters 16-22:
4 | let f x : int = eval x
                    ^^^^^^
Error: This expression has type "'a eval"
       but an expression was expected of type "int"
|}]

let f x : int list = eval x
[%%expect {|
Line 1, characters 21-27:
1 | let f x : int list = eval x
                         ^^^^^^
Error: This expression has type "'a eval"
       but an expression was expected of type "int list"
|}]

let f x : 'a list = eval x
[%%expect {|
Line 1, characters 20-26:
1 | let f x : 'a list = eval x
                        ^^^^^^
Error: This expression has type "'b eval"
       but an expression was expected of type "'a list"
|}]

let f (type t) x : t = eval x
[%%expect {|
Line 1, characters 23-29:
1 | let f (type t) x : t = eval x
                           ^^^^^^
Error: This expression has type "'a eval"
       but an expression was expected of type "t"
|}]

(* Arrows *)
let f x : string -> int = eval x
[%%expect {|
Line 1, characters 26-32:
1 | let f x : string -> int = eval x
                              ^^^^^^
Error: This expression has type "'a eval"
       but an expression was expected of type "string -> int"
|}]
let f x : 'a -> 'b = eval x
[%%expect {|
Line 1, characters 21-27:
1 | let f x : 'a -> 'b = eval x
                         ^^^^^^
Error: This expression has type "'c eval"
       but an expression was expected of type "'a -> 'b"
|}]

(* Tuples *)
let f x : int * string = eval x
[%%expect {|
Line 1, characters 25-31:
1 | let f x : int * string = eval x
                             ^^^^^^
Error: This expression has type "'a eval"
       but an expression was expected of type "int * string"
|}]
let f x : 'a * 'b = eval x
[%%expect {|
Line 1, characters 20-26:
1 | let f x : 'a * 'b = eval x
                        ^^^^^^
Error: This expression has type "'c eval"
       but an expression was expected of type "'a * 'b"
|}]

(* Unboxed tuples *)
let f x : #(int * float#) = eval x
[%%expect {|
Line 1, characters 28-34:
1 | let f x : #(int * float#) = eval x
                                ^^^^^^
Error: This expression has type "'a eval"
       but an expression was expected of type "#(int * float#)"
|}]
let f x : #('a * 'b) = eval x
[%%expect {|
Line 1, characters 23-29:
1 | let f x : #('a * 'b) = eval x
                           ^^^^^^
Error: This expression has type "'c eval"
       but an expression was expected of type "#('a * 'b)"
|}]

(* Objects *)
let f x : <a: int; b: string> = eval x
[%%expect {|
Line 1, characters 32-38:
1 | let f x : <a: int; b: string> = eval x
                                    ^^^^^^
Error: This expression has type "'a eval"
       but an expression was expected of type "< a : int; b : string >"
|}]
let f x : <a: int; b: string; ..> = eval x
[%%expect {|
Line 1, characters 36-42:
1 | let f x : <a: int; b: string; ..> = eval x
                                        ^^^^^^
Error: This expression has type "'a eval"
       but an expression was expected of type "< a : int; b : string; .. >"
|}]

(* Polymorphic variants *)
(* closed variant *)
let f x : [ `A of int | `B of string | `C ] = eval x
[%%expect {|
Line 1, characters 46-52:
1 | let f x : [ `A of int | `B of string | `C ] = eval x
                                                  ^^^^^^
Error: This expression has type "'a eval"
       but an expression was expected of type
         "[ `A of int | `B of string | `C ]"
|}]
let f x : [> `A of int | `B of string | `C ] = eval x
[%%expect {|
Line 1, characters 47-53:
1 | let f x : [> `A of int | `B of string | `C ] = eval x
                                                   ^^^^^^
Error: This expression has type "'a eval"
       but an expression was expected of type
         "[> `A of int | `B of string | `C ]"
|}]
let f x : [< `A of int | `B of string | `C ] = eval x
[%%expect {|
Line 1, characters 47-53:
1 | let f x : [< `A of int | `B of string | `C ] = eval x
                                                   ^^^^^^
Error: This expression has type "'a eval"
       but an expression was expected of type
         "[< `A of int | `B of string | `C ]"
|}]

(* Quantifiers *)
let f x : ('a. 'a -> 'a) -> int = eval x
[%%expect {|
Line 1, characters 34-40:
1 | let f x : ('a. 'a -> 'a) -> int = eval x
                                      ^^^^^^
Error: This expression has type "'b eval"
       but an expression was expected of type "('a. 'a -> 'a) -> int"
|}]

(* Package types *)
let f x : (module Map.OrderedType with type t = int) -> unit = eval x
[%%expect {|
Line 1, characters 63-69:
1 | let f x : (module Map.OrderedType with type t = int) -> unit = eval x
                                                                   ^^^^^^
Error: This expression has type "'a eval"
       but an expression was expected of type
         "(module Map.OrderedType with type t = int) -> unit"
|}]

(** Subsumption **)

module F (M : sig
  val x : 'a eval
end ) : sig
  val x : int
end = M
[%%expect {|
Line 7, characters 6-7:
7 | end = M
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val x : 'a eval end
       is not included in
         sig val x : int end
       Values do not match: val x : 'a eval is not included in val x : int
       The type "'a eval" is not compatible with the type "int"
|}]

module F (M : sig
  val x : 'a eval
end ) : sig
  val x : 'b
end = M
[%%expect {|
Line 5, characters 6-7:
5 | end = M
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val x : 'a eval end
       is not included in
         sig val x : 'b end
       Values do not match: val x : 'a eval is not included in val x : 'b
       The type "'a eval" is not compatible with the type "'b"
|}]

module F (M : sig
  val x : 'a eval
end ) : sig
  val x : 'b list
end = M
[%%expect {|
Line 5, characters 6-7:
5 | end = M
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val x : 'a eval end
       is not included in
         sig val x : 'b list end
       Values do not match:
         val x : 'a eval
       is not included in
         val x : 'b list
       The type "'a eval" is not compatible with the type "'b list"
|}]

(* should pass: [eval] is surjective *)
module F (M : sig
  val x : 'a eval
end ) : sig
  val x : 'b
end = M
[%%expect {|
Line 5, characters 6-7:
5 | end = M
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val x : 'a eval end
       is not included in
         sig val x : 'b end
       Values do not match: val x : 'a eval is not included in val x : 'b
       The type "'a eval" is not compatible with the type "'b"
|}]

(* should fail *)
module F (M : sig
  val x : int
end ) : sig
  val x : 'a eval
end = M
[%%expect {|
Line 5, characters 6-7:
5 | end = M
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val x : int end
       is not included in
         sig val x : 'a eval end
       Values do not match: val x : int is not included in val x : 'a eval
       The type "int" is not compatible with the type "'a eval"
|}]
