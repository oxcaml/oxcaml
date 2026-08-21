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

let f x = eval x
[%%expect {|
val f : 'a expr -> 'a eval = <fun>
|}]

let f (x : <[int]> expr) = eval x
[%%expect {|
val f : <[int]> expr -> int = <fun>
|}]

let f x : int = eval 0
[%%expect {|
Line 1, characters 21-22:
1 | let f x : int = eval 0
                         ^
Error: The constant "0" has type "int" but an expression was expected of type
         "'a expr"
|}]

(** Unification **)

(* Type constructors *)
let f x : int = eval x
[%%expect {|
val f : <[int]> expr -> int = <fun>
|}]

let f x : int list = eval x
[%%expect {|
val f : <[int list]> expr -> int list = <fun>
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
val f : <[string -> int]> expr -> string -> int = <fun>
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
val f : <[int * string]> expr -> int * string = <fun>
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
let f x : #(int * float#) = (x : _ eval)
[%%expect {|
val f : #(int * float#) -> #(int * float#) = <fun>
|}]
let f x : #('a * 'b) = (x : _ eval)
[%%expect {|
Line 1, characters 23-35:
1 | let f x : #('a * 'b) = (x : _ eval)
                           ^^^^^^^^^^^^
Error: This expression has type "'c eval"
       but an expression was expected of type "#('a * 'b)"
|}]

(* Objects *)
let f x : <a: int; b: string> = eval x
[%%expect {|
val f : <[< a : int; b : string >]> expr -> < a : int; b : string > = <fun>
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
let f x : [ `A of int | `B of string | `C ] = eval x
[%%expect {|
val f :
  <[[ `A of int | `B of string | `C ]]> expr ->
  [ `A of int | `B of string | `C ] = <fun>
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
val f :
  <[(module Map.OrderedType with type t = int) -> unit]> expr ->
  (module Map.OrderedType with type t = int) -> unit = <fun>
|}]

(** Subsumption **)

module F (M : sig
  val x : 'a eval
end ) : sig
  val x : int
end = M
[%%expect {|
module F : functor (M : sig val x : 'a eval end) -> sig val x : int end
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

(** Inject **)

(* [inject] stub *)
open (struct
  let inject x = x |> Obj.magic
end : sig
  val inject : 'a eval -> 'a expr
end)
[%%expect {|
val inject : 'a eval -> 'a expr = <fun>
|}]

let f x = inject x
[%%expect {|
val f : 'a eval -> 'a expr = <fun>
|}]

let f (x : int) = (x : _ eval)
[%%expect {|
val f : int -> int = <fun>
|}]

let f x : <[int]> expr = inject 0
[%%expect {|
val f : 'a -> <[int]> expr = <fun>
|}]
