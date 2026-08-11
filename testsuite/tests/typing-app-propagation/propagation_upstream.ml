(* TEST
 flags = "-extension-universe upstream_compatible";
 expect;
*)

(* Expected-type propagation into applications is a non-erasable typing
   change, so it is disabled when only erasable extensions are allowed:
   these programs must be rejected exactly as upstream rejects them. *)

type t = A | B
type s = A | C

let id x = x

let _ = (id A : t), (id A : s)
[%%expect{|
type t = A | B
type s = A | C
val id : 'a -> 'a = <fun>
Line 6, characters 9-13:
6 | let _ = (id A : t), (id A : s)
             ^^^^
Error: This expression has type "s" but an expression was expected of type "t"
|}]

type bar = Bar of int
type baz = Bar of string

let bars (xs : int list) : bar list = List.map (fun x -> Bar x) xs
[%%expect{|
type bar = Bar of int
type baz = Bar of string
Line 4, characters 64-66:
4 | let bars (xs : int list) : bar list = List.map (fun x -> Bar x) xs
                                                                    ^^
Error: The value "xs" has type "int list" but an expression was expected of type
         "string list"
       Type "int" is not compatible with type "string"
|}]

type t1 = {x: int}
type t2 = {x: bool}

let f =
  let k x _ = x in
  fun a b -> (k {x=a} {x=b} : t1)
[%%expect{|
type t1 = { x : int; }
type t2 = { x : bool; }
Line 6, characters 14-27:
6 |   fun a b -> (k {x=a} {x=b} : t1)
                  ^^^^^^^^^^^^^
Error: This expression has type "t2" but an expression was expected of type "t1"
|}]
