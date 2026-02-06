(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

(* Test that eval correctly maps concrete types across stages. *)

(** Basics **)

let f (x : <[int]> eval) : int = x
[%%expect {|
Line 3, characters 33-34:
3 | let f (x : <[int]> eval) : int = x
                                     ^
Error: This expression has type "<[int]> eval"
       but an expression was expected of type "int"
|}]

let f (x : <[string]> eval) : string = x
[%%expect {|
Line 1, characters 39-40:
1 | let f (x : <[string]> eval) : string = x
                                           ^
Error: This expression has type "<[string]> eval"
       but an expression was expected of type "string"
|}]

let f (x : <[int]> eval) : string = x
[%%expect {|
Line 1, characters 36-37:
1 | let f (x : <[int]> eval) : string = x
                                        ^
Error: This expression has type "<[int]> eval"
       but an expression was expected of type "string"
|}]


(** Basics with expression-level eval **)

(* [eval] stub *)
open (struct
  let eval = Obj.magic
end : sig
  val eval : 'a expr -> 'a eval
end)
[%%expect {|
val eval : 'a expr -> 'a eval = <fun>
|}]

let f (x : <[int]> expr) : int = eval x
[%%expect {|
Line 1, characters 33-39:
1 | let f (x : <[int]> expr) : int = eval x
                                     ^^^^^^
Error: This expression has type "<[int]> eval"
       but an expression was expected of type "int"
|}]

let f (x : <[string]> expr) : string = eval x
[%%expect {|
Line 1, characters 39-45:
1 | let f (x : <[string]> expr) : string = eval x
                                           ^^^^^^
Error: This expression has type "<[string]> eval"
       but an expression was expected of type "string"
|}]

let f (x : <[int]> expr) : string = eval x (* type error: int ~/~ string *)
[%%expect {|
Line 1, characters 36-42:
1 | let f (x : <[int]> expr) : string = eval x (* type error: int ~/~ string *)
                                        ^^^^^^
Error: This expression has type "<[int]> eval"
       but an expression was expected of type "string"
|}]


(** Type formers **)

(* Type constructors *)
let f (x : <[$('a) list]> expr) : 'a eval list = eval x
[%%expect {|
Line 4, characters 49-55:
4 | let f (x : <[$('a) list]> expr) : 'a eval list = eval x
                                                     ^^^^^^
Error: This expression has type "<[$('a) list]> eval"
       but an expression was expected of type "'a eval list"
|}]

let f (x : <[($('a), $('b)) Either.t]> expr)
    : ('a eval, 'b eval) Either.t = eval x
[%%expect {|
Line 2, characters 36-42:
2 |     : ('a eval, 'b eval) Either.t = eval x
                                        ^^^^^^
Error: This expression has type "<[($('a), $('b)) Either.t]> eval"
       but an expression was expected of type "('a eval, 'b eval) Either.t"
|}]

(* Tuples *)
let f (x : <[$('a) * $('b) * $('c)]> expr)
    : 'a eval * 'b eval * 'c eval = eval x
[%%expect {|
Line 2, characters 36-42:
2 |     : 'a eval * 'b eval * 'c eval = eval x
                                        ^^^^^^
Error: This expression has type "<[$('a) * $('b) * $('c)]> eval"
       but an expression was expected of type "'a eval * 'b eval * 'c eval"
|}]

(* Unboxed tuples *)
let f (x : <[#($('a) * $('b) * $('c))]> expr)
    : #('a eval * 'b eval * 'c eval) = eval x
[%%expect {|
Line 2, characters 39-45:
2 |     : #('a eval * 'b eval * 'c eval) = eval x
                                           ^^^^^^
Error: This expression has type "<[#($('a) * $('b) * $('c))]> eval"
       but an expression was expected of type "#('a eval * 'b eval * 'c eval)"
|}]

(* Objects *)
let f (x : <[ <a: $('a); b: $('b)> ]> expr)
    : <a: 'a eval; b: 'b eval> = eval x
[%%expect {|
Line 2, characters 33-39:
2 |     : <a: 'a eval; b: 'b eval> = eval x
                                     ^^^^^^
Error: This expression has type "<[< a : 'a; b : 'b >]> eval"
       but an expression was expected of type
         "< a : <['a]> eval; b : <['b]> eval >"
|}]
(* Objects with polymorphic methods *)
let f (x : <[ <a: 'c. 'c -> $('a); b: 'd. 'd -> $('b)> ]> expr)
    : <a: 'c. 'c eval -> 'a eval; b: 'd. 'd eval -> 'b eval> = eval x
[%%expect {|
Line 2, characters 63-69:
2 |     : <a: 'c. 'c eval -> 'a eval; b: 'd. 'd eval -> 'b eval> = eval x
                                                                   ^^^^^^
Error: This expression has type
         "<[< a : 'c. 'c -> 'a; b : 'd. 'd -> 'b >]> eval"
       but an expression was expected of type
         "< a : 'c. 'c eval -> <['a]> eval; b : 'd. 'd eval -> <['b]> eval >"
|}]

(* Quotes *)
(* this indirection is fine, as [expr] is injective *)
let f (x : <[ <[int]> ]> eval expr) : <[int]> expr = x
[%%expect {|
Line 1, characters 53-54:
1 | let f (x : <[ <[int]> ]> eval expr) : <[int]> expr = x
                                                         ^
Error: This expression has type "<[<[int]>]> eval expr"
       but an expression was expected of type "<[int]> expr"
       Type "<[<[int]>]> eval" is not compatible with type "<[int]>"
|}]

(* Splices *)
let f (x : <[ $('a) ]> expr) : 'a eval = eval x (* obviously *)
[%%expect {|
val f : 'a expr -> 'a eval = <fun>
|}]

(* Evals *)
let f (x : <[ $('a) eval ]> expr) : 'a eval eval = eval x
[%%expect {|
Line 1, characters 51-57:
1 | let f (x : <[ $('a) eval ]> expr) : 'a eval eval = eval x
                                                       ^^^^^^
Error: This expression has type "<[$('a) eval]> eval"
       but an expression was expected of type "'a eval eval"
       Type "<[$('a) eval]>" is not compatible with type "'a eval"
|}]

(* Polymorphic variants *)
let f (x : <[[ `A of $('a) | `B of $('b) | `C ]]> expr)
    : [ `A of 'a eval | `B of 'b eval | `C ] = eval x
[%%expect {|
Line 2, characters 47-53:
2 |     : [ `A of 'a eval | `B of 'b eval | `C ] = eval x
                                                   ^^^^^^
Error: This expression has type "<[[ `A of $('a) | `B of $('b) | `C ]]> eval"
       but an expression was expected of type
         "[ `A of 'a eval | `B of 'b eval | `C ]"
|}]
let f (x : <[[> `A of $('a) | `B of $('b) | `C | `D ]]> expr)
    : [> `A of 'a eval | `B of 'b eval | `C ] = eval x
[%%expect {|
Line 2, characters 48-54:
2 |     : [> `A of 'a eval | `B of 'b eval | `C ] = eval x
                                                    ^^^^^^
Error: This expression has type
         "<[[> `A of $('a) | `B of $('b) | `C | `D ] as 'c]> eval" =
           "<['c]> eval"
       but an expression was expected of type
         "[> `A of 'a eval | `B of 'b eval | `C ]"
|}]
let f (x : <[[< `A of $('a) | `B of $('b1) & $('b2) | `C ]]> expr)
    : [< `A of 'a eval | `B of 'b1 eval & 'b2 eval | `C | `D ] = eval x
[%%expect {|
Line 2, characters 65-71:
2 |     : [< `A of 'a eval | `B of 'b1 eval & 'b2 eval | `C | `D ] = eval x
                                                                     ^^^^^^
Error: This expression has type
         "<[[< `A of $('a) | `B of $('b1) & $('b2) | `C ] as 'b]> eval" =
           "<['b]> eval"
       but an expression was expected of type
         "[< `A of 'a eval | `B of 'b1 eval & 'b2 eval | `C | `D ]"
|}]

(* Quantifiers *)
(* partially re-staged *)
let f (x : <[('a. unit -> 'a -> 'a) -> int]> expr)
    : ('a. unit -> <[ $('a) -> $('a) ]> eval) -> int = eval x
[%%expect {|
Line 2, characters 55-61:
2 |     : ('a. unit -> <[ $('a) -> $('a) ]> eval) -> int = eval x
                                                           ^^^^^^
Error: This expression has type "<[('a. unit -> 'a -> 'a) -> int]> eval"
       but an expression was expected of type
         "('a. unit -> <[$('a) -> $('a)]> eval) -> int"
|}]
(* fully re-staged *)
let f (x : <[('a. unit -> 'a -> 'a) -> int]> expr)
    : ('a. unit -> 'a eval -> 'a eval) -> int = eval x
[%%expect {|
Line 2, characters 48-54:
2 |     : ('a. unit -> 'a eval -> 'a eval) -> int = eval x
                                                    ^^^^^^
Error: This expression has type "<[('a. unit -> 'a -> 'a) -> int]> eval"
       but an expression was expected of type
         "('a. unit -> 'a eval -> 'a eval) -> int"
|}]
(* variables from different quantifiers *)
let f (x : <[('a. 'a -> $('b) -> 'a) -> $('b)]> expr)
    : ('a. <[ $('a) -> $('b) -> $('a) ]> eval) -> 'b eval = eval x
[%%expect {|
Line 2, characters 60-66:
2 |     : ('a. <[ $('a) -> $('b) -> $('a) ]> eval) -> 'b eval = eval x
                                                                ^^^^^^
Error: This expression has type "<[('a. 'a -> 'b -> 'a) -> 'b]> eval"
       but an expression was expected of type
         "('a. <[$('a) -> 'b -> $('a)]> eval) -> <['b]> eval"
|}]

(* Package types *)
let f (x : <[(module Map.OrderedType with type t = $('a)) -> unit]> expr)
    : (module Map.OrderedType with type t = 'a eval) -> unit = eval x
[%%expect {|
Line 2, characters 63-69:
2 |     : (module Map.OrderedType with type t = 'a eval) -> unit = eval x
                                                                   ^^^^^^
Error: This expression has type
         "<[(module Map.OrderedType with type t = $('a)) -> unit]> eval"
       but an expression was expected of type
         "(module Map.OrderedType with type t = 'a eval) -> unit"
|}]
