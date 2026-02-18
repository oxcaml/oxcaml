(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

(* Test that eval correctly maps concrete types across stages. *)

(** Basics **)

let f (x : <[int]> eval) : int = x
[%%expect {|
val f : int -> int = <fun>
|}]

let f (x : <[string]> eval) : string = x
[%%expect {|
val f : string -> string = <fun>
|}]

let f (x : <[int]> eval) : string = x
[%%expect {|
Line 1, characters 36-37:
1 | let f (x : <[int]> eval) : string = x
                                        ^
Error: This expression has type "int" but an expression was expected of type
         "string"
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
val f : <[int]> expr -> int = <fun>
|}]

let f (x : <[string]> expr) : string = eval x
[%%expect {|
val f : <[string]> expr -> string = <fun>
|}]

let f (x : <[int]> expr) : string = eval x (* type error: int ~/~ string *)
[%%expect {|
Line 1, characters 36-42:
1 | let f (x : <[int]> expr) : string = eval x (* type error: int ~/~ string *)
                                        ^^^^^^
Error: This expression has type "int" but an expression was expected of type
         "string"
|}]


(** Type formers **)

(* Type constructors *)
let f (x : <[$('a) list]> expr) : 'a eval list = eval x
[%%expect {|
val f : <[$('a) list]> expr -> 'a eval list = <fun>
|}]

let f (x : <[($('a), $('b)) Either.t]> expr)
    : ('a eval, 'b eval) Either.t = eval x
[%%expect {|
val f : <[($('a), $('b)) Either.t]> expr -> ('a eval, 'b eval) Either.t =
  <fun>
|}]

(* Tuples *)
let f (x : <[$('a) * $('b) * $('c)]> expr)
    : 'a eval * 'b eval * 'c eval = eval x
[%%expect {|
val f : <[$('a) * $('b) * $('c)]> expr -> 'a eval * 'b eval * 'c eval = <fun>
|}]

(* Unboxed tuples *)
let f (x : <[#($('a) * $('b) * $('c))]> expr)
    : #('a eval * 'b eval * 'c eval) = eval x
[%%expect {|
val f : <[#($('a) * $('b) * $('c))]> expr -> #('a eval * 'b eval * 'c eval) =
  <fun>
|}]

(* Objects *)
let f (x : <[ <a: $('a); b: $('b)> ]> expr)
    : <a: 'a eval; b: 'b eval> = eval x
[%%expect {|
val f :
  ('a : any) ('b : any).
    <[< a : $('a); b : $('b) >]> expr -> < a : 'a eval; b : 'b eval > =
  <fun>
|}]
(* Objects with polymorphic methods *)
let f (x : <[ <a: 'c. 'c -> $('a); b: 'd. 'd -> $('b)> ]> expr)
    : <a: 'c. 'c eval -> 'a eval; b: 'd. 'd eval -> 'b eval> = eval x
[%%expect {|
val f :
  ('a : any) ('b : any).
    <[< a : 'c. 'c -> $('a); b : 'd. 'd -> $('b) >]> expr ->
    < a : 'c. 'c eval -> 'a eval; b : 'd. 'd eval -> 'b eval > =
  <fun>
|}]

(* Quotes *)
(* indirection with [expr] is fine, as it is injective *)
let f (x : <[ <[int]> ]> eval expr) : <[int]> expr = x
[%%expect {|
val f : <[int]> expr -> <[int]> expr = <fun>
|}]
let f (x : <[ <[ <[int]> ]> ]> eval eval expr) : <[int]> expr = x
[%%expect {|
val f : <[int]> expr -> <[int]> expr = <fun>
|}]
let f (x : <[ <[ <[ <[int]> ]> ]> ]> eval eval eval expr) : <[int]> expr = x
[%%expect {|
val f : <[int]> expr -> <[int]> expr = <fun>
|}]
let f (x : <[ <[ <[int]> ]> ]> eval eval eval) : int = x
[%%expect {|
val f : int -> int = <fun>
|}]

(* Splices *)
let f (x : <[ $('a) ]> expr) : 'a eval = eval x (* obviously *)
[%%expect {|
val f : 'a expr -> 'a eval = <fun>
|}]
let f (x : <[ $('a) eval ]>) : 'a eval = x
[%%expect {|
val f : 'a eval -> 'a eval = <fun>
|}]

(* Evals *)
(* also tests that we cancel the quotes & splices *)
let f (x : <[ $('a) eval ]> expr) : 'a eval eval = eval x
[%%expect {|
Line 1:
Error: Values do not match:
         val f : 'a eval expr -> 'a eval eval
       is not included in
         val f : 'a eval expr -> 'a eval eval
       The type "'a eval expr -> 'a eval eval" is not compatible with the type
         "'b eval expr -> 'b eval eval"
       Type "'a eval eval" is not compatible with type "'b eval eval"
       The constraints "<[<[<[<['a]>]>]>]> eval eval eval eval =
       <[<['b]>]> eval eval" and "<[<[<['a]>]>]> eval eval eval = <['b]> eval"
       have to be compatible.
|}]
let f (x : <[ $('a) eval ]> eval) : 'a eval eval = x
[%%expect {|
val f : 'a eval eval -> 'a eval eval = <fun>
|}]
let f (x : <[ <[ $($('a)) eval ]> eval ]> eval) : 'a eval eval eval = x
[%%expect {|
val f : 'a eval eval eval -> 'a eval eval eval = <fun>
|}]
let f (x : <[ <[ <[ $($($('a))) eval ]> eval ]> eval ]> eval)
    : 'a eval eval eval eval = x
[%%expect {|
val f : 'a eval eval eval eval -> 'a eval eval eval eval = <fun>
|}]
let f (x : <[ <[ <[ $($($('a))) expr ]> expr ]> eval ]> eval)
    : <[ $('a) eval eval expr ]> expr = x
[%%expect {|
val f :
  ('a : any). <[$('a) eval eval expr]> expr -> <[$('a) eval eval expr]> expr =
  <fun>
|}]

(* Polymorphic variants *)
let f (x : <[[ `A of $('a) | `B of $('b) | `C ]]> expr)
    : [ `A of 'a eval | `B of 'b eval | `C ] = eval x
[%%expect {|
val f :
  <[[ `A of $('a) | `B of $('b) | `C ]]> expr ->
  [ `A of 'a eval | `B of 'b eval | `C ] = <fun>
|}]
let f (x : <[[> `A of $('a) | `B of $('b) | `C | `D ]]> expr)
    : [> `A of 'a eval | `B of 'b eval | `C ] = eval x
[%%expect {|
val f :
  <[[> `A of $('_a) | `B of $('_b) | `C | `D ] as '_weak1]> expr -> '_weak1 =
  <fun>
|}]
let f (x : <[[< `A of $('a) | `B of $('b1) & $('b2) | `C ]]> expr)
    : [< `A of 'a eval | `B of 'b1 eval & 'b2 eval | `C | `D ] = eval x
[%%expect {|
val f :
  <[[< `A of $('_a) | `B of $('_b1) & $('_b2) | `C ] as '_weak2]> expr ->
  '_weak2 = <fun>
|}]

(* Quantifiers *)
(* partially re-staged *)
let f (x : <[('a. 'a -> 'a) -> int]> expr)
    : ('a. <[ $('a) -> $('a) ]> eval) -> int = eval x
[%%expect {|
val f : <[('a. 'a -> 'a) -> int]> expr -> ('a. 'a eval -> 'a eval) -> int =
  <fun>
|}]
(* fully re-staged *)
let f (x : <[('a. 'a -> 'a) -> int]> expr)
    : ('a. 'a eval -> 'a eval) -> int = eval x
[%%expect {|
val f : <[('a. 'a -> 'a) -> int]> expr -> ('a. 'a eval -> 'a eval) -> int =
  <fun>
|}]
(* variables from different quantifiers *)
let f (x : <[('a. 'a -> $('b) -> 'a) -> $('b)]> expr)
    : ('a. <[ $('a) -> $('b) -> $('a) ]> eval) -> 'b eval = eval x
[%%expect {|
val f :
  ('b : any).
    <[('a. 'a -> $('b) -> 'a) -> $('b)]> expr ->
    ('a. 'a eval -> 'b eval -> 'a eval) -> 'b eval =
  <fun>
|}]
(* nested quantifiers *)
let f (x : <[('a. ('b. 'a -> 'b) -> 'a) -> unit]> expr)
    : ('a. ('b. 'a eval -> 'b eval) -> 'a eval) -> unit = eval x
[%%expect {|
val f :
  <[('a. ('b. 'a -> 'b) -> 'a) -> unit]> expr ->
  ('a. ('b. 'a eval -> 'b eval) -> 'a eval) -> unit = <fun>
|}]
let f (x : <[('a. ('b. ('c. 'a -> 'b -> 'c) -> 'a -> 'b) -> 'a) -> unit]> expr)
    : ('a. ('b. ('c. 'a eval -> 'b eval -> 'c eval) -> 'a eval -> 'b eval)
            -> 'a eval) -> unit = eval x
[%%expect {|
val f :
  <[('a. ('b. ('c. 'a -> 'b -> 'c) -> 'a -> 'b) -> 'a) -> unit]> expr ->
  ('a.
     ('b. ('c. 'a eval -> 'b eval -> 'c eval) -> 'a eval -> 'b eval) ->
     'a eval) ->
  unit = <fun>
|}]

(* Package types *)
let f (x : <[(module Map.OrderedType with type t = $('a)) -> unit]> expr)
    : (module Map.OrderedType with type t = 'a eval) -> unit = eval x
[%%expect {|
val f :
  <[(module Map.OrderedType with type t = $('a)) -> unit]> expr ->
  (module Map.OrderedType with type t = 'a eval) -> unit = <fun>
|}]

(** Recursion **)
(* CR-soon jbachurski: These types will distribute evals forever and for now
   this is expected behaviour (it tells us that rewrites are proceeding and
   keep yielding unifiable types). However, these should actually just unify
   by memoising eval rewrites (see ticket #6513). *)

let f (x : (<[ <m : $('a) > ]> as 'a) eval) : (< m : 'b > as 'b) = x
[%%expect {|
Uncaught exception: Stack overflow

|}]

let f (x : (<[ <m : $('a) list > ]> as 'a) eval) : (< m : 'b list > as 'b) = x
[%%expect {|
Uncaught exception: Stack overflow

|}]
