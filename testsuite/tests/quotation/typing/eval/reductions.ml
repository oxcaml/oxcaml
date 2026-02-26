(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

(* Test that eval correctly maps concrete types across stages. *)



(** Basics **)

let f (x : <[int]> eval) : int = x
[%%expect {|
val f : <[int]> eval -> int = <fun>
|}]

let f (x : <[string]> eval) : string = x
[%%expect {|
val f : <[string]> eval -> string = <fun>
|}]

let f (x : <[int]> eval) : string = x
[%%expect {|
Line 1, characters 36-37:
1 | let f (x : <[int]> eval) : string = x
                                        ^
Error: This expression has type "<[int]> eval" = "int"
       but an expression was expected of type "string"
|}]

(* Quotes & splices reduce underneath as normal *)
let f (x : <[$('a)]> eval) : 'a eval = x
[%%expect {|
val f : 'a eval -> 'a eval = <fun>
|}]
let f (x : <[$(<[$('a)]>)]> eval) : 'a eval = x
[%%expect {|
val f : 'a eval -> 'a eval = <fun>
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
Error: This expression has type "<[int]> eval" = "int"
       but an expression was expected of type "string"
|}]

(* Printing *)
let f (x : <['a]> expr) = eval x
[%%expect {|
val f : ('a : any). 'a expr -> 'a eval = <fun>
|}]
let f (x : <[int]> expr) = eval x
[%%expect {|
val f : <[int]> expr -> <[int]> eval = <fun>
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

(* Arrows *)
let f (x : <[$('a) -> $('b)]> expr) : 'a eval -> 'b eval = eval x
[%%expect {|
val f : <[$('a) -> $('b)]> expr -> 'a eval -> 'b eval = <fun>
|}]
let f (x : <[l:$('a) -> $('b)]> expr) : l:('a eval) -> 'b eval = eval x
[%%expect {|
val f : <[l:$('a) -> $('b)]> expr -> l:'a eval -> 'b eval = <fun>
|}]
let f (x : <[?l:$('a) -> $('b)]> expr) : ?l:('a eval) -> 'b eval = eval x
[%%expect {|
val f : <[?l:$('a) -> $('b)]> expr -> ?l:'a eval -> 'b eval = <fun>
|}]
let f (x : <[$('a) @ local -> $('b) @ local]> expr)
    : 'a eval @ local -> 'b eval @ local = eval x
[%%expect {|
val f :
  <[$('a) @ local -> $('b) @ local]> expr ->
  'a eval @ local -> 'b eval @ local = <fun>
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
val f :
  <[('a. 'a -> 'a) -> int]> expr -> ('a. <[$('a) -> $('a)]> eval) -> int =
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
    ('a. <[$('a) -> $('b) -> $('a)]> eval) -> 'b eval =
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
(* [instance_poly] has special cases for polymorphic variants and objects,
   so we test them separately. *)
(* polymorphic variant *)
let f (x : <[('a. 'a -> [`Foo of 'a | `Bar of 'a]) -> unit]> expr)
    : ('a. 'a eval -> [`Foo of 'a eval | `Bar of 'a eval]) -> unit = eval x
[%%expect {|
val f :
  <[('a. 'a -> [ `Bar of 'a | `Foo of 'a ]) -> unit]> expr ->
  ('a. 'a eval -> [ `Bar of 'a eval | `Foo of 'a eval ]) -> unit = <fun>
|}]
(* object *)
let f (x : <[('a. 'a -> <i : 'a; j : 'a>) -> unit]> expr)
    : ('a. 'a eval -> <i : 'a eval; j : 'a eval>) -> unit = eval x
[%%expect {|
val f :
  <[('a. 'a -> < i : 'a; j : 'a >) -> unit]> expr ->
  ('a. 'a eval -> < i : 'a eval; j : 'a eval >) -> unit = <fun>
|}]

(* Package types *)
let f (x : <[(module Map.OrderedType with type t = $('a)) -> unit]> expr)
    : (module Map.OrderedType with type t = 'a eval) -> unit = eval x
[%%expect {|
val f :
  <[(module Map.OrderedType with type t = $('a)) -> unit]> expr ->
  (module Map.OrderedType with type t = 'a eval) -> unit = <fun>
|}]


(** Composing evals **)

(* Quoted [eval] stubs *)
open (struct
  let eval0 = Obj.magic ()
  let eval1 = <[Obj.magic ()]>
  let eval2 = <[<[Obj.magic ()]>]>
end : sig
  val eval0 : 'a expr -> 'a eval
  val eval1 : <[ $('a) expr -> $('a) eval ]> expr
  val eval2 : <[ <[ $($('a)) expr -> $($('a)) eval ]> expr ]> expr
end)
[%%expect {|
val eval0 : 'a expr -> 'a eval = <fun>
val eval1 : <[$('a) expr -> $('a) eval]> expr = <[Stdlib.Obj.magic ()]>
val eval2 : <[<[$($('a)) expr -> $($('a)) eval]> expr]> expr =
  <[<[Stdlib.Obj.magic ()]>]>
|}]

(* The obvious form of eval composition -- [fun x -> eval (eval x)] --
   has an irreducible type under our restriction to next-stage types.
   We make sure the alternative -- [fun x -> eval (eval $x)] -- behaves. *)

let f (x : 'a expr) : 'a eval =
  eval0 x
[%%expect {|
val f : 'a expr -> 'a eval = <fun>
|}]
let f (x : <[ $('a) expr ]> expr) : <[ $('a) eval ]> eval =
  eval0 <[ $eval1 $x ]>
[%%expect {|
val f : <[$('a) expr]> expr -> <[$('a) eval]> eval = <fun>
|}]
let f (x : <[ <[ $($('a)) expr ]> expr ]> expr)
         : <[ <[ $($('a)) eval ]> eval ]> eval =
  eval0 <[$eval1 <[$($eval2) $($x)]>]>
[%%expect {|
val f : <[<[$($('a)) expr]> expr]> expr -> <[<[$($('a)) eval]> eval]> eval =
  <fun>
|}]
(* Concrete types reduce in nested evals *)
let f (x : <[int]> expr) : <[int]> eval =
  eval0 x
[%%expect {|
val f : <[int]> expr -> <[int]> eval = <fun>
|}]
let f (x : <[ <[int]> expr ]> expr) : int =
  eval0 <[$eval1 $x]>
[%%expect {|
val f : <[<[int]> expr]> expr -> int = <fun>
|}]
let f (x : <[<[<[int]> expr]> expr]> expr) : int =
  eval0 <[$eval1 <[ $($eval2) $($x) ]>]>
[%%expect {|
val f : <[<[<[int]> expr]> expr]> expr -> int = <fun>
|}]


(** Restrictions **)

(* Coherence -- <[$t eval]> = t eval -- does not hold *)
let f (x : <[ $('a) eval ]> expr) : 'a eval expr = x
[%%expect {|
Line 4, characters 51-52:
4 | let f (x : <[ $('a) eval ]> expr) : 'a eval expr = x
                                                       ^
Error: This expression has type "<[$('a) eval]> expr"
       but an expression was expected of type "'a eval expr"
       Type "<[$('a) eval]>" is not compatible with type "'a eval"
|}]

(* Post-next-stage-kinded types are irreducible *)
let f (x : <[ <[int]> ]> eval expr) : <[int]> expr = x
[%%expect {|
Line 1, characters 53-54:
1 | let f (x : <[ <[int]> ]> eval expr) : <[int]> expr = x
                                                         ^
Error: This expression has type "<[<[int]>]> eval expr"
       but an expression was expected of type "<[int]> expr"
       Type "<[<[int]>]> eval" is not compatible with type "<[int]>"
|}]
let f (x : <[<[int]>]> eval eval) : int = x
[%%expect {|
Line 1, characters 42-43:
1 | let f (x : <[<[int]>]> eval eval) : int = x
                                              ^
Error: This expression has type "<[<[int]>]> eval eval"
       but an expression was expected of type "int"
|}]
let f (x : <[ <[ <[int]> ]> ]> eval eval eval) : int = x
[%%expect {|
Line 1, characters 55-56:
1 | let f (x : <[ <[ <[int]> ]> ]> eval eval eval) : int = x
                                                           ^
Error: This expression has type "<[<[<[int]>]>]> eval eval eval"
       but an expression was expected of type "int"
|}]
let f (x : <[ <[ <[int]> ]> ]> eval eval expr) : <[int]> expr = x
[%%expect {|
Line 1, characters 64-65:
1 | let f (x : <[ <[ <[int]> ]> ]> eval eval expr) : <[int]> expr = x
                                                                    ^
Error: This expression has type "<[<[<[int]>]>]> eval eval expr"
       but an expression was expected of type "<[int]> expr"
       Type "<[<[<[int]>]>]> eval eval" is not compatible with type "<[int]>"
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
