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
val f : <[int]> expr -> int = <fun>
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
Line 2, characters 63-69:
2 |     : <a: 'c. 'c eval -> 'a eval; b: 'd. 'd eval -> 'b eval> = eval x
                                                                   ^^^^^^
Error: This expression has type
         "<[< a : 'c. 'c -> 'a; b : 'd. 'd -> 'b >]> eval" =
           "< a : 'c. 'c0 eval -> <['a]> eval;
             b : 'd. 'd0 eval -> <['b]> eval >"
       but an expression was expected of type
         "< a : 'c. 'c eval -> <['a]> eval; b : 'd. 'd eval -> <['b]> eval >"
       Type "'c eval -> <['a]> eval" = "'c1 eval -> <['a]> eval"
       is not compatible with type "'c2 eval -> <['a]> eval"
       The method "a" has type "'c4. 'c5 eval -> <['a]> eval",
       but the expected method type was "'c2. 'c2 eval -> <['a]> eval"
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
Line 2, characters 47-53:
2 |     : ('a. <[ $('a) -> $('a) ]> eval) -> int = eval x
                                                   ^^^^^^
Error: This expression has type
         "<[('a. 'a -> 'a) -> int]> eval" = "('a. 'a0 eval -> 'a eval) -> int"
       but an expression was expected of type
         "('a. 'a1 eval -> 'a eval) -> int"
       Type "'a eval -> 'a2 eval" = "'a3 eval -> 'a2 eval"
       is not compatible with type
         "'a4 eval -> 'a5 eval" = "'a6 eval -> 'a5 eval"
       Type "'a7" is not compatible with type "'a8"
|}]
(* fully re-staged *)
let f (x : <[('a. 'a -> 'a) -> int]> expr)
    : ('a. 'a eval -> 'a eval) -> int = eval x
[%%expect {|
Line 2, characters 40-46:
2 |     : ('a. 'a eval -> 'a eval) -> int = eval x
                                            ^^^^^^
Error: This expression has type
         "<[('a. 'a -> 'a) -> int]> eval" = "('a. 'a0 eval -> 'a eval) -> int"
       but an expression was expected of type "('a. 'a eval -> 'a eval) -> int"
       Type "'a eval -> 'a1 eval" = "'a2 eval -> 'a1 eval"
       is not compatible with type "'a3 eval -> 'a3 eval"
       Type "'a4" is not compatible with type "'a3"
|}]
(* variables from different quantifiers *)
let f (x : <[('a. 'a -> $('b) -> 'a) -> $('b)]> expr)
    : ('a. <[ $('a) -> $('b) -> $('a) ]> eval) -> 'b eval = eval x
[%%expect {|
Line 2, characters 60-66:
2 |     : ('a. <[ $('a) -> $('b) -> $('a) ]> eval) -> 'b eval = eval x
                                                                ^^^^^^
Error: This expression has type
         "<[('a. 'a -> 'b -> 'a) -> 'b]> eval" =
           "('a. 'a0 eval -> <['b]> eval -> 'a eval) -> <['b]> eval"
       but an expression was expected of type
         "('a. 'a1 eval -> <['b]> eval -> 'a eval) -> <['b]> eval"
       Type
         "'a eval -> <['b]> eval -> 'a2 eval" =
           "'a3 eval -> <['b]> eval -> 'a2 eval"
       is not compatible with type
         "'a4 eval -> <['b]> eval -> 'a5 eval" =
           "'a6 eval -> <['b]> eval -> 'a5 eval"
       Type "'a7" is not compatible with type "'a8"
|}]
(* nested quantifiers *)
let f (x : <[('a. ('b. 'a -> 'b) -> 'a) -> unit]> expr)
    : ('a. ('b. 'a eval -> 'b eval) -> 'a eval) -> unit = eval x
[%%expect {|
Line 2, characters 58-64:
2 |     : ('a. ('b. 'a eval -> 'b eval) -> 'a eval) -> unit = eval x
                                                              ^^^^^^
Error: This expression has type
         "<[('a. ('b. 'a -> 'b) -> 'a) -> unit]> eval" =
           "('a. ('b. 'a0 eval -> 'b eval) -> 'a eval) -> unit"
       but an expression was expected of type
         "('a. ('b. 'a eval -> 'b eval) -> 'a eval) -> unit"
       Type
         "('b. 'a eval -> 'b eval) -> 'a1 eval" =
           "('b. 'a2 eval -> 'b eval) -> 'a1 eval"
       is not compatible with type "('b. 'a3 eval -> 'b eval) -> 'a3 eval"
       Type "'a4 eval -> 'b eval" = "'a5 eval -> 'b eval"
       is not compatible with type "'a3 eval -> 'b0 eval"
       Type "'a6" is not compatible with type "'a3"
|}]
let f (x : <[('a. ('b. ('c. 'a -> 'b -> 'c) -> 'a -> 'b) -> 'a) -> unit]> expr)
    : ('a. ('b. ('c. 'a eval -> 'b eval -> 'c eval) -> 'a eval -> 'b eval)
            -> 'a eval) -> unit = eval x
[%%expect {|
Line 3, characters 34-40:
3 |             -> 'a eval) -> unit = eval x
                                      ^^^^^^
Error: This expression has type
         "<[('a. ('b. ('c. 'a -> 'b -> 'c) -> 'a -> 'b) -> 'a) -> unit]> eval"
           =
           "('a.
              ('b.
                 ('c. 'a0 eval -> 'b0 eval -> 'c eval) -> 'a1 eval -> 'b eval) ->
              'a eval) ->
           unit"
       but an expression was expected of type
         "('a.
            ('b. ('c. 'a eval -> 'b eval -> 'c eval) -> 'a eval -> 'b eval) ->
            'a eval) ->
         unit"
       Type
         "('b. ('c. 'a eval -> 'b1 eval -> 'c eval) -> 'a2 eval -> 'b eval) ->
         'a3 eval" =
           "('b. ('c. 'a4 eval -> 'b2 eval -> 'c eval) -> 'a5 eval -> 'b eval) ->
           'a3 eval"
       is not compatible with type
         "('b. ('c. 'a6 eval -> 'b eval -> 'c eval) -> 'a6 eval -> 'b eval) ->
         'a6 eval"
       Type
         "('c. 'a7 eval -> 'b eval -> 'c eval) -> 'a8 eval -> 'b3 eval" =
           "('c. 'a9 eval -> 'b4 eval -> 'c eval) -> 'a10 eval -> 'b3 eval"
       is not compatible with type
         "('c. 'a6 eval -> 'b5 eval -> 'c eval) -> 'a6 eval -> 'b5 eval"
       Type
         "'a11 eval -> 'b6 eval -> 'c eval" = "'a12 eval -> 'b7 eval -> 'c eval"
       is not compatible with type "'a6 eval -> 'b5 eval -> 'c0 eval"
       Type "'a13" is not compatible with type "'a6"
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
val f : <[int]> expr -> int = <fun>
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
