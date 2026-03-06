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
(* non-top-level type constructor *)
(* CR-soon jbachurski: This should never state that [<[t]> eval = t]. *)
let _ = <[ fun (type t) (x : t) -> $(Quote.Expr.int (eval <[ x ]> : int)) ]>
[%%expect {|
Line 1, characters 53-65:
1 | let _ = <[ fun (type t) (x : t) -> $(Quote.Expr.int (eval <[ x ]> : int)) ]>
                                                         ^^^^^^^^^^^^
Error: This expression has type "<[t]> eval" = "t"
       but an expression was expected of type "int"
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
(* closed object *)
let f (x : <[ <a: $('a); b: $('b)> ]> expr)
    : <a: 'a eval; b: 'b eval> = eval x
[%%expect {|
val f :
  ('a : any) ('b : any).
    <[< a : $('a); b : $('b) >]> expr -> < a : 'a eval; b : 'b eval > =
  <fun>
|}]
(* error! open object should not eval *)
let f (x : <[ <a: $('a); b: $('b); ..> ]> expr)
    : <a: 'a eval; b: 'b eval; ..> = eval x
[%%expect {|
Line 2, characters 37-43:
2 |     : <a: 'a eval; b: 'b eval; ..> = eval x
                                         ^^^^^^
Error: This expression has type
         "<[< a : 'a; b : 'b; .. > as 'c]> eval" = "<['c]> eval"
       but an expression was expected of type
         "< a : <['a]> eval; b : <['b]> eval; .. >"
|}]
(* open object should eval once closed *)
let f (x : <[ <a: $('a); b: $('b); ..> ]> expr)
    : <a: 'a eval; b: 'b eval; c: int>
    = let y = eval x in
      ignore (x : <[ <a: _; b: _; c: int > ]> expr); y
[%%expect {|
val f :
  ('a : any) ('b : any).
    <[< a : $('a); b : $('b); c : int >]> expr ->
    < a : 'a eval; b : 'b eval; c : int > =
  <fun>
|}]
(* polymorphic methods *)
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
(* closed variant *)
let f (x : <[[ `A of $('a) | `B of $('b) | `C ]]> expr)
    : [ `A of 'a eval | `B of 'b eval | `C ] = eval x
[%%expect {|
val f :
  <[[ `A of $('a) | `B of $('b) | `C ]]> expr ->
  [ `A of 'a eval | `B of 'b eval | `C ] = <fun>
|}]
(* error! upper-open variant should not eval *)
let f (x : <[[> `A of $('a) | `B of $('b) | `C ]]> expr)
    : [> `A of 'a eval | `B of 'b eval | `C ] = eval x
[%%expect {|
Line 2, characters 48-54:
2 |     : [> `A of 'a eval | `B of 'b eval | `C ] = eval x
                                                    ^^^^^^
Error: This expression has type
         "<[[> `A of $('a) | `B of $('b) | `C ] as 'c]> eval" = "<['c]> eval"
       but an expression was expected of type
         "[> `A of 'a eval | `B of 'b eval | `C ]"
|}]
(* upper-open variant should eval once closed *)
let f (x : <[[> `A of $('a) | `B of $('b) | `C ]]> expr)
    : [ `A of 'a eval | `B of 'b eval | `C | `D ]
    = let y = eval x in
      ignore (x : <[[ `A of _ | `B of _ | `C | `D ]]> expr); y
[%%expect {|
val f :
  <[[ `A of $('a) | `B of $('b) | `C | `D ]]> expr ->
  [ `A of 'a eval | `B of 'b eval | `C | `D ] = <fun>
|}]
(* error! lower-open variant should not eval *)
let f (x : <[[< `A of $('a) | `B of $('b1) & $('b2) | `C ]]> expr)
    : [< `A of 'a eval | `B of 'b1 eval & 'b2 eval | `C ] = eval x
[%%expect {|
Line 2, characters 60-66:
2 |     : [< `A of 'a eval | `B of 'b1 eval & 'b2 eval | `C ] = eval x
                                                                ^^^^^^
Error: This expression has type
         "<[[< `A of $('a) | `B of $('b1) & $('b2) | `C ] as 'b]> eval" =
           "<['b]> eval"
       but an expression was expected of type
         "[< `A of 'a eval | `B of 'b1 eval & 'b2 eval | `C ]"
|}]
(* lower-open variant should eval once closed *)
let f (x : <[[< `A of $('a) | `B of $('b1) & $('b2) | `C ]]> expr)
    : [ `A of 'a eval | `B of 'b eval ]
    = let y = eval x in
      ignore (x : <[[ `A of _ | `B of _ ]]> expr); y

[%%expect {|
val f :
  <[[ `A of $('a) | `B of $('b) ]]> expr -> [ `A of 'a eval | `B of 'b eval ] =
  <fun>
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

(** Quote-kinded types **)

(* Test that quote-kinded (and quote-kind-parameterised) type constructors
   do not beta-reduce under quote-eval. *)
(* CR quoted-kinds jbachurski: For now, these tests indicate incompleteness lurking
   in the system that will be fixed by quoted kinds. *)

(* CR quoted-kinds jbachurski: Annotate [t : <[value]>]. *)
module QuoteKinded : sig
  type t
end = struct
  type t = <[int]>
end
(* CR quoted-kinds jbachurski: Annotate ['a : <[value]>]. *)
module QuoteKindedParam : sig
  type 'a t
end = struct
  type 'a t = 'a expr
end
[%%expect {|
module QuoteKinded : sig type t end
module QuoteKindedParam : sig type 'a t end
|}]
#mark_toplevel_in_quotations

(* CR quoted-kinds jbachurski: None of the types on [x] should reduce and hence all tests
   with annotated results should error. *)

(* [expr] should not reduce, as it has a quote-kinded parameter *)
let f (x : <[$('a) expr]> eval) : 'a eval expr = x
[%%expect {|
val f : <[$('a) expr]> eval -> 'a eval expr = <fun>
|}]
let f (x : <[<[int]> expr]> eval) : <[<[int]>]> eval expr = x
[%%expect {|
val f : <[<[int]> expr]> eval -> <[<[int]>]> eval expr = <fun>
|}]

(* [eval] should not reduce, as it has a quote-kinded parameter *)
(* CR quoted-kinds jbachurski: This test already fails, but for the wrong reason --
   [Ctype.expand_head] will expand the [Tconstr] containing [Tquote_eval] and
   will never see the inner expansion. *)
let f (x : <['a eval]> eval) : <['a]> eval eval = x
[%%expect {|
Line 1, characters 50-51:
1 | let f (x : <['a eval]> eval) : <['a]> eval eval = x
                                                      ^
Error: This expression has type "<['a eval]> eval"
       but an expression was expected of type "<['a]> eval eval"
       Type "'a eval" is not compatible with type "$(<['a]> eval)"
|}]
let f (x : <[$('a) eval]> eval) = x
[%%expect {|
val f : ('a : any). <[$('a) eval]> eval -> <[$('a) eval]> eval = <fun>
|}]
(* The inner eval is allowed to reduce on [int], but not the outer on [<[int]> eval]. *)
let f (x : <[<[int]> eval]> eval) : <[<[int]>]> eval eval = x
[%%expect {|
Line 1, characters 60-61:
1 | let f (x : <[<[int]> eval]> eval) : <[<[int]>]> eval eval = x
                                                                ^
Error: This expression has type "<[<[int]> eval]> eval" = "int"
       but an expression was expected of type "<[<[int]>]> eval eval"
|}]
(* This one should definitely succeed *)
let f (x : <[<[int]> eval]> eval) : int = x
[%%expect {|
val f : <[<[int]> eval]> eval -> int = <fun>
|}]

(* quote-kinded types should not reduce *)
let f (x : <[QuoteKinded.t]> eval expr) : QuoteKinded.t expr = x
[%%expect {|
val f : <[QuoteKinded.t]> eval expr -> QuoteKinded.t expr = <fun>
|}]

(* quote-kind-parameterised types should not reduce *)
let f (x : <[$('a) QuoteKindedParam.t]> eval expr) : 'a eval QuoteKindedParam.t expr = x
[%%expect {|
val f :
  <[$('a) QuoteKindedParam.t]> eval expr -> 'a eval QuoteKindedParam.t expr =
  <fun>
|}]
let f (x : <[<[int]> QuoteKindedParam.t]> eval expr)
         : <[<[int]>]> eval QuoteKindedParam.t expr = x
[%%expect {|
val f :
  <[<[int]> QuoteKindedParam.t]> eval expr ->
  <[<[int]>]> eval QuoteKindedParam.t expr = <fun>
|}]
