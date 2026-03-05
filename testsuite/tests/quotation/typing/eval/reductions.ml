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
Line 1, characters 6-26:
1 | let f (x : <[$('a)]> eval) : 'a eval = x
          ^^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type "'a eval"
       but a pattern was expected which matches values of type
         "('b : '_representable_layout_1)"
       The layout of 'a eval is any
         because it's a fresh unification variable.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of 'a eval must be representable
         because we must know concretely how to pass a function argument.
|}]
let f (x : <[$(<[$('a)]>)]> eval) : 'a eval = x
[%%expect {|
Line 1, characters 6-33:
1 | let f (x : <[$(<[$('a)]>)]> eval) : 'a eval = x
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type "'a eval"
       but a pattern was expected which matches values of type
         "('b : '_representable_layout_2)"
       The layout of 'a eval is any
         because it's a fresh unification variable.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of 'a eval must be representable
         because we must know concretely how to pass a function argument.
|}]


(** Basics with expression-level eval **)

(* [eval] stub *)
open (struct
  let eval = Obj.magic
end : sig
  val eval : 'a expr -> 'a eval
end)
[%%expect {|
>> Fatal error: estimate_type_jkind: Non-quote-kinded splice type
Uncaught exception: Misc.Fatal_error

|}]

let f (x : <[int]> expr) : int = eval x
[%%expect {|
Line 1, characters 33-37:
1 | let f (x : <[int]> expr) : int = eval x
                                     ^^^^
Error: Unbound value "eval"
|}]

let f (x : <[string]> expr) : string = eval x
[%%expect {|
Line 1, characters 39-43:
1 | let f (x : <[string]> expr) : string = eval x
                                           ^^^^
Error: Unbound value "eval"
|}]

let f (x : <[int]> expr) : string = eval x (* type error: int ~/~ string *)
[%%expect {|
Line 1, characters 36-40:
1 | let f (x : <[int]> expr) : string = eval x (* type error: int ~/~ string *)
                                        ^^^^
Error: Unbound value "eval"
|}]

(* Printing *)
let f (x : <['a]> expr) = eval x
[%%expect {|
Line 1, characters 26-30:
1 | let f (x : <['a]> expr) = eval x
                              ^^^^
Error: Unbound value "eval"
|}]
let f (x : <[int]> expr) = eval x
[%%expect {|
Line 1, characters 27-31:
1 | let f (x : <[int]> expr) = eval x
                               ^^^^
Error: Unbound value "eval"
|}]


(** Type formers **)

(* Type constructors *)
let f (x : <[$('a) list]> expr) : 'a eval list = eval x
[%%expect {|
Line 4, characters 13-18:
4 | let f (x : <[$('a) list]> expr) : 'a eval list = eval x
                 ^^^^^
Error: This type "$('a)" should be an instance of type "('b : value_or_null)"
       The layout of $('a) is any
         because it's a fresh unification variable.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of $('a) must be a sublayout of value
         because the type argument of list has layout value_or_null.
|}]
let f (x : <[($('a), $('b)) Either.t]> expr)
    : ('a eval, 'b eval) Either.t = eval x
[%%expect {|
Line 1, characters 14-19:
1 | let f (x : <[($('a), $('b)) Either.t]> expr)
                  ^^^^^
Error: This type "$('a)" should be an instance of type "('b : value_or_null)"
       The layout of $('a) is any
         because it's a fresh unification variable.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of $('a) must be a sublayout of value
         because the 1st type argument of Either.t has this layout.
|}]
(* non-top-level type constructor *)
(* CR-soon jbachurski: This should never state that [<[t]> eval = t]. *)
let _ = <[ fun (type t) (x : t) -> $(Quote.Expr.int (eval <[ x ]> : int)) ]>
[%%expect {|
Line 1, characters 53-57:
1 | let _ = <[ fun (type t) (x : t) -> $(Quote.Expr.int (eval <[ x ]> : int)) ]>
                                                         ^^^^
Error: Unbound value "eval"
|}]

(* Arrows *)
let f (x : <[$('a) -> $('b)]> expr) : 'a eval -> 'b eval = eval x
[%%expect {|
Line 1, characters 59-63:
1 | let f (x : <[$('a) -> $('b)]> expr) : 'a eval -> 'b eval = eval x
                                                               ^^^^
Error: Unbound value "eval"
|}]
let f (x : <[l:$('a) -> $('b)]> expr) : l:('a eval) -> 'b eval = eval x
[%%expect {|
Line 1, characters 65-69:
1 | let f (x : <[l:$('a) -> $('b)]> expr) : l:('a eval) -> 'b eval = eval x
                                                                     ^^^^
Error: Unbound value "eval"
|}]
let f (x : <[?l:$('a) -> $('b)]> expr) : ?l:('a eval) -> 'b eval = eval x
[%%expect {|
Line 1, characters 67-71:
1 | let f (x : <[?l:$('a) -> $('b)]> expr) : ?l:('a eval) -> 'b eval = eval x
                                                                       ^^^^
Error: Unbound value "eval"
|}]
let f (x : <[$('a) @ local -> $('b) @ local]> expr)
    : 'a eval @ local -> 'b eval @ local = eval x
[%%expect {|
Line 2, characters 43-47:
2 |     : 'a eval @ local -> 'b eval @ local = eval x
                                               ^^^^
Error: Unbound value "eval"
|}]

(* Tuples *)
let f (x : <[$('a) * $('b) * $('c)]> expr)
    : 'a eval * 'b eval * 'c eval = eval x
[%%expect {|
Line 1, characters 13-18:
1 | let f (x : <[$('a) * $('b) * $('c)]> expr)
                 ^^^^^
Error: Tuple element types must have layout value.
       The layout of "$('a)" is any
         because it's a fresh unification variable.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of "$('a)" must be a sublayout of value
         because it's the type of a tuple element.
|}]

(* Unboxed tuples *)
let f (x : <[#($('a) * $('b) * $('c))]> expr)
    : #('a eval * 'b eval * 'c eval) = eval x
[%%expect {|
Line 2, characters 39-43:
2 |     : #('a eval * 'b eval * 'c eval) = eval x
                                           ^^^^
Error: Unbound value "eval"
|}]

(* Objects *)
(* closed object *)
let f (x : <[ <a: $('a); b: $('b)> ]> expr)
    : <a: 'a eval; b: 'b eval> = eval x
[%%expect {|
Line 1, characters 15-24:
1 | let f (x : <[ <a: $('a); b: $('b)> ]> expr)
                   ^^^^^^^^^
Error: Object field types must have layout value.
       The layout of "'a" is any
         because it's a fresh unification variable.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of "'a" must be a sublayout of value
         because it's the type of an object field.
|}]
(* error! open object should not eval *)
let f (x : <[ <a: $('a); b: $('b); ..> ]> expr)
    : <a: 'a eval; b: 'b eval; ..> = eval x
[%%expect {|
Line 1, characters 15-24:
1 | let f (x : <[ <a: $('a); b: $('b); ..> ]> expr)
                   ^^^^^^^^^
Error: Object field types must have layout value.
       The layout of "'a" is any
         because it's a fresh unification variable.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of "'a" must be a sublayout of value
         because it's the type of an object field.
|}]
(* open object should eval once closed *)
let f (x : <[ <a: $('a); b: $('b); ..> ]> expr)
    : <a: 'a eval; b: 'b eval; c: int>
    = let y = eval x in
      ignore (x : <[ <a: _; b: _; c: int > ]> expr); y
[%%expect {|
Line 1, characters 15-24:
1 | let f (x : <[ <a: $('a); b: $('b); ..> ]> expr)
                   ^^^^^^^^^
Error: Object field types must have layout value.
       The layout of "'a" is any
         because it's a fresh unification variable.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of "'a" must be a sublayout of value
         because it's the type of an object field.
|}]
(* polymorphic methods *)
let f (x : <[ <a: 'c. 'c -> $('a); b: 'd. 'd -> $('b)> ]> expr)
    : <a: 'c. 'c eval -> 'a eval; b: 'd. 'd eval -> 'b eval> = eval x
[%%expect {|
Line 2, characters 63-67:
2 |     : <a: 'c. 'c eval -> 'a eval; b: 'd. 'd eval -> 'b eval> = eval x
                                                                   ^^^^
Error: Unbound value "eval"
|}]

(* Polymorphic variants *)
(* closed variant *)
let f (x : <[[ `A of $('a) | `B of $('b) | `C ]]> expr)
    : [ `A of 'a eval | `B of 'b eval | `C ] = eval x
[%%expect {|
Line 1, characters 21-26:
1 | let f (x : <[[ `A of $('a) | `B of $('b) | `C ]]> expr)
                         ^^^^^
Error: Polymorphic variant constructor argument types must have layout value.
       The layout of "$('a)" is any
         because it's a fresh unification variable.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of "$('a)" must be a sublayout of value
         because it's the type of the field of a polymorphic variant.
|}]
(* error! upper-open variant should not eval *)
let f (x : <[[> `A of $('a) | `B of $('b) | `C ]]> expr)
    : [> `A of 'a eval | `B of 'b eval | `C ] = eval x
[%%expect {|
Line 1, characters 22-27:
1 | let f (x : <[[> `A of $('a) | `B of $('b) | `C ]]> expr)
                          ^^^^^
Error: Polymorphic variant constructor argument types must have layout value.
       The layout of "$('a)" is any
         because it's a fresh unification variable.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of "$('a)" must be a sublayout of value
         because it's the type of the field of a polymorphic variant.
|}]
(* upper-open variant should eval once closed *)
let f (x : <[[> `A of $('a) | `B of $('b) | `C ]]> expr)
    : [ `A of 'a eval | `B of 'b eval | `C | `D ]
    = let y = eval x in
      ignore (x : <[[ `A of _ | `B of _ | `C | `D ]]> expr); y
[%%expect {|
Line 1, characters 22-27:
1 | let f (x : <[[> `A of $('a) | `B of $('b) | `C ]]> expr)
                          ^^^^^
Error: Polymorphic variant constructor argument types must have layout value.
       The layout of "$('a)" is any
         because it's a fresh unification variable.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of "$('a)" must be a sublayout of value
         because it's the type of the field of a polymorphic variant.
|}]
(* error! lower-open variant should not eval *)
let f (x : <[[< `A of $('a) | `B of $('b1) & $('b2) | `C ]]> expr)
    : [< `A of 'a eval | `B of 'b1 eval & 'b2 eval | `C ] = eval x
[%%expect {|
Line 1, characters 22-27:
1 | let f (x : <[[< `A of $('a) | `B of $('b1) & $('b2) | `C ]]> expr)
                          ^^^^^
Error: Polymorphic variant constructor argument types must have layout value.
       The layout of "$('a)" is any
         because it's a fresh unification variable.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of "$('a)" must be a sublayout of value
         because it's the type of the field of a polymorphic variant.
|}]
(* lower-open variant should eval once closed *)
let f (x : <[[< `A of $('a) | `B of $('b1) & $('b2) | `C ]]> expr)
    : [ `A of 'a eval | `B of 'b eval ]
    = let y = eval x in
      ignore (x : <[[ `A of _ | `B of _ ]]> expr); y

[%%expect {|
Line 1, characters 22-27:
1 | let f (x : <[[< `A of $('a) | `B of $('b1) & $('b2) | `C ]]> expr)
                          ^^^^^
Error: Polymorphic variant constructor argument types must have layout value.
       The layout of "$('a)" is any
         because it's a fresh unification variable.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of "$('a)" must be a sublayout of value
         because it's the type of the field of a polymorphic variant.
|}]

(* Quantifiers *)
(* partially re-staged *)
let f (x : <[('a. 'a -> 'a) -> int]> expr)
    : ('a. <[ $('a) -> $('a) ]> eval) -> int = eval x
[%%expect {|
Line 2, characters 47-51:
2 |     : ('a. <[ $('a) -> $('a) ]> eval) -> int = eval x
                                                   ^^^^
Error: Unbound value "eval"
|}]
(* fully re-staged *)
let f (x : <[('a. 'a -> 'a) -> int]> expr)
    : ('a. 'a eval -> 'a eval) -> int = eval x
[%%expect {|
Line 2, characters 40-44:
2 |     : ('a. 'a eval -> 'a eval) -> int = eval x
                                            ^^^^
Error: Unbound value "eval"
|}]
(* variables from different quantifiers *)
let f (x : <[('a. 'a -> $('b) -> 'a) -> $('b)]> expr)
    : ('a. <[ $('a) -> $('b) -> $('a) ]> eval) -> 'b eval = eval x
[%%expect {|
Line 2, characters 60-64:
2 |     : ('a. <[ $('a) -> $('b) -> $('a) ]> eval) -> 'b eval = eval x
                                                                ^^^^
Error: Unbound value "eval"
|}]
(* nested quantifiers *)
let f (x : <[('a. ('b. 'a -> 'b) -> 'a) -> unit]> expr)
    : ('a. ('b. 'a eval -> 'b eval) -> 'a eval) -> unit = eval x
[%%expect {|
Line 2, characters 58-62:
2 |     : ('a. ('b. 'a eval -> 'b eval) -> 'a eval) -> unit = eval x
                                                              ^^^^
Error: Unbound value "eval"
|}]
let f (x : <[('a. ('b. ('c. 'a -> 'b -> 'c) -> 'a -> 'b) -> 'a) -> unit]> expr)
    : ('a. ('b. ('c. 'a eval -> 'b eval -> 'c eval) -> 'a eval -> 'b eval)
            -> 'a eval) -> unit = eval x
[%%expect {|
Line 3, characters 34-38:
3 |             -> 'a eval) -> unit = eval x
                                      ^^^^
Error: Unbound value "eval"
|}]
(* [instance_poly] has special cases for polymorphic variants and objects,
   so we test them separately. *)
(* polymorphic variant *)
let f (x : <[('a. 'a -> [`Foo of 'a | `Bar of 'a]) -> unit]> expr)
    : ('a. 'a eval -> [`Foo of 'a eval | `Bar of 'a eval]) -> unit = eval x
[%%expect {|
>> Fatal error: estimate_type_jkind: Non-quote-kinded splice type
Uncaught exception: Misc.Fatal_error

|}]
(* object *)
let f (x : <[('a. 'a -> <i : 'a; j : 'a>) -> unit]> expr)
    : ('a. 'a eval -> <i : 'a eval; j : 'a eval>) -> unit = eval x
[%%expect {|
>> Fatal error: estimate_type_jkind: Non-quote-kinded splice type
Uncaught exception: Misc.Fatal_error

|}]

(* Package types *)
let f (x : <[(module Map.OrderedType with type t = $('a)) -> unit]> expr)
    : (module Map.OrderedType with type t = 'a eval) -> unit = eval x
[%%expect {|
Line 1, characters 13-57:
1 | let f (x : <[(module Map.OrderedType with type t = $('a)) -> unit]> expr)
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "t"
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t = $('a)
       is not included in
         type t
       The layout of the first is any
         because it's a fresh unification variable.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of the first must be a sublayout of value.
       File "map.mli", line 56, characters 4-10: Expected declaration
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
>> Fatal error: estimate_type_jkind: Non-quote-kinded splice type
Uncaught exception: Misc.Fatal_error

|}]

(* The obvious form of eval composition -- [fun x -> eval (eval x)] --
   has an irreducible type under our restriction to next-stage types.
   We make sure the alternative -- [fun x -> eval (eval $x)] -- behaves. *)

let f (x : 'a expr) : 'a eval =
  eval0 x
[%%expect {|
Line 2, characters 2-7:
2 |   eval0 x
      ^^^^^
Error: Unbound value "eval0"
|}]
let f (x : <[ $('a) expr ]> expr) : <[ $('a) eval ]> eval =
  eval0 <[ $eval1 $x ]>
[%%expect {|
Line 2, characters 2-7:
2 |   eval0 <[ $eval1 $x ]>
      ^^^^^
Error: Unbound value "eval0"
|}]
let f (x : <[ <[ $($('a)) expr ]> expr ]> expr)
         : <[ <[ $($('a)) eval ]> eval ]> eval =
  eval0 <[$eval1 <[$($eval2) $($x)]>]>
[%%expect {|
Line 3, characters 2-7:
3 |   eval0 <[$eval1 <[$($eval2) $($x)]>]>
      ^^^^^
Error: Unbound value "eval0"
|}]
(* Concrete types reduce in nested evals *)
let f (x : <[int]> expr) : <[int]> eval =
  eval0 x
[%%expect {|
Line 2, characters 2-7:
2 |   eval0 x
      ^^^^^
Error: Unbound value "eval0"
|}]
let f (x : <[ <[int]> expr ]> expr) : int =
  eval0 <[$eval1 $x]>
[%%expect {|
Line 2, characters 2-7:
2 |   eval0 <[$eval1 $x]>
      ^^^^^
Error: Unbound value "eval0"
|}]
let f (x : <[<[<[int]> expr]> expr]> expr) : int =
  eval0 <[$eval1 <[ $($eval2) $($x) ]>]>
[%%expect {|
Line 2, characters 2-7:
2 |   eval0 <[$eval1 <[ $($eval2) $($x) ]>]>
      ^^^^^
Error: Unbound value "eval0"
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
Line 7, characters 16-25:
7 | let f (x : (<[ <m : $('a) > ]> as 'a) eval) : (< m : 'b > as 'b) = x
                    ^^^^^^^^^
Error: Object field types must have layout value.
       The layout of "$('a)" is any
         because it's assigned a dummy kind that should have been overwritten.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of "$('a)" must be a sublayout of value
         because it's the type of an object field.
|}]

let f (x : (<[ <m : $('a) list > ]> as 'a) eval) : (< m : 'b list > as 'b) = x
[%%expect {|
Line 1, characters 20-25:
1 | let f (x : (<[ <m : $('a) list > ]> as 'a) eval) : (< m : 'b list > as 'b) = x
                        ^^^^^
Error: This type "$('a)" should be an instance of type "('b : value_or_null)"
       The layout of $('a) is any
         because it's assigned a dummy kind that should have been overwritten.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of $('a) must be a sublayout of value
         because the type argument of list has layout value_or_null.
|}]

(** Quote-kinded types **)

(* Test that quote-kinded (and quote-kind-parameterised) type constructors
   do not beta-reduce under quote-eval. *)
(* CR quoted-kinds jbachurski: For now, these tests indicate incompleteness
   lurking in the system that will be fixed by quoted kinds. *)

module QuoteKinded : sig
  type t : <[value]>
end = struct
  type t = <[int]>
end
module QuoteKindedParam : sig
  type ('a : <[value]>) t
end = struct
  type 'a t = 'a expr
end
[%%expect {|
module QuoteKinded : sig type t : <[value]> end
Lines 15-17, characters 6-3:
15 | ......struct
16 |   type 'a t = 'a expr
17 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a expr end
       is not included in
         sig type ('a : <[value]>) t end
       Type declarations do not match:
         type 'a t = 'a expr
       is not included in
         type ('a : <[value]>) t
       The problem is in the kinds of a parameter:
       The layout of 'a is value
         because of the definition of t at line 14, characters 2-25.
       But the layout of 'a must overlap with value
         because of the definition of t at line 16, characters 2-21.
|}]
#mark_toplevel_in_quotations

(* CR quoted-kinds jbachurski: None of the types on [x] should reduce and hence all tests
   with annotated results should error. *)

(* [expr] should not reduce, as it has a quote-kinded parameter *)
let f (x : <[$('a) expr]> eval) : 'a eval expr = x
[%%expect {|
val f : ('a : any). <[$('a) expr]> eval -> 'a eval expr = <fun>
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
Line 1, characters 6-28:
1 | let f (x : <['a eval]> eval) : <['a]> eval eval = x
          ^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type "<['a eval]> eval"
       but a pattern was expected which matches values of type
         "('b : '_representable_layout_3)"
       The layout of <['a eval]> eval is any
         because it's a fresh unification variable.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of <['a eval]> eval must be representable
         because we must know concretely how to pass a function argument.
|}]
let f (x : <[$('a) eval]> eval) = x
[%%expect {|
Line 1, characters 6-31:
1 | let f (x : <[$('a) eval]> eval) = x
          ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type "<[$('a) eval]> eval"
       but a pattern was expected which matches values of type
         "('b : '_representable_layout_4)"
       The layout of <[$('a) eval]> eval is any
         because it's a fresh unification variable.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of <[$('a) eval]> eval must be representable
         because we must know concretely how to pass a function argument.
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
let f (x : <[<[int]> eval]> eval) = x
[%%expect {|
val f : <[<[int]> eval]> eval -> <[<[int]> eval]> eval = <fun>
|}]

(* quote-kinded types should not reduce *)
let f (x : <[QuoteKinded.t]> eval expr) : QuoteKinded.t expr = x
[%%expect {|
val f : <[QuoteKinded.t]> eval expr -> QuoteKinded.t expr = <fun>
|}]

(* quote-kind-parameterised types should not reduce *)
let f (x : <[$('a) QuoteKindedParam.t]> eval expr) : 'a eval QuoteKindedParam.t expr = x
[%%expect {|
Line 1, characters 19-37:
1 | let f (x : <[$('a) QuoteKindedParam.t]> eval expr) : 'a eval QuoteKindedParam.t expr = x
                       ^^^^^^^^^^^^^^^^^^
Error: Unbound module "QuoteKindedParam"
|}]
let f (x : <[<[int]> QuoteKindedParam.t]> eval expr)
         : <[<[int]>]> eval QuoteKindedParam.t expr = x
[%%expect {|
Line 1, characters 21-39:
1 | let f (x : <[<[int]> QuoteKindedParam.t]> eval expr)
                         ^^^^^^^^^^^^^^^^^^
Error: Unbound module "QuoteKindedParam"
|}]
