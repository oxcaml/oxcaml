(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

type ('a : any) t = V : 'a t | B : ('a : bits64) t
[%%expect{|
type ('a : any) t = V : 'a t | B : ('a : bits64). 'a t
|}]

(* Regression test: we should not treat [x] as representable. Even though it
   has kind [value] in the function body, this depends on being under the scope
   of [V], yet a caller could pass [B], causing a segfault during the function
   call before a match error can be raised. *)
let f : type (a : any). a t -> a -> unit -> a = fun V x () -> x
[%%expect{|
Line 1, characters 48-63:
1 | let f : type (a : any). a t -> a -> unit -> a = fun V x () -> x
                                                    ^^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to pass a function argument.
|}]

(* The result sort is part of the calling convention for the same reason. *)
let f_ret : type (a : any). a t -> unit -> a = fun V () -> assert false
[%%expect{|
Line 1, characters 47-71:
1 | let f_ret : type (a : any). a t -> unit -> a = fun V () -> assert false
                                                   ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

(* Trailing [function] cases form one function with the preceding parameters,
   so their argument and result sorts are part of the calling convention
   too. *)
let f_cases : type (a : any). a t -> a -> a = fun V -> function x -> x
[%%expect{|
Line 1, characters 46-70:
1 | let f_cases : type (a : any). a t -> a -> a = fun V -> function x -> x
                                                  ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to pass a function argument.
|}]

(* The pattern on the optional argument covers its payload, but a caller can
   omit the argument entirely. [g_opt #2L] used to segfault. *)
type ('a : any) opt = YesO : (int -> int) opt

let g_opt : type (a : any). ?x:(a opt) -> a -> unit -> (int -> int) =
  fun ?x:(YesO = assert false) y () -> y
[%%expect{|
type ('a : any) opt = YesO : (int -> int) opt
Line 4, characters 10-14:
4 |   fun ?x:(YesO = assert false) y () -> y
              ^^^^
Warning 18 [not-principal]: typing this pattern requires considering
  "int -> int" and "a" as equal. But the knowledge of these types is not
  principal.

Line 4, characters 2-40:
4 |   fun ?x:(YesO = assert false) y () -> y
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to pass a function argument.
|}]

(* Sound, as the later layouts do not rely on the optional argument's
   narrowing. *)
type _ w = AW : int w

let opt_value : type a. ?x:(a w) -> a -> unit =
  fun ?x:(AW = assert false) _y -> ()
[%%expect{|
type _ w = AW : int w
Line 4, characters 10-12:
4 |   fun ?x:(AW = assert false) _y -> ()
              ^^
Warning 18 [not-principal]: typing this pattern requires considering
  "int" and "a" as equal. But the knowledge of these types is not principal.

val opt_value : ?x:'a w -> 'a -> unit = <fun>
|}]

(* A nested function's sorts could soundly use the narrowing (the closure is
   only created after the match on [V] succeeds), but we conservatively drop
   a partial match's equations everywhere. *)
let nested : type (a : any). a t -> (a -> a) = fun V -> fun x -> x
[%%expect{|
Line 1, characters 56-66:
1 | let nested : type (a : any). a t -> (a -> a) = fun V -> fun x -> x
                                                            ^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to pass a function argument.
|}]

(* Sound, as at [a : value], [B] is refuted by its kind, making the match
   total. *)
let total_by_refutation : type a. a t -> a -> a = fun V x -> x
[%%expect{|
val total_by_refutation : 'a t -> 'a -> 'a = <fun>
|}]

type ('a : any) s = I : int s
[%%expect{|
type ('a : any) s = I : int s
|}]

(* Sound, as a total single-constructor match may narrow later parameters'
   sorts. *)
let total : type (a : any). a s -> a -> a = fun I x -> x
[%%expect{|
val total : ('a : any). 'a s -> 'a -> 'a = <fun>
|}]

(* Sound, as [A]'s narrowing does not affect any layouts. *)
type _ v = A : int v | C : string v
let partial_value : type a. a v -> a -> unit = fun A _x -> ()
[%%expect{|
type _ v = A : int v | C : string v
Line 2, characters 51-52:
2 | let partial_value : type a. a v -> a -> unit = fun A _x -> ()
                                                       ^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "C"

val partial_value : 'a v -> 'a -> unit = <fun>
|}]

(* It would be sound for the body to use the partial match's narrowing (the
   body only runs once the match succeeds), but we conservatively drop the
   equations everywhere. *)
let body_uses_narrowing : type a. a v -> a -> int = fun A x -> x
[%%expect{|
Line 1, characters 63-64:
1 | let body_uses_narrowing : type a. a v -> a -> int = fun A x -> x
                                                                   ^
Error: The value "x" has type "a" but an expression was expected of type "int"
|}]

(* A total match's equations are kept even under an enclosing partial match:
   [_x]'s layout is justified by [I] alone. *)
let partial_then_total : type a (b : any). a v -> b s -> b -> unit =
  fun A I _x -> ()
[%%expect{|
Line 2, characters 6-7:
2 |   fun A I _x -> ()
          ^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "C"

val partial_then_total : 'a ('b : any). 'a v -> 'b s -> 'b -> unit = <fun>
|}]

(* Dropping [Val]'s equations must also keep [Refl]'s equation from
   recording what they implied: [b]'s jkind may not be taken to be [value].
   [f_eq B64 Refl #2L] used to segfault. *)
type ('a : any) tv = Val : ('a : value) tv | B64 : ('a : bits64) tv
type ('x : any, 'y : any) eq = Refl : ('z : any). ('z, 'z) eq

let f_eq : type (a : any) (b : any). a tv -> (a, b) eq -> b -> unit -> unit =
  fun Val Refl x () -> ()
[%%expect{|
type ('a : any) tv = Val : 'a tv | B64 : ('a : bits64). 'a tv
type ('x : any, 'y : any) eq = Refl : ('z : any). ('z, 'z) eq
Line 5, characters 2-25:
5 |   fun Val Refl x () -> ()
      ^^^^^^^^^^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of b is any
         because of the annotation on the abstract type declaration for a.
       But the layout of b must be representable
         because we must know concretely how to pass a function argument.
|}]
