(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* The result sort is part of the calling convention, so it may not rely on a
   parameter's partial GADT match. Unlike the results in
   [function_arg_gadt_any.ml], the results here get their sort from a return
   site, which is only known once the body has been typed. *)

type ('a : any) t = V : int t | B : ('a : bits64) t
[%%expect{|
type ('a : any) t = V : int t | B : ('a : bits64). 'a t
|}]

(* The sort of the result is known only from the return site in the [V]
   branch, but a caller could pass [B]. *)
let direct : type (a : any). a t -> unit -> a = fun V () -> 42
[%%expect{|
Line 1, characters 52-53:
1 | let direct : type (a : any). a t -> unit -> a = fun V () -> 42
                                                        ^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "B"

Line 1, characters 54-62:
1 | let direct : type (a : any). a t -> unit -> a = fun V () -> 42
                                                          ^^^^^^^^
Error: This function's result has type a,
       whose layout is known only from the GADT pattern match at line 1, characters 52-53.
       That match is not exhaustive, so a caller could reach this result at a different layout.
       Function arguments and results must be representable independently of any non-exhaustive match.
|}]

(* Sound: a one-constructor match is total, so the result may rely on it. *)
type ('a : any) one = One : int one

let total : type (a : any). a one -> unit -> a = fun One () -> 42
[%%expect{|
type ('a : any) one = One : int one
val total : ('a : any). 'a one -> unit -> 'a = <fun>
|}]

(* A caller can omit the optional argument entirely, so its pattern is
   effectively partial for the result too. *)
let optional : type (a : any). ?x:(a one) -> unit -> a =
  fun ?x:(One = assert false) () -> 42
[%%expect{|
Line 2, characters 10-13:
2 |   fun ?x:(One = assert false) () -> 42
              ^^^
Warning 18 [not-principal]: typing this pattern requires considering
  "int" and "a" as equal. But the knowledge of these types is not principal.

Line 2, characters 30-38:
2 |   fun ?x:(One = assert false) () -> 42
                                  ^^^^^^^^
Error: This function's result has type a,
       whose layout is known only from the GADT pattern match at line 2, characters 10-13.
       That pattern matches an optional argument that a caller could omit, so a caller could reach this result at a different layout.
       Function arguments and results must be representable independently of the patterns of optional arguments.
|}]

(* When both parameters refine the same type, the outer one is reported:
   reverting only the inner parameter's constraints still leaves the outer
   refinement in scope, so that check passes. *)
let both : type (a : any). a t -> a t -> unit -> a = fun V V () -> 42
[%%expect{|
Line 1, characters 57-58:
1 | let both : type (a : any). a t -> a t -> unit -> a = fun V V () -> 42
                                                             ^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "B"

Line 1, characters 61-69:
1 | let both : type (a : any). a t -> a t -> unit -> a = fun V V () -> 42
                                                                 ^^^^^^^^
Error: This function's result has type a,
       whose layout is known only from the GADT pattern match at line 1, characters 57-58.
       That match is not exhaustive, so a caller could reach this result at a different layout.
       Function arguments and results must be representable independently of any non-exhaustive match.
|}]

(* The outer match refines an unrelated type, so only the inner one justifies
   the result sort; the innermost failing match is the one reported. *)
let innermost : type (a : any) (b : any). b t -> a t -> unit -> a =
  fun V V () -> 42
[%%expect{|
Line 2, characters 8-9:
2 |   fun V V () -> 42
            ^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "B"

Line 2, characters 6-7:
2 |   fun V V () -> 42
          ^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "B"

Line 2, characters 10-18:
2 |   fun V V () -> 42
              ^^^^^^^^
Error: This function's result has type a,
       whose layout is known only from the GADT pattern match at line 2, characters 8-9.
       That match is not exhaustive, so a caller could reach this result at a different layout.
       Function arguments and results must be representable independently of any non-exhaustive match.
|}]

(* The result sort is forced inside the branch by a [try] that must record a
   result sort, but it is still the function's result sort. *)
let via_try : type (a : any). a t -> unit -> a =
  fun V () -> (try 42 with Not_found -> 0)
[%%expect{|
Line 2, characters 6-7:
2 |   fun V () -> (try 42 with Not_found -> 0)
          ^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "B"

Line 2, characters 8-42:
2 |   fun V () -> (try 42 with Not_found -> 0)
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This function's result has type a,
       whose layout is known only from the GADT pattern match at line 2, characters 6-7.
       That match is not exhaustive, so a caller could reach this result at a different layout.
       Function arguments and results must be representable independently of any non-exhaustive match.
|}]

(* Sound: the result sort comes from the annotation, not from the match. *)
let annotated : type (a : any). a t -> unit -> int = fun V () -> 42
[%%expect{|
Line 1, characters 57-58:
1 | let annotated : type (a : any). a t -> unit -> int = fun V () -> 42
                                                             ^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "B"

val annotated : ('a : any). 'a t -> unit -> int = <fun>
|}]
