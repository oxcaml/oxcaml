(* TEST
   expect;
*)

(* Constructor disambiguation under omnidirectional type inference.

   Tests are grouped by the first stage of the implementation at which they
   should behave as intended.

   - Defaulting: the existing default rule is preserved.
   - Always rejected: programs that are ill-typed under every stage,
     whether due to defaulting or otherwise.
   - Stage 1: region-local omnidirectionality (typically within spine)
   - Stage 2: guard-directed defaulting
   - Stage 3: incremental instantiation *)

type t =
  | A
  | B of int

type u =
  | A
  | B of bool
  | C

[%%expect {|
type t = A | B of int
type u = A | B of bool | C
|}]

type t2 = D of t

type u2 = D of u

[%%expect {|
type t2 = D of t
type u2 = D of u
|}]

let unify x y = ignore (x = y)

[%%expect {|
val unify : 'a -> 'a -> unit = <fun>
|}]

let apply f x = f x

let rev_apply x f = f x

[%%expect
{|
val apply : ('a -> 'b) -> 'a -> 'b = <fun>
val rev_apply : 'a -> ('a -> 'b) -> 'b = <fun>
|}]

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Defaulting ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

let d1 = A

[%%expect {|
val d1 : u = A
|}]

let[@warning "+41"] d2 = A

[%%expect
{|
Line 1, characters 25-26:
1 | let[@warning "+41"] d2 = A
                             ^
Warning 41 [ambiguous-name]: "A" belongs to several types: "u" "t".
  The first one was selected. Please disambiguate if this is wrong.

val d2 : u = A
|}]

let d3 = Some A

[%%expect {|
val d3 : u option = Some A
|}]

let d4 = [A; B true]

[%%expect {|
val d4 : u list = [A; B true]
|}]

let d5 x = x, A

[%%expect {|
val d5 : 'a -> 'a * u = <fun>
|}]

let d6 = D A

[%%expect {|
val d6 : u2 = D A
|}]

(* Exhaustiveness checking after defaulting *)
let d7 = function A -> 0 | B _ -> 1

[%%expect
{|
Line 1, characters 9-35:
1 | let d7 = function A -> 0 | B _ -> 1
             ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "C"

val d7 : u -> int = <fun>
|}]

(* No instance learns anything: defaulted at the toplevel *)
let d8 =
  let f = function A -> 0 | B _ -> 1 in
  f

[%%expect
{|
Line 2, characters 10-36:
2 |   let f = function A -> 0 | B _ -> 1 in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "C"

val d8 : u -> int = <fun>
|}]

(* Defaulting in a let rec block *)
let rec d9 () = A

and d10 () = d9 ()

[%%expect {|
val d9 : unit -> u = <fun>
val d10 : unit -> u = <fun>
|}]

(* Defaulting within a local region *)
let d11 =
  let x = A in
  x

[%%expect {|
val d11 : u = A
|}]

(* Defaulting with multiple instances (testing incremental instantiation in
   stage 3) *)
let d12 =
  let x = A in
  x, x

[%%expect {|
val d12 : u * u = (A, A)
|}]

let d13 =
  let x = A in
  Some x, [x]

[%%expect {|
val d13 : u option * u list = (Some A, [A])
|}]

let d14 =
  let x = A in
  fun () -> x

[%%expect {|
val d14 : unit -> u = <fun>
|}]

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~ Always rejected ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

(* The argument does not take part in disambiguation. [B] defaults to [u.B] and
   the argument is then rejected *)
let r1 = B 1

[%%expect
{|
Line 1, characters 11-12:
1 | let r1 = B 1
               ^
Error: The constant "1" has type "int" but an expression was expected of type
         "bool"
|}]

(* ex_10 in the paper: constructors are disambiguated one at a time, not
   jointly. Neither the annotation nor the argument of the inner constructor
   disambiguates [D]. Should fail at every stage *)
let r2 = D (B 1 : t)

[%%expect
{|
Line 1, characters 11-20:
1 | let r2 = D (B 1 : t)
               ^^^^^^^^^
Error: This expression has type "t" but an expression was expected of type "u"
|}]

let r3 = D (B 1)

[%%expect
{|
Line 1, characters 14-15:
1 | let r3 = D (B 1)
                  ^
Error: The constant "1" has type "int" but an expression was expected of type
         "bool"
|}]

let r4 = function D (B 1) -> 0 | _ -> 1

[%%expect
{|
Line 1, characters 23-24:
1 | let r4 = function D (B 1) -> 0 | _ -> 1
                           ^
Error: This pattern matches values of type "int"
       but a pattern was expected which matches values of type "bool"
|}]

(* Static overloading is resolved once for all instances *)
let r5 =
  let f = function A -> 0 | _ -> 1 in
  f (A : t), f (A : u)

[%%expect
{|
Line 3, characters 4-11:
3 |   f (A : t), f (A : u)
        ^^^^^^^
Error: This expression has type "t" but an expression was expected of type "u"
|}]

(* In stage 1, [D] defaults to [u2.D]. In stages 2 & 3, [D] resolves to [t2.D],
   so [C] is checked against [t] instead, which fails since [C] is absent from
   [t] *)
let r6 x =
  let () = unify x (D C) in
  (x : t2)

[%%expect
{|
Line 3, characters 3-4:
3 |   (x : t2)
       ^
Error: The value "x" has type "u2" but an expression was expected of type "t2"
|}]

(* In stages 1 & 2, this fails because [B] is defaulted to [u.B]. In stage 3, it
   fails because [t.B] takes an [int], not a [bool] *)
let r7 =
  let f y = B y in
  (f true : t)

[%%expect
{|
Line 3, characters 3-9:
3 |   (f true : t)
       ^^^^^^
Error: This expression has type "u" but an expression was expected of type "t"
|}]

let r8 x =
  unify x C;
  unify x A;
  unify x (B 1 : t)

[%%expect
{|
Line 4, characters 10-19:
4 |   unify x (B 1 : t)
              ^^^^^^^^^
Error: This expression has type "t" but an expression was expected of type "u"
|}]

let r9 x =
  unify x A;
  unify x 1

[%%expect
{|
Line 3, characters 10-11:
3 |   unify x 1
              ^
Error: The constant "1" has type "int" but an expression was expected of type "u"
|}]

(* [f]'s argument is defaulted to [u]. As a result, the [function] is missing a
   case for [C] and must report a partial-match warning. Additionally, [B] is
   defaulted to [u.B] which takes a [bool], not a [string] *)
let r10 =
  let f = function A -> 0 | B _ -> 1 in
  f A, f (B "s")

[%%expect
{|
Line 2, characters 10-36:
2 |   let f = function A -> 0 | B _ -> 1 in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "C"

Line 3, characters 12-15:
3 |   f A, f (B "s")
                ^^^
Error: This constant has type "string" but an expression was expected of type
         "bool"
|}]

(* In stages 2 & 3, we learn that [A] and [B] resolve to [u.A] and [u.B] resp.
   due to closed world reasoning on [C]. However, [B] takes a [bool], not
   [int] *)
let r11 = [A; B 1; C]

[%%expect
{|
Line 1, characters 16-17:
1 | let r11 = [A; B 1; C]
                    ^
Error: The constant "1" has type "int" but an expression was expected of type
         "bool"
|}]

(* See [c4] *)
let r12 old =
  let g = fun x -> 1 + old (B x) in
  let y1, y2 = g 0, g "hi" in
  ignore (old : t -> int);
  y1, y2

[%%expect
{|
Line 3, characters 17-18:
3 |   let y1, y2 = g 0, g "hi" in
                     ^
Error: The constant "0" has type "int" but an expression was expected of type
         "bool"
|}]

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Stage 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

(* Closed-world reasoning: [C] is not overloaded *)
let a1 = C

[%%expect {|
val a1 : u = C
|}]

(* Omnidirectionality of applications [a2-a4] *)
let a21 = (fun (x : t) -> x) A

[%%expect {|
val a21 : t = A
|}]

(* Principality warning will disappear *)
let a22 = apply (fun (x : t) -> x) A

[%%expect
{|
val a22 : t = A
|}, Principal{|
Line 1, characters 35-36:
1 | let a22 = apply (fun (x : t) -> x) A
                                       ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

val a22 : t = A
|}]

let a31 = A |> fun (x : t) -> x

[%%expect {|
Line 1, characters 19-26:
1 | let a31 = A |> fun (x : t) -> x
                       ^^^^^^^
Error: This pattern matches values of type "t"
       but a pattern was expected which matches values of type "u"
|}]

let a32 = rev_apply A (fun (x : t) -> x)

[%%expect
{|
Line 1, characters 27-34:
1 | let a32 = rev_apply A (fun (x : t) -> x)
                               ^^^^^^^
Error: This pattern matches values of type "t"
       but a pattern was expected which matches values of type "u"
|}]

let a41 = B 1 |> fun (x : t) -> x

[%%expect {|
Line 1, characters 12-13:
1 | let a41 = B 1 |> fun (x : t) -> x
                ^
Error: The constant "1" has type "int" but an expression was expected of type
         "bool"
|}]

let a42 = rev_apply (B 1) (fun (x : t) -> x)

[%%expect
{|
Line 1, characters 23-24:
1 | let a42 = rev_apply (B 1) (fun (x : t) -> x)
                           ^
Error: The constant "1" has type "int" but an expression was expected of type
         "bool"
|}]

(* Omnidirectionality of if-then-else *)

(* Principality warning will disappear *)
let a51 b = if b then (B 1 : t) else A

[%%expect {|
val a51 : bool -> t = <fun>
|}, Principal{|
Line 1, characters 37-38:
1 | let a51 b = if b then (B 1 : t) else A
                                         ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

val a51 : bool -> t = <fun>
|}]

let a52 b = if b then B 1 else (A : t)

[%%expect
{|
Line 1, characters 24-25:
1 | let a52 b = if b then B 1 else (A : t)
                            ^
Error: The constant "1" has type "int" but an expression was expected of type
         "bool"
|}]

(* Right-to-left [let] propagation continues to work *)
let a6 =
  let (B n) = (B 1 : t) in
  n

[%%expect
{|
Lines 2-3, characters 2-3:
2 | ..let (B n) = (B 1 : t) in
3 |   n
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "A"

val a6 : int = 1
|}]

(* Chaining suspended constraints *)
let a7 : t2 = D (B 1)

[%%expect {|
val a7 : t2 = D (B 1)
|}]

let a8 = D (B 1) |> fun (x : t2) -> x

[%%expect
{|
Line 1, characters 14-15:
1 | let a8 = D (B 1) |> fun (x : t2) -> x
                  ^
Error: The constant "1" has type "int" but an expression was expected of type
         "bool"
|}]

(* [A] resolved to [t.A] immediately, matching the current implementation. The
   principality warning should disappear *)
let a9 x = match x with (A : t) -> 0 | B _ -> 1

[%%expect
{|
val a9 : t -> int = <fun>
|}, Principal{|
Line 1, characters 39-40:
1 | let a9 x = match x with (A : t) -> 0 | B _ -> 1
                                           ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

val a9 : t -> int = <fun>
|}]

(* Tests alias expansion during constructor disambiguation *)
type t_alias = t

let a10 = B 1 |> fun (x : t_alias) -> x

[%%expect
{|
type t_alias = t
Line 3, characters 12-13:
3 | let a10 = B 1 |> fun (x : t_alias) -> x
                ^
Error: The constant "1" has type "int" but an expression was expected of type
         "bool"
|}]

module Inline_record = struct
  (* [t3.B] takes an inline record, [u3.B] a [bool] *)
  type t3 =
    | A
    | B of { n : int }

  type u3 =
    | A
    | B of bool
    | C
end

[%%expect
{|
module Inline_record :
  sig type t3 = A | B of { n : int; } type u3 = A | B of bool | C end
|}]

let a11 =
  let open Inline_record in
  B { n = 1 } |> fun (x : t3) -> x

[%%expect
{|
Line 3, characters 4-13:
3 |   B { n = 1 } |> fun (x : t3) -> x
        ^^^^^^^^^
Error: This expression should not be a record, the expected type is "bool"
|}]

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Stage 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

(* Many of the tests in this section ought to work in stage 1. However,
   because they (surprisingly) introduce local regions, they only work
   in stages 2 & 3 *)

(* Omnidirectionality of sequences *)
let b1 x =
  unify x A;
  (x : t)

[%%expect
{|
Line 3, characters 3-4:
3 |   (x : t)
       ^
Error: The value "x" has type "u" but an expression was expected of type "t"
|}]

let b2 x y =
  unify x y;
  unify x A;
  (y : t)

[%%expect
{|
Line 4, characters 3-4:
4 |   (y : t)
       ^
Error: The value "y" has type "u" but an expression was expected of type "t"
|}]

(* In stage 1, [A] is unnecessarily defaulted to [u.A]. Stages 2 & 3 correctly
   delay the constraint *)
let b3 x =
  unify x A;
  let _y = 1 in
  (x : t)

[%%expect
{|
Line 4, characters 3-4:
4 |   (x : t)
       ^
Error: The value "x" has type "u" but an expression was expected of type "t"
|}]

let b4 x =
  let () = unify x A in
  (x : t)

[%%expect
{|
Line 3, characters 3-4:
3 |   (x : t)
       ^
Error: The value "x" has type "u" but an expression was expected of type "t"
|}]

let b5 x =
  unify x A;
  let id y = y in
  (id x : t)

[%%expect
{|
Line 4, characters 3-7:
4 |   (id x : t)
       ^^^^
Error: This expression has type "u" but an expression was expected of type "t"
|}]

let b6 x =
  unify x A;
  let y = x in
  (y : t)

[%%expect
{|
Line 4, characters 3-4:
4 |   (y : t)
       ^
Error: The value "y" has type "u" but an expression was expected of type "t"
|}]

(* Omnidirectionality of matches [b7-b11] *)

(* Suspending on the matchee *)
let b7 = match B 1 with (x : t) -> x

[%%expect
{|
Line 1, characters 17-18:
1 | let b7 = match B 1 with (x : t) -> x
                     ^
Error: The constant "1" has type "int" but an expression was expected of type
         "bool"
|}]

(* Suspending in patterns (cf. [a9] for the symmetric case) *)
let b8 x = match x with A -> 0 | (B _ : t) -> 1

[%%expect
{|
Line 1, characters 33-42:
1 | let b8 x = match x with A -> 0 | (B _ : t) -> 1
                                     ^^^^^^^^^
Error: This pattern matches values of type "t"
       but a pattern was expected which matches values of type "u"
|}]

(* Deeply nested suspensions *)
let b9 x y = match x, y with A, A -> 0 | (B _ : t), C -> 1 | _ -> 2

[%%expect
{|
Line 1, characters 41-50:
1 | let b9 x y = match x, y with A, A -> 0 | (B _ : t), C -> 1 | _ -> 2
                                             ^^^^^^^^^
Error: This pattern matches values of type "t"
       but a pattern was expected which matches values of type "u"
|}]

(* Closed-world reasoning in patterns: [C] only belongs to [u], which resolves
   its siblings. Should not warn *)
let[@warning "+41"] b10 = function A -> 0 | C -> 1 | B _ -> 2

[%%expect
{|
Line 1, characters 35-36:
1 | let[@warning "+41"] b10 = function A -> 0 | C -> 1 | B _ -> 2
                                       ^
Warning 41 [ambiguous-name]: "A" belongs to several types: "u" "t".
  The first one was selected. Please disambiguate if this is wrong.

val b10 : u -> int = <fun>
|}, Principal{|
Line 1, characters 35-36:
1 | let[@warning "+41"] b10 = function A -> 0 | C -> 1 | B _ -> 2
                                       ^
Warning 41 [ambiguous-name]: "A" belongs to several types: "u" "t".
  The first one was selected. Please disambiguate if this is wrong.

Line 1, characters 53-54:
1 | let[@warning "+41"] b10 = function A -> 0 | C -> 1 | B _ -> 2
                                                         ^
Warning 41 [ambiguous-name]: "B" belongs to several types: "u" "t".
  The first one was selected. Please disambiguate if this is wrong.

val b10 : u -> int = <fun>
|}]

(* Annotation in a sibling branch body *)
let b11 x = match x with A -> x | B _ -> (x : t)

[%%expect
{|
Line 1, characters 42-43:
1 | let b11 x = match x with A -> x | B _ -> (x : t)
                                              ^
Error: The value "x" has type "u" but an expression was expected of type "t"
|}]

(* Omnidirectionality of let bindings *)

(* ocaml/ocaml#7388: the annotation on a let-pattern is not used to type the
   let-bound expression *)
let b12 =
  let (B n : t) = B 1 in
  n

[%%expect
{|
Line 2, characters 20-21:
2 |   let (B n : t) = B 1 in
                        ^
Error: The constant "1" has type "int" but an expression was expected of type
         "bool"
|}]

let b13 ((A | B _) as x) = (x : t)

[%%expect
{|
Line 1, characters 28-29:
1 | let b13 ((A | B _) as x) = (x : t)
                                ^
Error: The value "x" has type "u" but an expression was expected of type "t"
|}]

(* ex_4 in the paper: the constraint is in the let-definition, the information
   in the let-body. Here [x] is an {e old} variable *)
let b14 x =
  let y = match x with A -> 0 | B _ -> 1 in
  y + match (x : t) with _ -> 0

[%%expect
{|
Line 2, characters 10-40:
2 |   let y = match x with A -> 0 | B _ -> 1 in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "C"

Line 3, characters 13-14:
3 |   y + match (x : t) with _ -> 0
                 ^
Error: The value "x" has type "u" but an expression was expected of type "t"
|}]

module Three_way = struct
  type v =
    | A
    | B of string
    | E
end

[%%expect {|
module Three_way : sig type v = A | B of string | E end
|}]

(* Partial-match after resolving a suspended constraint. Doesn't
   take the defaulting path. *)
let b15 x =
  let open Three_way in
  match x with A -> 0 | (B _ : u) -> 1

[%%expect
{|
Line 3, characters 24-33:
3 |   match x with A -> 0 | (B _ : u) -> 1
                            ^^^^^^^^^
Error: This pattern matches values of type "u"
       but a pattern was expected which matches values of type "Three_way.v"
|}]

module Cycle = struct
  (* This module tests an explicit cycle of suspended constraints involved in
     constructor disambiguation *)

  type tx = Foo of ty

  and ty = Foo of tx
end

[%%expect {|
module Cycle : sig type tx = Foo of ty and ty = Foo of tx end
|}]

(* Okay: [x] resolves to [tx] and [y] to [ty] *)
let b18 x y =
  let open Cycle in
  unify x (Foo y);
  unify y (Foo x);
  (x : tx)

[%%expect
{|
Line 5, characters 3-4:
5 |   (x : tx)
       ^
Error: The value "x" has type "Cycle.ty" but an expression was expected of type
         "Cycle.tx"
|}, Principal{|
Line 4, characters 11-14:
4 |   unify y (Foo x);
               ^^^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

Line 5, characters 3-4:
5 |   (x : tx)
       ^
Error: The value "x" has type "Cycle.ty" but an expression was expected of type
         "Cycle.tx"
|}]

(* Also okay: [x] resolves to [ty] and [y] to [tx] (symmetric to [b18]) *)
let b19 x y =
  let open Cycle in
  unify x (Foo y);
  unify y (Foo x);
  (x : ty)

[%%expect
{|
val b19 : Cycle.ty -> Cycle.tx -> Cycle.ty = <fun>
|}, Principal{|
Line 4, characters 11-14:
4 |   unify y (Foo x);
               ^^^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

val b19 : Cycle.ty -> Cycle.tx -> Cycle.ty = <fun>
|}]

(* This is a breaking change. We expect defaulting to fail here.

   The current constructor disambiguation implementation succeeds by using a
   lexical defaulting order: [Foo y] is defaulted to [ty.Foo]. As a result, [x]
   resolves to [ty] and [y] to [tx].

   The proposed defaulting implementation would default [Foo y] and [Foo x]
   simultaneously, both to [ty.Foo]. As a result, [x] and [y] must both resolve
   to [tx] and [ty], which leads to a type error *)
let b20 x y =
  let open Cycle in
  unify x (Foo y);
  unify y (Foo x)

[%%expect
{|
val b20 : Cycle.ty -> Cycle.tx -> unit = <fun>
|}, Principal{|
Line 4, characters 11-14:
4 |   unify y (Foo x)
               ^^^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

val b20 : Cycle.ty -> Cycle.tx -> unit = <fun>
|}]

let b21 =
  let open Inline_record in
  function B { n = _ } -> 42 | (A : t3) -> 0

[%%expect
{|
Line 3, characters 13-22:
3 |   function B { n = _ } -> 42 | (A : t3) -> 0
                 ^^^^^^^^^
Error: This pattern should not be a record, the expected type is "bool"
|}]

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Stage 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

(* Suspended patterns that bind variables. The variable types will be
   generalized in [map_half_typed_cases] *)

let c1 x = match x with B n -> n + 1 | (A : t) -> 0

[%%expect
{|
Line 1, characters 39-46:
1 | let c1 x = match x with B n -> n + 1 | (A : t) -> 0
                                           ^^^^^^^
Error: This pattern matches values of type "t"
       but a pattern was expected which matches values of type "u"
|}]

let c2 x = match x with B n when n > 0 -> true | (_ : t) -> false

[%%expect
{|
Line 1, characters 49-56:
1 | let c2 x = match x with B n when n > 0 -> true | (_ : t) -> false
                                                     ^^^^^^^
Error: This pattern matches values of type "t"
       but a pattern was expected which matches values of type "u"
|}]

(* [D]'s argument is generalizable in [y]'s region *)
let c3 x =
  let y = unify x (D A) in
  ignore (x : t2);
  y

[%%expect
{|
Line 3, characters 10-11:
3 |   ignore (x : t2);
              ^
Error: The value "x" has type "u2" but an expression was expected of type "t2"
|}]

(* [g]'s type will contain a partially generic [x] which is only resolved to
   [int] after [old] is unified with [t -> int] (c.f. [r12] for the bad case) *)
let c4 old =
  let g = fun x -> 1 + old (B x) in
  let y = g 0 in
  ignore (old : t -> int);
  y

[%%expect
{|
Line 3, characters 12-13:
3 |   let y = g 0 in
                ^
Error: The constant "0" has type "int" but an expression was expected of type
         "bool"
|}]

(* Backpropagation: information flows from an instance back to its
   let-definition *)

(* In stages 1 & 2, [A] is defaulted to [u.A]. In stage 3, backpropagation
   occurs and the example typechecks *)
let c6 =
  let x = A in
  (x : t)

[%%expect
{|
Line 3, characters 3-4:
3 |   (x : t)
       ^
Error: The value "x" has type "u" but an expression was expected of type "t"
|}]

(* ex_11 in the paper *)
let c7 =
  let f = function A -> 0 | B _ -> 1 in
  f (B 1 : t)

[%%expect
{|
Line 2, characters 10-36:
2 |   let f = function A -> 0 | B _ -> 1 in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "C"

Line 3, characters 4-13:
3 |   f (B 1 : t)
        ^^^^^^^^^
Error: This expression has type "t" but an expression was expected of type "u"
|}]

(* The definition is ill-typed under the default, so it must not be defaulted
   before backpropagation *)
let c8 =
  let f = function A -> 0 | B n -> n in
  f (A : t)

[%%expect
{|
Line 2, characters 35-36:
2 |   let f = function A -> 0 | B n -> n in
                                       ^
Error: The value "n" has type "bool" but an expression was expected of type "int"
|}]

(* The instance's type is a variable when the instance is taken, and is only
   learnt later *)
let c9 =
  let f = function A -> 0 | B _ -> 1 in
  fun y ->
    let r = f y in
    ignore (y : t);
    r

[%%expect
{|
Line 2, characters 10-36:
2 |   let f = function A -> 0 | B _ -> 1 in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "C"

Line 5, characters 12-13:
5 |     ignore (y : t);
                ^
Error: The value "y" has type "u" but an expression was expected of type "t"
|}]

(* One instance is unconstrained, the other is known. Backpropagation from the
   second updates the first *)
let c10 =
  let f = function A -> 0 | B _ -> 1 in
  f, (f : t -> int)

[%%expect
{|
Line 2, characters 10-36:
2 |   let f = function A -> 0 | B _ -> 1 in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "C"

Line 3, characters 6-7:
3 |   f, (f : t -> int)
          ^
Error: The value "f" has type "u -> int" but an expression was expected of type
         "t -> int"
       Type "u" is not compatible with type "t"
|}]

(* The argument [A] of the first instance is only resolved once the second
   instance has backpropagated *)
let c11 =
  let f = function A -> 0 | B _ -> 1 in
  f A, f (B 1 : t)

[%%expect
{|
Line 2, characters 10-36:
2 |   let f = function A -> 0 | B _ -> 1 in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "C"

Line 3, characters 9-18:
3 |   f A, f (B 1 : t)
             ^^^^^^^^^
Error: This expression has type "t" but an expression was expected of type "u"
|}]

(* Through two partial type schemes *)
let c12 =
  let f = function A -> 0 | B _ -> 1 in
  let g x = f x in
  g (B 1 : t)

[%%expect
{|
Line 2, characters 10-36:
2 |   let f = function A -> 0 | B _ -> 1 in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "C"

Line 4, characters 4-13:
4 |   g (B 1 : t)
        ^^^^^^^^^
Error: This expression has type "t" but an expression was expected of type "u"
|}]

(* Backpropagation to a non-root variable of the scheme. ['a] stays generic *)
let c13 =
  let f y = y, A in
  f 1, (f true : bool * t)

[%%expect
{|
Line 3, characters 8-14:
3 |   f 1, (f true : bool * t)
            ^^^^^^
Error: This expression has type "bool * u"
       but an expression was expected of type "bool * t"
       Type "u" is not compatible with type "t"
|}]

(* Once backpropagation resolves [B], its argument type must reach the
   instance *)
let c14 =
  let f y = B y in
  (f 1 : t)

[%%expect
{|
Line 3, characters 5-6:
3 |   (f 1 : t)
         ^
Error: The constant "1" has type "int" but an expression was expected of type
         "bool"
|}]

(* The instance is unified with an outer variable that learns the type later *)
let c15 z =
  let x = A in
  unify z x;
  (z : t)

[%%expect
{|
Line 4, characters 3-4:
4 |   (z : t)
       ^
Error: The value "z" has type "u" but an expression was expected of type "t"
|}]

module Parameterized = struct
  (* ex_8 in the paper: a parameterized overloaded constructor. [w] is declared
     last so that [pt] is not the default *)
  type 'a pt =
    | A
    | B of 'a

  type w =
    | A
    | B of bool
end

[%%expect {|
module Parameterized :
  sig type 'a pt = A | B of 'a type w = A | B of bool end
|}]

(* Backpropagation from the first instance selects [pt]; the second instance
   uses a different ['a] *)
let c16 gp =
  let open Parameterized in
  let getb = function B x -> x | _ -> assert false in
  getb (B 42 : int pt), (getb gp : float)

[%%expect
{|
Line 4, characters 7-22:
4 |   getb (B 42 : int pt), (getb gp : float)
           ^^^^^^^^^^^^^^^
Error: This expression has type "int Parameterized.pt"
       but an expression was expected of type "Parameterized.w"
|}]

let c17 =
  let open Parameterized in
  let getb = function B x -> x | _ -> assert false in
  getb (B 1 : int pt), getb (B "s" : string pt)

[%%expect
{|
Line 4, characters 7-21:
4 |   getb (B 1 : int pt), getb (B "s" : string pt)
           ^^^^^^^^^^^^^^
Error: This expression has type "int Parameterized.pt"
       but an expression was expected of type "Parameterized.w"
|}]
