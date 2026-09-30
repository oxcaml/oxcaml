(* TEST
   expect;
*)

(* Constructor disambiguation under omndirectional type inference *)

type t = A | B of int
type u = A | B of bool | C
[%%expect {|
type t = A | B of int
type u = A | B of bool | C
|}];;


(* Stage 1 good defaulting (i.e top-level defaults work) *)

let a1 = A;;
[%%expect {|
val a1 : u = A
|}];;

let[@warning "+41"] a2 = A;;
[%%expect {|
Line 1, characters 25-26:
1 | let[@warning "+41"] a2 = A;;
                             ^
Warning 41 [ambiguous-name]: "A" belongs to several types: "u" "t".
  The first one was selected. Please disambiguate if this is wrong.

val a2 : u = A
|}];;

(* The argument does not take part in disambiguation.
   [B] defaults to [u.B] and the argument is then rejected. *)
let a3 = B 1;;
[%%expect {|
Line 1, characters 11-12:
1 | let a3 = B 1;;
               ^
Error: The constant "1" has type "int" but an expression was expected of type
         "bool"
|}];;

let a4 = C;;
[%%expect {|
val a4 : u = C
|}];;

let a5 = Some A;;
[%%expect {|
val a5 : u option = Some A
|}];;

let a6 = [A; B true];;
[%%expect {|
val a6 : u list = [A; B true]
|}];;

let a8 x = (x, A);;
[%%expect {|
val a8 : 'a -> 'a * u = <fun>
|}];;

(* Omnidirectional application *)

(* Currently the compiler will default [A] to [u.A]. *)
let apply f x = f x;;
let rev_apply x f = f x;;
[%%expect {|
val apply : ('a -> 'b) -> 'a -> 'b = <fun>
val rev_apply : 'a -> ('a -> 'b) -> 'b = <fun>
|}];;

let b11 = (fun (x : t) -> x) A;;
let b12 = apply (fun (x : t) -> x) A;;
[%%expect {|
val b11 : t = A
val b12 : t = A
|}, Principal{|
val b11 : t = A
Line 2, characters 35-36:
2 | let b12 = apply (fun (x : t) -> x) A;;
                                       ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

val b12 : t = A
|}];;

let b21 = A |> (fun (x : t) -> x);;
let b22 = rev_apply A (fun (x : t) -> x);;
[%%expect {|
Line 1, characters 20-27:
1 | let b21 = A |> (fun (x : t) -> x);;
                        ^^^^^^^
Error: This pattern matches values of type "t"
       but a pattern was expected which matches values of type "u"
|}];;

let b31 = B 1 |> (fun (x : t) -> x);;
let b32 = rev_apply (B 1) (fun (x : t) -> x);;
[%%expect {|
Line 1, characters 12-13:
1 | let b31 = B 1 |> (fun (x : t) -> x);;
                ^
Error: The constant "1" has type "int" but an expression was expected of type
         "bool"
|}];;

let unify x y = ignore (x = y);;
[%%expect {|
val unify : 'a -> 'a -> unit = <fun>
|}]

let b4 x = unify x A; (x : t);;
[%%expect {|
Line 1, characters 23-24:
1 | let b4 x = unify x A; (x : t);;
                           ^
Error: The value "x" has type "u" but an expression was expected of type "t"
|}];;

let b5 b = if b then B 1 else (A : t);;
[%%expect {|
Line 1, characters 23-24:
1 | let b5 b = if b then B 1 else (A : t);;
                           ^
Error: The constant "1" has type "int" but an expression was expected of type
         "bool"
|}];;

let b6 = match B 1 with (x : t) -> x;;
[%%expect {|
Line 1, characters 17-18:
1 | let b6 = match B 1 with (x : t) -> x;;
                     ^
Error: The constant "1" has type "int" but an expression was expected of type
         "bool"
|}];;

let b7 = [A; B 1; C];;
[%%expect {|
Line 1, characters 15-16:
1 | let b7 = [A; B 1; C];;
                   ^
Error: The constant "1" has type "int" but an expression was expected of type
         "bool"
|}];;

(* ocaml/ocaml#7388: the annotation on a let-pattern is not used to type
   the let-bound expression. *)
let b8 = let (B n : t) = B 1 in n;;
[%%expect {|
Line 1, characters 27-28:
1 | let b8 = let (B n : t) = B 1 in n;;
                               ^
Error: The constant "1" has type "int" but an expression was expected of type
         "bool"
|}];;

let b9 = let B n = (B 1 : t) in n;;
[%%expect {|
Line 1, characters 9-33:
1 | let b9 = let B n = (B 1 : t) in n;;
             ^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "A"

val b9 : int = 1
|}];;

(* Chaining of suspended constraints *)
type t2 = D of t;;
type u2 = D of u;;
[%%expect {|
type t2 = D of t
type u2 = D of u
|}];;

let c1 = D A;;
[%%expect {|
val c1 : u2 = D A
|}];;

let c2 = D (B 1);;
[%%expect {|
Line 1, characters 14-15:
1 | let c2 = D (B 1);;
                  ^
Error: The constant "1" has type "int" but an expression was expected of type
         "bool"
|}]

let c3 : t2 = D (B 1);;
[%%expect {|
val c3 : t2 = D (B 1)
|}];;

let c4 = D (B 1) |> (fun (x : t2) -> x);;
[%%expect {|
Line 1, characters 14-15:
1 | let c4 = D (B 1) |> (fun (x : t2) -> x);;
                  ^
Error: The constant "1" has type "int" but an expression was expected of type
         "bool"
|}];;

(* ex_10 in the paper: constructors are disambiguated one at a time,
   not jointly. Neither the annotation nor the argument of the inner
   constructor disambiguates [D]. Should fail at every stage. *)
let c6 = D (B 1 : t);;
[%%expect {|
Line 1, characters 11-20:
1 | let c6 = D (B 1 : t);;
               ^^^^^^^^^
Error: This expression has type "t" but an expression was expected of type "u"
|}];;

let c7 = function D (B 1) -> 0 | _ -> 1;;
[%%expect {|
Line 1, characters 23-24:
1 | let c7 = function D (B 1) -> 0 | _ -> 1;;
                           ^
Error: This pattern matches values of type "int"
       but a pattern was expected which matches values of type "bool"
|}];;

let c5 x y = unify x y; unify x A; (y : t);;
[%%expect {|
Line 1, characters 36-37:
1 | let c5 x y = unify x y; unify x A; (y : t);;
                                        ^
Error: The value "y" has type "u" but an expression was expected of type "t"
|}];;

(* Exhaustiveness checking *)

(* Should fail. We could mimic the notion of 'pressure' from polymorphic variants.
   But this would be complex and not worth it imo *)
let d1 = function A -> 0 | B _ -> 1;;
[%%expect {|
Line 1, characters 9-35:
1 | let d1 = function A -> 0 | B _ -> 1;;
             ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "C"

val d1 : u -> int = <fun>
|}];;


let d2 x = match x with (A : t) -> 0 | B _ -> 1;;
[%%expect {|
val d2 : t -> int = <fun>
|}, Principal{|
Line 1, characters 39-40:
1 | let d2 x = match x with (A : t) -> 0 | B _ -> 1;;
                                           ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

val d2 : t -> int = <fun>
|}];;

let d3 x = match x with A -> 0 | (B _ : t) -> 1;;
[%%expect {|
Line 1, characters 33-42:
1 | let d3 x = match x with A -> 0 | (B _ : t) -> 1;;
                                     ^^^^^^^^^
Error: This pattern matches values of type "t"
       but a pattern was expected which matches values of type "u"
|}];;

(* No eager decisions on the argument type *)
let d4 x = match x with B n -> n + 1 | (A : t) -> 0;;
[%%expect {|
Line 1, characters 39-46:
1 | let d4 x = match x with B n -> n + 1 | (A : t) -> 0;;
                                           ^^^^^^^
Error: This pattern matches values of type "t"
       but a pattern was expected which matches values of type "u"
|}];;

let d5 x = match x with B n when n > 0 -> true | (_ : t) -> false;;
[%%expect {|
Line 1, characters 49-56:
1 | let d5 x = match x with B n when n > 0 -> true | (_ : t) -> false;;
                                                     ^^^^^^^
Error: This pattern matches values of type "t"
       but a pattern was expected which matches values of type "u"
|}];;

let d6 (A | B _ as x) = (x : t);;
[%%expect {|
Line 1, characters 25-26:
1 | let d6 (A | B _ as x) = (x : t);;
                             ^
Error: The value "x" has type "u" but an expression was expected of type "t"
|}];;

let d7 x y = match x, y with A, A -> 0 | ((B _ : t), C) -> 1 | _ -> 2;;
[%%expect {|
Line 1, characters 42-51:
1 | let d7 x y = match x, y with A, A -> 0 | ((B _ : t), C) -> 1 | _ -> 2;;
                                              ^^^^^^^^^
Error: This pattern matches values of type "t"
       but a pattern was expected which matches values of type "u"
|}];;

(* Closed-world reasoning in patterns: [C] only belongs to [u], which
   resolves its siblings. Should not warn. *)
let[@warning "+41"] d8 = function A -> 0 | C -> 1 | B _ -> 2;;
[%%expect {|
Line 1, characters 34-35:
1 | let[@warning "+41"] d8 = function A -> 0 | C -> 1 | B _ -> 2;;
                                      ^
Warning 41 [ambiguous-name]: "A" belongs to several types: "u" "t".
  The first one was selected. Please disambiguate if this is wrong.

val d8 : u -> int = <fun>
|}, Principal{|
Line 1, characters 34-35:
1 | let[@warning "+41"] d8 = function A -> 0 | C -> 1 | B _ -> 2;;
                                      ^
Warning 41 [ambiguous-name]: "A" belongs to several types: "u" "t".
  The first one was selected. Please disambiguate if this is wrong.

Line 1, characters 52-53:
1 | let[@warning "+41"] d8 = function A -> 0 | C -> 1 | B _ -> 2;;
                                                        ^
Warning 41 [ambiguous-name]: "B" belongs to several types: "u" "t".
  The first one was selected. Please disambiguate if this is wrong.

val d8 : u -> int = <fun>
|}];;

(* Annotation in a sibling branch body. *)
let d9 x = match x with A -> (x : t) | B _ -> x;;
[%%expect {|
Line 1, characters 30-31:
1 | let d9 x = match x with A -> (x : t) | B _ -> x;;
                                  ^
Error: The value "x" has type "u" but an expression was expected of type "t"
|}];;

(* Errors *)
let e1 x = unify x C; unify x A; unify x (B 1 : t);;
[%%expect {|
Line 1, characters 41-50:
1 | let e1 x = unify x C; unify x A; unify x (B 1 : t);;
                                             ^^^^^^^^^
Error: This expression has type "t" but an expression was expected of type "u"
|}];;

let e2 x = unify x A; unify x 1;;
[%%expect {|
Line 1, characters 30-31:
1 | let e2 x = unify x A; unify x 1;;
                                  ^
Error: The constant "1" has type "int" but an expression was expected of type "u"
|}];;

(* Stage 1 defaulting *)

let rec f1 () = A
and f2 () = f1 ()
;;
[%%expect {|
val f1 : unit -> u = <fun>
val f2 : unit -> u = <fun>
|}];;

let f2 = let x = A in x;;
[%%expect {|
val f2 : u = A
|}];;

(* Fails. [A] is defaulted to [u.A] *)
let f3 = let x = A in (x : t);;
[%%expect {|
Line 1, characters 23-24:
1 | let f3 = let x = A in (x : t);;
                           ^
Error: The value "x" has type "u" but an expression was expected of type "t"
|}];;

(* Defaults [x] unnecessarily to [u.A] *)
let f4 x = unify x A; let y = 1 in (x : t);;
[%%expect {|
Line 1, characters 36-37:
1 | let f4 x = unify x A; let y = 1 in (x : t);;
                                        ^
Error: The value "x" has type "u" but an expression was expected of type "t"
|}];;

let f5 x = let () = unify x A in (x : t);;
[%%expect {|
Line 1, characters 34-35:
1 | let f5 x = let () = unify x A in (x : t);;
                                      ^
Error: The value "x" has type "u" but an expression was expected of type "t"
|}];;

(* Defaulting a generalizable inner [let]. Stages 1 and 2 default at the
   inner [let]; Stage 3 defaults at the toplevel, and the default must
   reach every instance. *)
let f6 = let x = A in (x, x);;
[%%expect {|
val f6 : u * u = (A, A)
|}];;

let f7 = let x = A in (Some x, [x]);;
[%%expect {|
val f7 : u option * u list = (Some A, [A])
|}];;

let f8 = let x = A in fun () -> x;;
[%%expect {|
val f8 : unit -> u = <fun>
|}];;

(* The partial-match warning must still be reported at the [function]. *)
let f9 = let f = function A -> 0 | B _ -> 1 in (f A, f (B "s"));;
[%%expect {|
Line 1, characters 17-43:
1 | let f9 = let f = function A -> 0 | B _ -> 1 in (f A, f (B "s"));;
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "C"

Line 1, characters 58-61:
1 | let f9 = let f = function A -> 0 | B _ -> 1 in (f A, f (B "s"));;
                                                              ^^^
Error: This constant has type "string" but an expression was expected of type
         "bool"
|}];;

(* Stage 2 defaulting *)

let g1 x = unify x A; let y = 1 in (x : t);;
[%%expect {|
Line 1, characters 36-37:
1 | let g1 x = unify x A; let y = 1 in (x : t);;
                                        ^
Error: The value "x" has type "u" but an expression was expected of type "t"
|}];;

let g2 x = let () = unify x A in (x : t);;
[%%expect {|
Line 1, characters 34-35:
1 | let g2 x = let () = unify x A in (x : t);;
                                      ^
Error: The value "x" has type "u" but an expression was expected of type "t"
|}];;

let g3 x = unify x A; let id y = y in (id x : t);;
[%%expect {|
Line 1, characters 39-43:
1 | let g3 x = unify x A; let id y = y in (id x : t);;
                                           ^^^^
Error: This expression has type "u" but an expression was expected of type "t"
|}];;

let g4 x = unify x A; let y = x in (y : t);;
[%%expect {|
Line 1, characters 36-37:
1 | let g4 x = unify x A; let y = x in (y : t);;
                                        ^
Error: The value "y" has type "u" but an expression was expected of type "t"
|}];;

(* ex_4 in the paper: the constraint is in the let-definition, the
   information in the let-body. Here [x] is an {e old} variable. *)
let g5 x = let y = match x with A -> 0 | B _ -> 1 in y + (match (x : t) with _ -> 0);;
[%%expect {|
Line 1, characters 19-49:
1 | let g5 x = let y = match x with A -> 0 | B _ -> 1 in y + (match (x : t) with _ -> 0);;
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "C"

Line 1, characters 65-66:
1 | let g5 x = let y = match x with A -> 0 | B _ -> 1 in y + (match (x : t) with _ -> 0);;
                                                                     ^
Error: The value "x" has type "u" but an expression was expected of type "t"
|}];;

(* Stage 3 generalization *)

let h1 x = let y = unify x (D A) in ignore (x : t2); y;;
[%%expect {|
Line 1, characters 44-45:
1 | let h1 x = let y = unify x (D A) in ignore (x : t2); y;;
                                                ^
Error: The value "x" has type "u2" but an expression was expected of type "t2"
|}];;

let h2 x = let () = unify x (D C) in (x : t2);;
[%%expect {|
Line 1, characters 38-39:
1 | let h2 x = let () = unify x (D C) in (x : t2);;
                                          ^
Error: The value "x" has type "u2" but an expression was expected of type "t2"
|}];;

let h3 old =
  let g = fun x -> 1 + old (B x) in
  let y = g 0 in
  ignore (old : t -> int);
  y
;;
[%%expect {|
Line 3, characters 12-13:
3 |   let y = g 0 in
                ^
Error: The constant "0" has type "int" but an expression was expected of type
         "bool"
|}];;

(* Stage 3 backpropagation: information flows from an instance back to
   its let-definition. *)

(* ex_11 in the paper. *)
let i1 = let f = function A -> 0 | B _ -> 1 in f (B 1 : t);;
[%%expect {|
Line 1, characters 17-43:
1 | let i1 = let f = function A -> 0 | B _ -> 1 in f (B 1 : t);;
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "C"

Line 1, characters 49-58:
1 | let i1 = let f = function A -> 0 | B _ -> 1 in f (B 1 : t);;
                                                     ^^^^^^^^^
Error: This expression has type "t" but an expression was expected of type "u"
|}];;

(* The definition is ill-typed under the default, so it must not be
   defaulted before backpropagation. *)
let i2 = let f = function A -> 0 | B n -> n in f (A : t);;
[%%expect {|
Line 1, characters 42-43:
1 | let i2 = let f = function A -> 0 | B n -> n in f (A : t);;
                                              ^
Error: The value "n" has type "bool" but an expression was expected of type "int"
|}];;

(* The instance's type is a variable when the instance is taken, and is
   only learnt later. *)
let i3 = let f = function A -> 0 | B _ -> 1 in fun y -> let r = f y in ignore (y : t); r;;
[%%expect {|
Line 1, characters 17-43:
1 | let i3 = let f = function A -> 0 | B _ -> 1 in fun y -> let r = f y in ignore (y : t); r;;
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "C"

Line 1, characters 79-80:
1 | let i3 = let f = function A -> 0 | B _ -> 1 in fun y -> let r = f y in ignore (y : t); r;;
                                                                                   ^
Error: The value "y" has type "u" but an expression was expected of type "t"
|}];;

(* One instance is unconstrained, the other is known. Backpropagation
   from the second updates the first. *)
let i4 = let f = function A -> 0 | B _ -> 1 in (f, (f : t -> int));;
[%%expect {|
Line 1, characters 17-43:
1 | let i4 = let f = function A -> 0 | B _ -> 1 in (f, (f : t -> int));;
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "C"

Line 1, characters 52-53:
1 | let i4 = let f = function A -> 0 | B _ -> 1 in (f, (f : t -> int));;
                                                        ^
Error: The value "f" has type "u -> int" but an expression was expected of type
         "t -> int"
       Type "u" is not compatible with type "t"
|}];;

(* The argument [A] of the first instance is only resolved once the
   second instance has backpropagated. *)
let i5 = let f = function A -> 0 | B _ -> 1 in (f A, f (B 1 : t));;
[%%expect {|
Line 1, characters 17-43:
1 | let i5 = let f = function A -> 0 | B _ -> 1 in (f A, f (B 1 : t));;
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "C"

Line 1, characters 55-64:
1 | let i5 = let f = function A -> 0 | B _ -> 1 in (f A, f (B 1 : t));;
                                                           ^^^^^^^^^
Error: This expression has type "t" but an expression was expected of type "u"
|}];;

(* Through two partial type schemes. *)
let i6 = let f = function A -> 0 | B _ -> 1 in let g x = f x in g (B 1 : t);;
[%%expect {|
Line 1, characters 17-43:
1 | let i6 = let f = function A -> 0 | B _ -> 1 in let g x = f x in g (B 1 : t);;
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "C"

Line 1, characters 66-75:
1 | let i6 = let f = function A -> 0 | B _ -> 1 in let g x = f x in g (B 1 : t);;
                                                                      ^^^^^^^^^
Error: This expression has type "t" but an expression was expected of type "u"
|}];;

(* Backpropagation to a non-root variable of the scheme. ['a] stays
   generic. *)
let i7 = let f y = (y, A) in (f 1, (f true : bool * t));;
[%%expect {|
Line 1, characters 36-42:
1 | let i7 = let f y = (y, A) in (f 1, (f true : bool * t));;
                                        ^^^^^^
Error: This expression has type "bool * u"
       but an expression was expected of type "bool * t"
       Type "u" is not compatible with type "t"
|}];;

(* Once backpropagation resolves [B], its argument type must reach the
   instance. *)
let i8 = let f y = B y in (f 1 : t);;
[%%expect {|
Line 1, characters 29-30:
1 | let i8 = let f y = B y in (f 1 : t);;
                                 ^
Error: The constant "1" has type "int" but an expression was expected of type
         "bool"
|}];;

(* Check that the argument type is correctly propagated *)
let i9 = let f y = B y in (f true : t);;
[%%expect {|
Line 1, characters 27-33:
1 | let i9 = let f y = B y in (f true : t);;
                               ^^^^^^
Error: This expression has type "u" but an expression was expected of type "t"
|}];;

(* The instance is unified with an outer variable that learns the type
   later. *)
let i10 z = let x = A in unify z x; (z : t);;
[%%expect {|
Line 1, characters 37-38:
1 | let i10 z = let x = A in unify z x; (z : t);;
                                         ^
Error: The value "z" has type "u" but an expression was expected of type "t"
|}];;

(* Should fail at every stage: static overloading is resolved once for
   all instances. *)
let i11 = let f = function A -> 0 | B _ -> 1 in (f (A : t), f (A : u));;
[%%expect {|
Line 1, characters 18-44:
1 | let i11 = let f = function A -> 0 | B _ -> 1 in (f (A : t), f (A : u));;
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "C"

Line 1, characters 51-58:
1 | let i11 = let f = function A -> 0 | B _ -> 1 in (f (A : t), f (A : u));;
                                                       ^^^^^^^
Error: This expression has type "t" but an expression was expected of type "u"
|}];;

(* No instance learns anything: defaulted at the toplevel. *)
let i12 = let f = function A -> 0 | B _ -> 1 in f;;
[%%expect {|
Line 1, characters 18-44:
1 | let i12 = let f = function A -> 0 | B _ -> 1 in f;;
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "C"

val i12 : u -> int = <fun>
|}];;

(* Some nasty edge cases *)

type t_alias = t;;
let z1 = B 1 |> (fun (x : t_alias) -> x);;
[%%expect {|
type t_alias = t
Line 2, characters 11-12:
2 | let z1 = B 1 |> (fun (x : t_alias) -> x);;
               ^
Error: The constant "1" has type "int" but an expression was expected of type
         "bool"
|}];;

type x = Foo of y
and y = Foo of x;;
[%%expect {|
type x = Foo of y
and y = Foo of x
|}];;

(* Constructs a cycle of suspended constraints between x and y. *)
let z2 x y = unify x (Foo y); unify y (Foo x); (x : x);;
[%%expect {|
val z2 : x -> y -> x = <fun>
|}, Principal{|
Line 1, characters 39-42:
1 | let z2 x y = unify x (Foo y); unify y (Foo x); (x : x);;
                                           ^^^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

val z2 : x -> y -> x = <fun>
|}];;

let z3 x y = unify x (Foo y); unify y (Foo x); (x : y);;
[%%expect {|
Line 1, characters 48-49:
1 | let z3 x y = unify x (Foo y); unify y (Foo x); (x : y);;
                                                    ^
Error: The value "x" has type "x" but an expression was expected of type "y"
|}, Principal{|
Line 1, characters 39-42:
1 | let z3 x y = unify x (Foo y); unify y (Foo x); (x : y);;
                                           ^^^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

Line 1, characters 48-49:
1 | let z3 x y = unify x (Foo y); unify y (Foo x); (x : y);;
                                                    ^
Error: The value "x" has type "x" but an expression was expected of type "y"
|}];;

(* Should fail. This currently succeeds because there is a lexical order of
   defaulting. This should be extremely rare. *)
let z4 x y = unify x (Foo y); unify y (Foo x);;
[%%expect {|
val z4 : x -> y -> unit = <fun>
|}, Principal{|
Line 1, characters 39-42:
1 | let z4 x y = unify x (Foo y); unify y (Foo x);;
                                           ^^^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

val z4 : x -> y -> unit = <fun>
|}];;

(* These tests overload the constructors further  *)

(* [t3.B] takes an inline record, [u3.B] a [bool]. *)
type t3 = A | B of { n : int }
type u3 = A | B of bool | C
[%%expect {|
type t3 = A | B of { n : int; }
type u3 = A | B of bool | C
|}];;

let j1 = B { n = 1 } |> (fun (x : t3) -> x);;
[%%expect {|
Line 1, characters 11-20:
1 | let j1 = B { n = 1 } |> (fun (x : t3) -> x);;
               ^^^^^^^^^
Error: This expression should not be a record, the expected type is "bool"
|}];;

let j2 = function B { n } -> n | (A : t3) -> 0;;
[%%expect {|
Line 1, characters 20-25:
1 | let j2 = function B { n } -> n | (A : t3) -> 0;;
                        ^^^^^
Error: This pattern should not be a record, the expected type is "bool"
|}];;

(* ex_8 in the paper: a parametrised overloaded constructor.
   [w] is declared last so that [pt] is not the default. *)
type 'a pt = A | B of 'a
type w = A | B of bool
[%%expect {|
type 'a pt = A | B of 'a
type w = A | B of bool
|}];;

(* Stage 3 backpropagation from the first instance selects [pt]; the
   second instance uses a different ['a]. *)
let j3 gp =
  let getb = function B x -> x | _ -> assert false in
  (getb (B 42 : int pt), (getb gp : float))
;;
[%%expect {|
Line 3, characters 8-23:
3 |   (getb (B 42 : int pt), (getb gp : float))
            ^^^^^^^^^^^^^^^
Error: This expression has type "int pt" but an expression was expected of type
         "w"
|}];;

let j4 =
  let getb = function B x -> x | _ -> assert false in
  (getb (B 1 : int pt), getb (B "s" : string pt))
;;
[%%expect {|
Line 3, characters 8-22:
3 |   (getb (B 1 : int pt), getb (B "s" : string pt))
            ^^^^^^^^^^^^^^
Error: This expression has type "int pt" but an expression was expected of type
         "w"
|}];;

type v = A | B of string | E;;
let j5 x = match x with A -> 0 | (B _ : v) -> 1;;
[%%expect {|
type v = A | B of string | E
Line 2, characters 11-47:
2 | let j5 x = match x with A -> 0 | (B _ : v) -> 1;;
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "E"

val j5 : v -> int = <fun>
|}];;
