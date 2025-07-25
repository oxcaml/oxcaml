(* TEST
   expect;
*)

type ab =
  [ `A
  | `B ]

let f (x : [`A]) = match x with #ab -> 1

[%%expect
{|
type ab = [ `A | `B ]
Line 5, characters 32-35:
5 | let f (x : [`A]) = match x with #ab -> 1
                                    ^^^
Error: This pattern matches values of type "[? `A | `B ]"
       but a pattern was expected which matches values of type "[ `A ]"
       The second variant type does not allow tag(s) "`B"
|}]

let f x =
  ignore (match x with #ab -> 1);
  ignore (x : [`A])

[%%expect
{|
Line 2, characters 23-26:
2 |   ignore (match x with #ab -> 1);
                           ^^^
Error: This pattern matches values of type "[? `B ]"
       but a pattern was expected which matches values of type "[ `A ]"
       The second variant type does not allow tag(s) "`B"
|}]

let f x =
  ignore (match x with `A | `B -> 1);
  ignore (x : [`A])

[%%expect
{|
Line 2, characters 28-30:
2 |   ignore (match x with `A | `B -> 1);
                                ^^
Error: This pattern matches values of type "[? `B ]"
       but a pattern was expected which matches values of type "[ `A ]"
       The second variant type does not allow tag(s) "`B"
|}]

let f (x : [< `A | `B]) = match x with `A | `B | `C -> 0

(* warn *)
[%%expect
{|
Line 1, characters 49-51:
1 | let f (x : [< `A | `B]) = match x with `A | `B | `C -> 0
                                                     ^^
Warning 12 [redundant-subpat]: this sub-pattern is unused.

val f : [< `A | `B ] -> int = <fun>
|}]

let f (x : [`A | `B]) = match x with `A | `B | `C -> 0

(* fail *)
[%%expect
{|
Line 1, characters 47-49:
1 | let f (x : [`A | `B]) = match x with `A | `B | `C -> 0
                                                   ^^
Error: This pattern matches values of type "[? `C ]"
       but a pattern was expected which matches values of type "[ `A | `B ]"
       The second variant type does not allow tag(s) "`C"
|}]

(* imported from in poly.ml *)
type t =
  | A
  | B
;;

function `A, _ -> 1 | _, A -> 2 | _, B -> 3;;

function `A, _ -> 1 | _, (A | B) -> 2;;

function Some `A, _ -> 1 | Some _, A -> 2 | None, A -> 3 | _, B -> 4;;

function
| Some `A, A -> 1 | Some `A, B -> 1 | Some _, A -> 2 | None, A -> 3 | _, B -> 4
;;

function A, `A -> 1 | A, `B -> 2 | B, _ -> 3;;

function `A, A -> 1 | `B, A -> 2 | _, B -> 3;;

function (`A | `B), _ -> 0 | _, (`A | `B) -> 1;;

function `B, 1 -> 1 | _, 1 -> 2;;

function 1, `B -> 1 | 1, _ -> 2

[%%expect
{|
type t = A | B
- : [> `A ] * t -> int = <fun>
- : [> `A ] * t -> int = <fun>
- : [> `A ] option * t -> int = <fun>
- : [> `A ] option * t -> int = <fun>
- : t * [< `A | `B ] -> int = <fun>
- : [< `A | `B ] * t -> int = <fun>
Line 20, characters 0-46:
20 | function (`A | `B), _ -> 0 | _, (`A | `B) -> 1;;
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(`AnyOtherTag, `AnyOtherTag)

- : [> `A | `B ] * [> `A | `B ] -> int = <fun>
Line 22, characters 0-31:
22 | function `B, 1 -> 1 | _, 1 -> 2;;
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(_, 0)

Line 22, characters 22-26:
22 | function `B, 1 -> 1 | _, 1 -> 2;;
                           ^^^^
Warning 11 [redundant-case]: this match case is unused.

- : [< `B ] * int -> int = <fun>
Line 24, characters 0-31:
24 | function 1, `B -> 1 | 1, _ -> 2
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(0, _)

Line 24, characters 22-26:
24 | function 1, `B -> 1 | 1, _ -> 2
                           ^^^^
Warning 11 [redundant-case]: this match case is unused.

- : int * [< `B ] -> int = <fun>
|}]

(* PR#6787 *)
let revapply x f = f x

let f x (g : [< `Foo]) =
  let y = `Bar x, g in
  revapply y (fun (`Bar i, _) -> i)

(* f : 'a -> [< `Foo ] -> 'a *)
[%%expect
{|
val revapply : 'a -> ('a -> 'b) -> 'b = <fun>
val f : 'a -> [< `Foo ] -> 'a = <fun>
|}]

(* PR#6124 *)
let f : ([`A | `B] as 'a) -> [> 'a] -> unit = fun x (y : [> 'a]) -> ()

let f (x : [`A | `B] as 'a) (y : [> 'a]) = ()

[%%expect
{|
Line 1, characters 60-62:
1 | let f : ([`A | `B] as 'a) -> [> 'a] -> unit = fun x (y : [> 'a]) -> ()
                                                                ^^
Error: The type "'a" does not expand to a polymorphic variant type
Hint: Did you mean "`a"?
|}]

(* CR reisenberg: This test is disabled. It only barely works in `main`
      anyway, as evidenced by https://github.com/ocaml/ocaml/issues/13369
   (* PR#5927 *)
   type 'a foo = 'a constraint 'a = [< `Tag of & int];;
   [%%expect{|
   type 'a foo = 'a constraint 'a = [< `Tag of & int ]
   |}]
*)

(* PR#7704 *)
type t = private [> `A of string];;

function (`A x : t) -> x

[%%expect
{|
type t = private [> `A of string ]
Line 3, characters 0-24:
3 | function (`A x : t) -> x
    ^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
`<some private tag>

- : t -> string = <fun>
|}]

let f = function `AnyOtherTag, _ -> 1 | _, (`AnyOtherTag | `AnyOtherTag') -> 2

[%%expect
{|
Line 1, characters 8-78:
1 | let f = function `AnyOtherTag, _ -> 1 | _, (`AnyOtherTag | `AnyOtherTag') -> 2
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(`AnyOtherTag', `AnyOtherTag'')

val f : [> `AnyOtherTag ] * [> `AnyOtherTag | `AnyOtherTag' ] -> int = <fun>
|}]

let x : ([`A] as 'a) * ([`B] as 'a) = [`A]

[%%expect
{|
Line 1, characters 33-34:
1 | let x : ([`A] as 'a) * ([`B] as 'a) = [`A]
                                     ^
Error: This alias is bound to type "[ `B ]" but is used as an instance of type
         "[ `A ]"
       These two variant types have no intersection
|}]

type t = private [< `A]

let f : t -> [`A] = fun x -> x

[%%expect
{|
type t = private [< `A ]
Line 3, characters 29-30:
3 | let f : t -> [`A] = fun x -> x
                                 ^
Error: This expression has type "t" but an expression was expected of type
         "[ `A ]"
       The first variant type is private, it may not allow the tag(s) "`A"
|}]

(** Check that the non-regularity error message is robust to permutation *)

type ('a, 'b, 'c, 'd, 'e) a = [`A of ('d, 'a, 'e, 'c, 'b) b]

and ('a, 'b, 'c, 'd, 'e) b = [`B of ('c, 'd, 'e, 'a, 'b) c]

and ('a, 'b, 'c, 'd, 'e) c = [`C of ('a, 'b, 'c, 'd, 'e) a]

[%%expect
{|
Line 3, characters 0-60:
3 | type ('a, 'b, 'c, 'd, 'e) a = [`A of ('d, 'a, 'e, 'c, 'b) b]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This recursive type is not regular.
       The type constructor "a" is defined as
         type "('a, 'b, 'c, 'd, 'e) a"
       but it is used as
         "('e, 'c, 'b, 'd, 'a) a"
       after the following expansion(s):
         "[ `A of ('d, 'a, 'e, 'c, 'b) b ]" contains "('d, 'a, 'e, 'c, 'b) b",
         "('d, 'a, 'e, 'c, 'b) b" = "[ `B of ('e, 'c, 'b, 'd, 'a) c ]",
         "[ `B of ('e, 'c, 'b, 'd, 'a) c ]" contains "('e, 'c, 'b, 'd, 'a) c",
         "('e, 'c, 'b, 'd, 'a) c" = "[ `C of ('e, 'c, 'b, 'd, 'a) a ]",
         "[ `C of ('e, 'c, 'b, 'd, 'a) a ]" contains "('e, 'c, 'b, 'd, 'a) a"
       All uses need to match the definition for the recursive type to be regular.
|}]

(* PR 10762 *)
type a = int

type t = [`A of a]

let inspect : [< t] -> unit = function `A 0 -> () | `A _ -> ()

[%%expect
{|
type a = int
type t = [ `A of a ]
val inspect : [< `A of a & int ] -> unit = <fun>
|}]

(** Error messages with weakly polymorphic row variables *)
let x = !(ref (function `X -> () | _ -> ()))

[%%expect {|
val x : ([> `X ] as '_weak1) -> unit = <fun>
|}]

let x =
  let rec x = `X (`Y (fun y -> x = y)) in
  !(ref x)

[%%expect
{|
val x : [> `X of [> `Y of '_weak2 -> bool ] as '_weak3 ] as '_weak2 =
  `X (`Y <fun>)
|}]

(** Code coverage for [unify_row_field] errors *)

(** Arity mismatch *)

let f (x : [`X of int]) : [`X] = x

[%%expect
{|
Line 5, characters 33-34:
5 | let f (x : [`X of int]) : [`X] = x
                                     ^
Error: This expression has type "[ `X of int ]"
       but an expression was expected of type "[ `X ]"
       Types for tag "`X" are incompatible
|}]

let f (x : [`X of int]) : [< `X of  & int] = x

[%%expect
{|
Line 1, characters 45-46:
1 | let f (x : [`X of int]) : [< `X of  & int] = x
                                                 ^
Error: This expression has type "[ `X of int ]"
       but an expression was expected of type "[< `X of & int ]"
       Types for tag "`X" are incompatible
|}]

(** Inconsistent type *)

let f (x : [< `X of  & int & float]) : [`X] = x

[%%expect
{|
Line 3, characters 46-47:
3 | let f (x : [< `X of  & int & float]) : [`X] = x
                                                  ^
Error: This expression has type "[< `X of & int & float ]"
       but an expression was expected of type "[ `X ]"
       Types for tag "`X" are incompatible
|}]

(** Missing tag correctly attributed *)
type rt =
  [ `A
  | `B of string
  | `R of rt ]

let rec f = function `A -> 0 | `B s -> int_of_string s | `R x -> f x

let g (x : [`A | `R of rt]) = f x

[%%expect
{|
type rt = [ `A | `B of string | `R of rt ]
val f : ([< `A | `B of string | `R of 'a ] as 'a) -> int = <fun>
Line 8, characters 32-33:
8 | let g (x : [`A | `R of rt]) = f x
                                    ^
Error: This expression has type "[ `A | `R of rt ]"
       but an expression was expected of type "[< `A | `R of 'a ] as 'a"
       Type "rt" = "[ `A | `B of string | `R of rt ]" is not compatible with type
         "[< `A | `R of 'a ] as 'a"
       The second variant type does not allow tag(s) "`B"
|}]
