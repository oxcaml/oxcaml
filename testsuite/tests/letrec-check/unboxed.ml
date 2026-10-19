(* TEST
 expect;
*)
(* CR layouts: Using layout annotations here is not backward-compatible.
   We can delete this when internal ticket 1110 is resolved.
*)

type r = R of r list [@@unboxed]
let rec a = R [a];;
[%%expect{|
type r = R of r list [@@unboxed]
Line 2, characters 12-17:
2 | let rec a = R [a];;
                ^^^^^
Error: This expression may use "a" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;


type t = {x: int64} [@@unboxed]
let rec x = {x = y} and y = 3L;;
[%%expect{|
type t = { x : int64; } [@@unboxed]
Line 2, characters 12-19:
2 | let rec x = {x = y} and y = 3L;;
                ^^^^^^^
Error: This expression may use "y" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

(* This test was made to error by disallowing singleton recursive unboxed types.
   We keep it in case these are re-allowed, in which case it should error with:
   [This kind of expression is not allowed as right-hand side of "let rec"] *)
type r = A of r [@@unboxed]
let rec y = A y;;
[%%expect{|
Line 1, characters 0-27:
1 | type r = A of r [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "r" is recursive without boxing:
         "r" contains "r"
|}];;

(* This test is not allowed if 'a' is unboxed, and is also rejected
   as written: the field is not a construction *)
type a = {a: b}
and b = X of a | Y

let rec a =
  {a=
    (if Sys.opaque_identity true then
       X a
     else
       Y)};;
[%%expect{|
type a = { a : b; }
and b = X of a | Y
Lines 6-9, characters 4-9:
6 | ....(if Sys.opaque_identity true then
7 |        X a
8 |      else
9 |        Y)...
Error: This expression may use "a" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

type a = {a: b }[@@unboxed]
and b = X of a | Y

let rec a =
  {a=
    (if Sys.opaque_identity true then
       X a
     else
       Y)};;
[%%expect{|
type a = { a : b; } [@@unboxed]
and b = X of a | Y
Lines 5-9, characters 2-10:
5 | ..{a=
6 |     (if Sys.opaque_identity true then
7 |        X a
8 |      else
9 |        Y)}..
Error: This expression may use "a" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

(* This test is not allowed if 'c' is unboxed, and is also rejected
   as written: the field is not a construction *)
type d = D of e
and e = V of d | W;;
[%%expect{|
type d = D of e
and e = V of d | W
|}];;

let rec d =
  D
    (if Sys.opaque_identity true then
       V d
     else
       W);;
[%%expect{|
Lines 3-6, characters 4-9:
3 | ....(if Sys.opaque_identity true then
4 |        V d
5 |      else
6 |        W)..
Error: This expression may use "d" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

type d = D of e [@@unboxed]
and e = V of d | W;;

let rec d =
  D
    (if Sys.opaque_identity true then
       V d
     else
       W);;
[%%expect{|
type d = D of e [@@unboxed]
and e = V of d | W
Lines 5-9, characters 2-9:
5 | ..D
6 |     (if Sys.opaque_identity true then
7 |        V d
8 |      else
9 |        W)..
Error: This expression may use "d" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;
