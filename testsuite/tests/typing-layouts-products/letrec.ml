(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 {
   expect;
 }
*)

(* This test was made to error by disallowing singleton recursive unboxed types.
   We keep it in case these are re-allowed, in which case it should error with:
   [This kind of expression is not allowed as right-hand side of "let rec"] *)
type t : value = #{ t : t }
let rec t = #{ t = t }
[%%expect{|
Line 1, characters 0-27:
1 | type t : value = #{ t : t }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "t" is recursive without boxing:
         "t" contains "t"
|}]


type t : value = { u : t }
let rec t = #{ u = t }
[%%expect{|
type t = { u : t; }
Line 2, characters 12-22:
2 | let rec t = #{ u = t }
                ^^^^^^^^^^
Error: This expression has type "t#" but an expression was expected of type "t"
|}]

type bx = { bx : ubx }
and ubx = #{ ubx : bx }
[%%expect{|
type bx = { bx : ubx; }
and ubx = #{ ubx : bx; }
|}]

let rec t = #{ ubx = { bx = t } }
[%%expect{|
Line 1, characters 12-33:
1 | let rec t = #{ ubx = { bx = t } }
                ^^^^^^^^^^^^^^^^^^^^^
Error: This expression may use "t" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}]

let rec t = { bx = #{ ubx = t } }
[%%expect{|
Line 1, characters 19-31:
1 | let rec t = { bx = #{ ubx = t } }
                       ^^^^^^^^^^^^
Error: This expression may use "t" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}]

type bx = { bx : bx2# }
and bx2 = { bx2 : bx }
[%%expect{|
type bx = { bx : bx2#; }
and bx2 = { bx2 : bx; }
|}]

let rec t = #{ bx2 = { bx = t } }
[%%expect{|
Line 1, characters 12-33:
1 | let rec t = #{ bx2 = { bx = t } }
                ^^^^^^^^^^^^^^^^^^^^^
Error: This expression may use "t" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}]

let rec t = { bx = #{ bx2 = t } }
[%%expect{|
Line 1, characters 19-31:
1 | let rec t = { bx = #{ bx2 = t } }
                       ^^^^^^^^^^^^
Error: This expression may use "t" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}]

(* The cycle-finding algorithm in genprintval does not store non-values in its
   [ObjTbl.t]. As a result, a cyclic unboxed record may be printed twice
   ([unboxed_cycle] in this example), but any cycle will contain at least one
   boxed value and will eventually be detected by genprintval. *)
type unboxed = #{ b : boxed option; name : string }
and boxed = { mutable a : unboxed }

let unboxed_cycle =
  let t = { a = #{ b = None; name = "t" } } in
  t.a <- #{ b = Some t; name = "t" };
  t.a
[%%expect{|
type unboxed = #{ b : boxed option; name : string; }
and boxed = { mutable a : unboxed; }
val unboxed_cycle : unboxed =
  #{b = Some {a = #{b = <cycle>; name = "t"}}; name = "t"}
|}]

(* The below is adapted from [testsuite/tests/letrec-check/unboxed.ml]. *)

type t = #{x: int64}
let rec x = #{x = y} and y = 3L;;
[%%expect{|
type t = #{ x : int64; }
Line 2, characters 12-20:
2 | let rec x = #{x = y} and y = 3L;;
                ^^^^^^^^
Error: This expression may use "y" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

type t = {x: int64}
let rec x = #{x = y} and y = 3L;;
[%%expect{|
type t = { x : int64; }
Line 2, characters 12-20:
2 | let rec x = #{x = y} and y = 3L;;
                ^^^^^^^^
Error: This expression may use "y" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

(* This test is not allowed if 'a' is unboxed, but should be accepted
   as written *)
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

type a = #{ a: b }
and b = X of a | Y

let rec a =
  #{a=
    (if Sys.opaque_identity true then
       X a
     else
       Y)};;
[%%expect{|
type a = #{ a : b; }
and b = X of a | Y
Lines 5-9, characters 2-10:
5 | ..#{a=
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

type a = { a: b }
and b = X of a# | Y

let rec a =
  #{a=
    (if Sys.opaque_identity true then
       X a
     else
       Y)};;
[%%expect{|
type a = { a : b; }
and b = X of a# | Y
Lines 5-9, characters 2-10:
5 | ..#{a=
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
