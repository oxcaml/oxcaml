(* TEST
 expect;
*)

(* Recursive bindings of records and variants containing [any] *)

type ('a : any) t = { v : 'a; mutable next : 'a t option }
type r = { back : r t }
[%%expect{|
type ('a : any) t = { v : 'a; mutable next : 'a t option; }
type r = { back : r t; }
|}];;

let rec x = { v = 1; next = Some x }
[%%expect{|
Line 1, characters 12-36:
1 | let rec x = { v = 1; next = Some x }
                ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression may use "x" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

let rec a = { back = b }
and b = { v = a; next = None }
[%%expect{|
Line 2, characters 8-30:
2 | and b = { v = a; next = None }
            ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression may use "a" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

type ('a : any) w = A of 'a * 'a w option
[%%expect{|
type ('a : any) w = A of 'a * 'a w option
|}];;

let rec y = A (1, Some y)
[%%expect{|
Line 1, characters 23-24:
1 | let rec y = A (1, Some y)
                           ^
Error: This "let rec" would create a cycle of immutable values: "y" -> "y".
Hint: A cycle must go through a function, a "lazy" value or a mutable field.
|}];;

type q = { unwrap : q w }
[%%expect{|
type q = { unwrap : q w; }
|}];;

let rec c = { unwrap = d }
and d = A (c, None)
[%%expect{|
Line 1, characters 23-24:
1 | let rec c = { unwrap = d }
                           ^
Error: This "let rec" would create a cycle of immutable values: "d" -> "c" -> "d".
Hint: A cycle must go through a function, a "lazy" value or a mutable field.
|}];;

type ('a : any) opt = N | S of 'a
type tree = { left : tree opt; right : tree opt; data : int }
let rec leaf = { left = S leaf; right = S leaf; data = 42 }
[%%expect{|
type ('a : any) opt = N | S of 'a
type tree = { left : tree opt; right : tree opt; data : int; }
Line 3, characters 26-30:
3 | let rec leaf = { left = S leaf; right = S leaf; data = 42 }
                              ^^^^
Error: This "let rec" would create a cycle of immutable values: "leaf" -> "leaf".
Hint: A cycle must go through a function, a "lazy" value or a mutable field.
|}];;
