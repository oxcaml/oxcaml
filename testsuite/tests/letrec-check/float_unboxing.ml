(* TEST
 expect;
*)

(* This program is a minimal example which segfault if
   (e1.x <- e2) considers that (e2) is in Return mode,
   rather than Dereference -- here a write to a
   field in a statically-known all-float record is
   unboxed on the flight, so accepting this example
   would dereference (when running `g.f <- y` with y
   uninitialized) an arbitrary address. *)
type t = { mutable f: float }
let g = { f = 0.0 }
let rec x = (g.f <- y; ()) and y = 2.0;;
[%%expect{|
type t = { mutable f : float; }
val g : t = {f = 0.}
Line 3, characters 13-21:
3 | let rec x = (g.f <- y; ()) and y = 2.0;;
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

(* same example, with object instance variables
   instead of record fields *)
class c = object
  val mutable f = 0.0
  method m =
    let rec x = (f <- y; ()) and y = 2.0 in f
end;;
let _ = print_float (new c)#m;;
[%%expect{|
Line 4, characters 17-23:
4 |     let rec x = (f <- y; ()) and y = 2.0 in f
                     ^^^^^^
Error: This expression may use "y" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;
