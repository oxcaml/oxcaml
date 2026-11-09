(* TEST
 expect;
*)

(* Example from Stephen Dolan.
   Accessing an extension constructor involves accessing the module
   in which it's defined.
 *)
module type T =
  sig exception A of int end;;
[%%expect{|
module type T = sig exception A of int end
|}];;

let rec x =
  let module M = (val m) in
  M.A 42
and (m : (module T)) =
  (module (struct exception A of int end) : T);;
[%%expect{|
Line 2, characters 17-24:
2 |   let module M = (val m) in
                     ^^^^^^^
Error: This expression may use "m" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;
