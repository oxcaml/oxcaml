(* TEST
 expect;
*)

let f ~x () = x ();;
[%%expect{|
val f : x:(unit -> 'a) -> unit -> 'a = <fun>
|}];;

let rec x = f ~x;;
[%%expect{|
Line 1, characters 12-16:
1 | let rec x = f ~x;;
                ^^^^
Error: This expression may use "x" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

let f x ~y = x + y
(* this function creates "abstracted arguments" in the sense of
   Rec_check.is_abstracted_arg. Those should be treated as
   returned/unguarded, and not delayed, otherwise the code below
   segfaults. *)
let rec g = f ~y:(print_endline !y; 0)
and y =
  let _ = g in (* ignore g to have a real dependency *)
  ref "foo";;
[%%expect {|
val f : int -> y:int -> int = <fun>
Line 6, characters 12-38:
6 | let rec g = f ~y:(print_endline !y; 0)
                ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression may use "y" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}]
