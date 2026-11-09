(* TEST
 expect;
*)

let f ~x:_ ~y:_ ~z:_ = ();;
[%%expect{|
val f : x:'a -> y:'b -> z:'c -> unit = <fun>
|}];;

(* Passing self immediately: forbidden *)
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

(* Passing self after an omitted argument: also forbidden *)
let rec y = f ~y;;
[%%expect{|
Line 1, characters 12-16:
1 | let rec y = f ~y;;
                ^^^^
Error: This expression may use "y" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

(* Passing self immediately: forbidden even if other arguments are omitted *)
let rec x = f ~x ~z:0;;
[%%expect{|
Line 1, characters 12-21:
1 | let rec x = f ~x ~z:0;;
                ^^^^^^^^^
Error: This expression may use "x" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

(* Calling self: also forbidden if the first argument is omitted *)
let g ~omitted ~given = fun ~given:_ -> given ~omitted
let rec f : omitted:_ -> given:_ -> _ = g ~given:(f ~given:0);;
[%%expect{|
val g : omitted:'a -> given:(omitted:'a -> 'b) -> given:'c -> 'b = <fun>
Line 2, characters 40-61:
2 | let rec f : omitted:_ -> given:_ -> _ = g ~given:(f ~given:0);;
                                            ^^^^^^^^^^^^^^^^^^^^^
Error: This expression may use "f" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

(* Calling self: forbidden if the first argument is passed *)
let g ~omitted_g ~given = fun ~omitted_f:_ ~given:_ -> given ~omitted_f:()
let rec f : omitted_g:_ -> omitted_f:_ -> given:_ -> _ =
  g ~given:(f ~omitted_g:() ~given:0);;
[%%expect{|
val g :
  omitted_g:'a ->
  given:(omitted_f:unit -> 'b) -> omitted_f:'c -> given:'d -> 'b = <fun>
Line 3, characters 2-37:
3 |   g ~given:(f ~omitted_g:() ~given:0);;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression may use "f" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;
