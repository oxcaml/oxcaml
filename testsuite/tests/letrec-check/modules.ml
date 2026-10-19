(* TEST
 expect;
*)

let rec x = let module M = struct let f = x end in ();;
[%%expect{|
Line 1, characters 27-47:
1 | let rec x = let module M = struct let f = x end in ();;
                               ^^^^^^^^^^^^^^^^^^^^
Error: This expression may use "x" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

let rec x = let module M = struct let f = x let g = x () end in fun () -> ();;
[%%expect{|
Line 1, characters 27-60:
1 | let rec x = let module M = struct let f = x let g = x () end in fun () -> ();;
                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression may use "x" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

let rec x = let module _ = struct let _ = x () end in fun () -> ();;
[%%expect{|
Line 1, characters 27-50:
1 | let rec x = let module _ = struct let _ = x () end in fun () -> ();;
                               ^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression may use "x" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

let rec x = let module M = struct let f = x () let g = x end in fun () -> ();;
[%%expect{|
Line 1, characters 27-60:
1 | let rec x = let module M = struct let f = x () let g = x end in fun () -> ();;
                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression may use "x" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

let rec x = (let module M = struct let f = y 0 let g = () end in fun () -> ())
    and y = succ;;
[%%expect{|
Line 1, characters 28-61:
1 | let rec x = (let module M = struct let f = y 0 let g = () end in fun () -> ())
                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression may use "y" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

let rec x =
  let module M = struct
    module N = struct let y = x end
  end in M.N.y;;
[%%expect{|
Lines 2-4, characters 17-5:
2 | .................struct
3 |     module N = struct let y = x end
4 |   end...........
Error: This expression may use "x" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

module type T = sig val y: int end

let rec x = let module M =
            struct
              module N =
              struct
                let y = x
              end
            end
  in fun () -> ignore (M.N.y ());;
[%%expect{|
module type T = sig val y : int end
Lines 4-9, characters 12-15:
4 | ............struct
5 |               module N =
6 |               struct
7 |                 let y = x
8 |               end
9 |             end
Error: This expression may use "x" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

let rec x = let module M = struct let f = x () and g = x end in fun () -> ();;
[%%expect{|
Line 1, characters 27-60:
1 | let rec x = let module M = struct let f = x () and g = x end in fun () -> ();;
                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression may use "x" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

module type T = sig end
let rec x = (module (val y : T) : T)
and y = let module M = struct let x = x end in (module M : T)
;;
[%%expect{|
module type T = sig end
Line 2, characters 12-36:
2 | let rec x = (module (val y : T) : T)
                ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression may use "y" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

(* module constraints *)
module type S = sig               val y : float end;;
module type T = sig val x : float val y : float end;;
type t = T : (module S) -> t;;

let rec x = let module M = (val m) in T (module M)
and (m : (module T)) = (module (struct let x = 10.0 and y = 20.0 end) : T);;
[%%expect{|
module type S = sig val y : float end
module type T = sig val x : float val y : float end
type t = T : (module S) -> t
Line 5, characters 27-34:
5 | let rec x = let module M = (val m) in T (module M)
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
