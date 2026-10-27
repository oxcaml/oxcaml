(* TEST
 expect;
*)

(* Recursive definitions of functions are accepted, as are bindings that do
   not refer to the recursive group. *)

let rec even n = n = 0 || odd (n - 1)
and odd n = n <> 0 && even (n - 1);;
[%%expect{|
val even : int -> bool = <fun>
val odd : int -> bool = <fun>
|}];;

let rec fact = function 0 -> 1 | n -> n * fact (n - 1);;
[%%expect{|
val fact : int -> int = <fun>
|}];;

let rec f : type a. a -> a = fun x -> f x;;
[%%expect{|
val f : 'a -> 'a = <fun>
|}];;

let rec f x = g x
and g = (fun x -> f x : int -> int);;
[%%expect{|
val f : int -> int = <fun>
val g : int -> int = <fun>
|}];;

let rec f n = if n = 0 then base else f (n - 1)
and base = 42;;
[%%expect{|
val f : int -> int = <fun>
val base : int = 42
|}];;

let rec x = "x";;
[%%expect{|
val x : string = "x"
|}];;

let rec f () = x
and x = [1; 2; 3];;
[%%expect{|
val f : unit -> int list = <fun>
val x : int list = [1; 2; 3]
|}];;

let rec f = let tbl = Hashtbl.create 7 in fun x -> Hashtbl.find tbl x;;
[%%expect{|
val f : '_weak1 -> 'a = <fun>
|}];;

let rec f = fun x -> x
and g = let h = f in fun x -> h x;;
[%%expect{|
Line 2, characters 16-17:
2 | and g = let h = f in fun x -> h x;;
                    ^
Error: This expression may use "f" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

(* Non-function bindings may not refer to the recursive group: no cyclic
   values. *)

let rec x = 1 :: x;;
[%%expect{|
Line 1, characters 17-18:
1 | let rec x = 1 :: x;;
                     ^
Error: This "let rec" would create a cycle of immutable values: "x" -> "x".
Hint: A cycle must go through a function, a "lazy" value or a mutable field.
|}];;

type tup = T of tup * int
let rec x = T (x, 0);;
[%%expect{|
type tup = T of tup * int
Line 2, characters 15-16:
2 | let rec x = T (x, 0);;
                   ^
Error: This "let rec" would create a cycle of immutable values: "x" -> "x".
Hint: A cycle must go through a function, a "lazy" value or a mutable field.
|}];;

type 'a stream = Cons of 'a * 'a stream
let rec s = Cons (1, s);;
[%%expect{|
type 'a stream = Cons of 'a * 'a stream
Line 2, characters 21-22:
2 | let rec s = Cons (1, s);;
                         ^
Error: This "let rec" would create a cycle of immutable values: "s" -> "s".
Hint: A cycle must go through a function, a "lazy" value or a mutable field.
|}];;

type tree = Node of tree array
let rec t = Node [| t |];;
[%%expect{|
type tree = Node of tree array
val t : tree = Node [|<cycle>|]
|}];;

type t = { f : unit -> t }
let rec r = { f = (fun () -> r) };;
[%%expect{|
type t = { f : unit -> t; }
val r : t = {f = <fun>}
|}];;

(* Lazy values are accepted: their bodies are only evaluated when forced. *)

let rec x = lazy (Lazy.force x);;
[%%expect{|
val x : 'a Lazy.t = <lazy>
|}];;

let () =
  match Lazy.force x with
  | _ -> assert false
  | exception CamlinternalLazy.Undefined -> ();;
[%%expect{|
|}];;

let rec squares = lazy (List.map square [1; 2; 3])
and square n = n * n
and sum () = List.fold_left ( + ) 0 (Lazy.force squares);;
[%%expect{|
val squares : int list Lazy.t = <lazy>
val square : int -> int = <fun>
val sum : unit -> int = <fun>
|}];;

let () = assert (sum () = 14);;
[%%expect{|
|}];;

type cell = { mutable items : int Lazy.t list }
let rec count = lazy (List.length cell.items)
and cell = { items = [count; lazy 0] };;
[%%expect{|
type cell = { mutable items : int Lazy.t list; }
val count : int Lazy.t = <lazy>
val cell : cell = {items = [<lazy>; lazy 0]}
|}];;

let () = assert (Lazy.force count = 2);;
[%%expect{|
|}];;

(* Only the lazy values bound by the [let rec] are accepted, not those
   nested in its right-hand sides. *)

type stream = Cons of int * stream Lazy.t
let rec s = Cons (1, lazy s);;
[%%expect{|
type stream = Cons of int * stream Lazy.t
val s : stream = Cons (1, <cycle>)
|}];;

let rec x = (x; ());;
[%%expect{|
Line 1, characters 13-14:
1 | let rec x = (x; ());;
                 ^
Error: This expression may use "x" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

let rec x = let _ = x in 0;;
[%%expect{|
Line 1, characters 20-21:
1 | let rec x = let _ = x in 0;;
                        ^
Error: This expression may use "x" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

type cell = C of cell ref
let rec c = C (ref c);;
[%%expect{|
type cell = C of cell ref
val c : cell = C {contents = <cycle>}
|}];;

let rec x = let module M = struct let y = x end in M.y;;
[%%expect{|
Line 1, characters 27-47:
1 | let rec x = let module M = struct let y = x end in M.y;;
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

module type S = sig val y : int end
let rec x = (module struct let y = ignore x; 1 end : S);;
[%%expect{|
module type S = sig val y : int end
Line 2, characters 12-55:
2 | let rec x = (module struct let y = ignore x; 1 end : S);;
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression may use "x" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

type a = A of b * int
and b = B of a * int
let rec x = A (y, 0)
and y = B (x, 1);;
[%%expect{|
type a = A of b * int
and b = B of a * int
Line 3, characters 15-16:
3 | let rec x = A (y, 0)
                   ^
Error: This "let rec" would create a cycle of immutable values: "y" -> "x" -> "y".
Hint: A cycle must go through a function, a "lazy" value or a mutable field.
|}];;

let rec x = object method m = x end;;
[%%expect{|
Line 1, characters 12-35:
1 | let rec x = object method m = x end;;
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

(* Functions that are not syntactically functions are rejected too. *)

let rec f = let g x = f x in g;;
[%%expect{|
Line 1, characters 29-30:
1 | let rec f = let g x = f x in g;;
                                 ^
Error: This expression may use "g" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

let rec f = let count = ref 0 in fun x -> incr count; f x;;
[%%expect{|
val f : '_weak2 -> 'a = <fun>
|}];;

let rec f x = x
and g = f;;
[%%expect{|
Line 2, characters 8-9:
2 | and g = f;;
            ^
Error: This expression may use "f" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

let rec f = fun x -> x
and y = f 0;;
[%%expect{|
Line 2, characters 8-11:
2 | and y = f 0;;
            ^^^
Error: This expression may use "f" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

let rec f = Fun.id (fun x -> f x);;
[%%expect{|
Line 1, characters 12-33:
1 | let rec f = Fun.id (fun x -> f x);;
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

let rec f = (fun x -> x) |> fun g -> g;;
[%%expect{|
val f : '_weak3 -> '_weak3 = <fun>
|}];;

(* Binding operators count as references. *)

let ( let* ) x f = f x
let rec f = let* x = 1 in fun () -> x + f ();;
[%%expect{|
val ( let* ) : 'a -> ('a -> 'b) -> 'b = <fun>
Line 2, characters 12-44:
2 | let rec f = let* x = 1 in fun () -> x + f ();;
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression may use "f" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

let rec ( let* ) x f = f x
and y = let* x = 1 in x;;
[%%expect{|
Line 2, characters 8-23:
2 | and y = let* x = 1 in x;;
            ^^^^^^^^^^^^^^^
Error: This expression may use "let*" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

(* Recursive bindings in classes. *)

class c = let rec f x = if x then f false else 0 in object method m = f true end;;
[%%expect{|
class c : object method m : int end
|}];;

class c = let rec x = 1 :: x in object method m = x end;;
[%%expect{|
Line 1, characters 27-28:
1 | class c = let rec x = 1 :: x in object method m = x end;;
                               ^
Error: This "let rec" would create a cycle of immutable values: "x" -> "x".
Hint: A cycle must go through a function, a "lazy" value or a mutable field.
|}];;
