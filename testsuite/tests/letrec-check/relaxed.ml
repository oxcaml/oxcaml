(* TEST
 expect;
*)

(* A preamble that does not refer to the group is evaluated before the
   group. *)

let rec f = let tbl = Hashtbl.create 7 in fun x -> Hashtbl.mem tbl x || g x
and g = let open List in fun x -> mem x [1; 2; 3] && f (x + 1);;
[%%expect{|
val f : int -> bool = <fun>
val g : int -> bool = <fun>
|}];;

let rec len : 'a. 'a list -> int =
  fun (type a) -> let zero = 0 in fun (l : a list) ->
    match l with [] -> zero | _ :: t -> 1 + len t;;
[%%expect{|
val len : 'a list -> int = <fun>
|}];;

let rec f =
  let module M = struct let base = 1 end in
  print_endline "preamble";
  fun n -> if n = 0 then M.base else n * f (n - 1);;
[%%expect{|
val f : int -> int = <fun>
|}];;

(* The functions of a preamble that refer to the group join the group. *)

let rec to_string =
  let concat l = String.concat ", " (List.map to_string l) in
  function
  | `Atom s -> s
  | `List l -> "(" ^ concat l ^ ")";;
[%%expect{|
val to_string : ([< `Atom of string | `List of 'a list ] as 'a) -> string =
  <fun>
|}];;

let rec f =
  let rec loop acc = function [] -> acc | x :: l -> loop (acc + f x) l in
  fun x -> if x <= 0 then 0 else loop 1 (List.init x (fun i -> i));;
[%%expect{|
val f : int -> int = <fun>
|}];;

let rec t =
  let get () = Lazy.force t in
  lazy (fun () -> get ());;
[%%expect{|
Line 3, characters 18-24:
3 |   lazy (fun () -> get ());;
                      ^^^^^^
Error: This expression has type "unit -> 'a"
       but an expression was expected of type "'a"
       The type variable "'a" occurs inside "unit -> 'a"
Hint: This function application is partial, maybe some arguments are missing.
|}];;

(* Preamble items that are not variable bindings may not refer to the
   group, and neither may aliases. *)

let rec f = let h = f in fun x -> h x;;
[%%expect{|
Line 1, characters 20-21:
1 | let rec f = let h = f in fun x -> h x;;
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

let rec f = (ignore (f 0); fun x -> x + 1);;
[%%expect{|
Line 1, characters 13-25:
1 | let rec f = (ignore (f 0); fun x -> x + 1);;
                 ^^^^^^^^^^^^
Error: This expression may use "f" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

let rec f = let (h, _) = (fun x -> f x), 0 in fun x -> h x;;
[%%expect{|
Line 1, characters 25-42:
1 | let rec f = let (h, _) = (fun x -> f x), 0 in fun x -> h x;;
                             ^^^^^^^^^^^^^^^^^
Error: This expression may use "f" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

(* Calling a function is not a construction. *)

let rec f = (fun () -> f) ();;
[%%expect{|
Line 1, characters 12-28:
1 | let rec f = (fun () -> f) ();;
                ^^^^^^^^^^^^^^^^
Error: This expression may use "f" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

let rec xs = (fun () -> 1 :: xs) ();;
[%%expect{|
Line 1, characters 13-35:
1 | let rec xs = (fun () -> 1 :: xs) ();;
                 ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression may use "xs" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

(* Immutable blocks may refer to the group in their fields, as long as every
   cycle goes through a function, a lazy value or a mutable field. *)

type t = { f : unit -> int; x : int }
let rec t = { f = g; x = 1 } and g () = t.x + 1;;
[%%expect{|
type t = { f : unit -> int; x : int; }
val t : t = {f = <fun>; x = 1}
val g : unit -> int = <fun>
|}];;

let rec r = { f = (fun () -> r.x); x = 2 };;
[%%expect{|
val r : t = {f = <fun>; x = 2}
|}];;

type 'a stream = Cons of 'a * (unit -> 'a stream)
let rec ones = Cons (1, fun () -> ones);;
[%%expect{|
type 'a stream = Cons of 'a * (unit -> 'a stream)
val ones : int stream = Cons (1, <fun>)
|}];;

type p = P of int * p Lazy.t
let rec p = P (1, lazy p) and q = `Q (Some (fun () -> p));;
[%%expect{|
type p = P of int * p Lazy.t
val p : p = P (1, <cycle>)
val q : [> `Q of (unit -> p) option ] = `Q (Some <fun>)
|}];;

type u = A of { a : int; next : u lazy_t } | B
let rec a = A { a = 1; next = lazy b } and b = A { a = 2; next = lazy a };;
[%%expect{|
type u = A of { a : int; next : u lazy_t; } | B
val a : u = A {a = 1; next = lazy (A {a = 2; next = <cycle>})}
val b : u = A {a = 2; next = lazy (A {a = 1; next = <cycle>})}
|}];;

type node = { label : string; edges : node list ref }
let rec n1 = { label = "1"; edges = ref [n2] }
and n2 = { label = "2"; edges = ref [n1; n2] };;
[%%expect{|
type node = { label : string; edges : node list ref; }
val n1 : node =
  {label = "1";
   edges =
    {contents = [{label = "2"; edges = {contents = [<cycle>; <cycle>]}}]}}
val n2 : node =
  {label = "2";
   edges =
    {contents = [{label = "1"; edges = {contents = [<cycle>]}}; <cycle>]}}
|}];;

(* Acyclic references between blocks are fine. *)

type a = A of b and b = B of (unit -> a)
let rec a = A b and b = B (fun () -> a);;
[%%expect{|
type a = A of b
and b = B of (unit -> a)
val a : a = A (B <fun>)
val b : b = B <fun>
|}];;

(* Immutable cycles are rejected. *)

let rec xs = 1 :: xs;;
[%%expect{|
Line 1, characters 18-20:
1 | let rec xs = 1 :: xs;;
                      ^^
Error: This "let rec" would create a cycle of immutable values: "xs" -> "xs".
Hint: A cycle must go through a function, a "lazy" value or a mutable field.
|}];;

type c = C of c
let rec a = C b and b = C a;;
[%%expect{|
type c = C of c
Line 2, characters 14-15:
2 | let rec a = C b and b = C a;;
                  ^
Error: This "let rec" would create a cycle of immutable values: "b" -> "a" -> "b".
Hint: A cycle must go through a function, a "lazy" value or a mutable field.
|}];;

type d = D of d list * d Lazy.t
let rec a = D ([b], lazy b) and b = D ([], lazy a) and c = D ([a], lazy c)
and d = D ([e], lazy d) and e = D ([d], lazy a);;
[%%expect{|
type d = D of d list * d Lazy.t
Line 3, characters 12-13:
3 | and d = D ([e], lazy d) and e = D ([d], lazy a);;
                ^
Error: This "let rec" would create a cycle of immutable values: "e" -> "d" -> "e".
Hint: A cycle must go through a function, a "lazy" value or a mutable field.
|}];;

let rec x = { f = (fun () -> 0); x = (fun () -> 1) () } and y = Some x;;
[%%expect{|
val x : t = {f = <fun>; x = 1}
val y : t option = Some {f = <fun>; x = 1}
|}];;

(* Only constructions may refer to the group. *)

let rec x = Some (List.length [x]);;
[%%expect{|
Line 1, characters 17-34:
1 | let rec x = Some (List.length [x]);;
                     ^^^^^^^^^^^^^^^^^
Error: This expression may use "x" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

let rec x = { f = (fun () -> 0); x = x.x };;
[%%expect{|
Line 1, characters 37-40:
1 | let rec x = { f = (fun () -> 0); x = x.x };;
                                         ^^^
Error: This expression may use "x" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

(* Members of preambles are defined together with the group, so they must be
   heap-allocated. *)
let rec f =
  let p = local_ (fun x -> x) in
  let _g x = p x; f x in
  fun x -> x;;
[%%expect{|
Line 3, characters 6-8:
3 |   let _g x = p x; f x in
          ^^
Error: This value is local, but it must be allocated on the heap:
       it refers to the names defined by the enclosing "let rec",
       so "_g" is defined together with them.
|}];;
