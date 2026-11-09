(* TEST
 expect;
*)

(* A record bound by [let rec] may refer to the group through its mutable
   fields, which are assigned once the whole group is defined. *)

type t = { mutable next : t; v : int }
let rec x = { next = x; v = 1 };;
[%%expect{|
type t = { mutable next : t; v : int; }
val x : t = {next = <cycle>; v = 1}
|}];;

let () = assert (x.next == x);;
[%%expect{|
|}];;

let rec x : t = { next = x; v = 2 };;
[%%expect{|
val x : t = {next = <cycle>; v = 2}
|}];;

type cell = { mutable prev : cell; mutable succ : cell; data : int }
let rec a = { prev = b; succ = b; data = 0 }
and b = { prev = a; succ = a; data = 1 };;
[%%expect{|
type cell = { mutable prev : cell; mutable succ : cell; data : int; }
val a : cell =
  {prev = {prev = <cycle>; succ = <cycle>; data = 1};
   succ = {prev = <cycle>; succ = <cycle>; data = 1}; data = 0}
val b : cell =
  {prev = {prev = <cycle>; succ = <cycle>; data = 0};
   succ = {prev = <cycle>; succ = <cycle>; data = 0}; data = 1}
|}];;

let () = assert (a.prev == b && a.succ == b && b.prev == a && b.succ == a);;
[%%expect{|
|}];;

let mk v = let rec x = { next = x; v } in x;;
[%%expect{|
val mk : int -> t = <fun>
|}];;

(* Inline records. *)

type node = Node of { mutable parent : node option; name : string }
let rec root = Node { parent = Some root; name = "root" };;
[%%expect{|
type node = Node of { mutable parent : node option; name : string; }
val root : node = Node {parent = Some <cycle>; name = "root"}
|}];;

let () =
  match root with
  | Node { parent = Some p; _ } -> assert (p == root)
  | Node { parent = None; _ } -> assert false;;
[%%expect{|
|}];;

(* The mutable fields may hold functions and lazy values referring to the
   group, and the functions of the group may refer to the records. *)

type counter = { mutable tick : unit -> int; mutable count : int }
let rec c = { tick = (fun () -> incr ()); count = 0 }
and incr () = c.count <- c.count + 1; c.count;;
[%%expect{|
type counter = { mutable tick : unit -> int; mutable count : int; }
val c : counter = {tick = <fun>; count = 0}
val incr : unit -> int = <fun>
|}];;

let () = assert (c.tick () = 1 && incr () = 2 && c.count = 2);;
[%%expect{|
|}];;

type tree = { mutable children : tree list Lazy.t; id : int }
let rec tree = { children = lazy [tree]; id = 0 };;
[%%expect{|
type tree = { mutable children : tree list Lazy.t; id : int; }
val tree : tree = {children = <lazy>; id = 0}
|}];;

let () = assert (List.hd (Lazy.force tree.children) == tree);;
[%%expect{|
|}];;

(* Constructors, tuples, arrays, polymorphic variants and nested records of
   recursive names are constructions. *)

type many =
  { mutable all : many list;
    mutable pair : many * many;
    mutable arr : many array;
    mutable link : [ `Link of many | `None ];
    mutable info : info }
and info = { depth : int; owner : many }
let rec m =
  { all = [m; m];
    pair = (m, m);
    arr = [| m |];
    link = `Link m;
    info = { depth = -1; owner = m } };;
[%%expect{|
type many = {
  mutable all : many list;
  mutable pair : many * many;
  mutable arr : many array;
  mutable link : [ `Link of many | `None ];
  mutable info : info;
}
and info = { depth : int; owner : many; }
val m : many =
  {all = [<cycle>; <cycle>]; pair = (<cycle>, <cycle>); arr = [|<cycle>|];
   link = `Link <cycle>; info = {depth = -1; owner = <cycle>}}
|}];;

let () =
  assert (List.for_all (fun n -> n == m) m.all);
  assert (fst m.pair == m && snd m.pair == m && m.arr.(0) == m);
  assert (match m.link with `Link n -> n == m | `None -> false);
  assert (m.info.owner == m);;
[%%expect{|
|}];;

(* Flat float fields and mixed records. *)

type fr = { mutable fx : float; fy : float }
let rec one = 1.0
and fr = { fx = one; fy = 2.0 };;
[%%expect{|
type fr = { mutable fx : float; fy : float; }
val one : float = 1.
val fr : fr = {fx = 1.; fy = 2.}
|}];;

type mixed = { mutable m : mixed; u : float#; k : int }
let rec mx = { m = mx; u = #3.0; k = 4 };;
[%%expect{|
type mixed = { mutable m : mixed; u : float#; k : int; }
val mx : mixed = {m = <cycle>; u = <abstr>; k = 4}
|}];;

external box_float : float# -> float = "%box_float"
let () = assert (mx.m == mx && box_float mx.u = 3.0);;
[%%expect{|
external box_float : float# -> float = "%box_float"
|}];;

(* Fields that do not refer to the group are evaluated when the record is
   allocated, whether or not they are mutable. *)

type r = { mutable self : r; mutable n : int; s : string }
let s = "abc"
let rec r = { self = r; n = String.length s; s };;
[%%expect{|
type r = { mutable self : r; mutable n : int; s : string; }
val s : string = "abc"
val r : r = {self = <cycle>; n = 3; s = "abc"}
|}];;

(* Immutable fields may not refer to the group. *)

type f = { f : unit -> f }
let rec r = { f = (fun () -> r) };;
[%%expect{|
type f = { f : unit -> f; }
val r : f = {f = <fun>}
|}];;

type g = { g : unit -> int; mutable self : g }
let rec gr = { g = h; self = gr }
and h () = 1;;
[%%expect{|
type g = { g : unit -> int; mutable self : g; }
val gr : g = {g = <fun>; self = <cycle>}
val h : unit -> int = <fun>
|}];;

let rec e = { next = e; v = e.v };;
[%%expect{|
Line 1, characters 28-31:
1 | let rec e = { next = e; v = e.v };;
                                ^^^
Error: This expression may use "e" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

(* A mutable field may not read through the group. *)

let rec y = { next = y.next; v = 0 };;
[%%expect{|
Line 1, characters 21-27:
1 | let rec y = { next = y.next; v = 0 };;
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

let rec z = { next = id z; v = 0 }
and id w = w;;
[%%expect{|
Line 1, characters 21-25:
1 | let rec z = { next = id z; v = 0 }
                         ^^^^
Error: This expression may use "id" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

let rec w = { next = (if true then w else w); v = 0 };;
[%%expect{|
Line 1, characters 21-44:
1 | let rec w = { next = (if true then w else w); v = 0 };;
                         ^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression may use "w" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

let rec w = { next = (let n = w in n); v = 0 };;
[%%expect{|
Line 1, characters 21-37:
1 | let rec w = { next = (let n = w in n); v = 0 };;
                         ^^^^^^^^^^^^^^^^
Error: This expression may use "w" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

let rec w = { next = { w with v = 1 }; v = 0 };;
[%%expect{|
Line 1, characters 21-37:
1 | let rec w = { next = { w with v = 1 }; v = 0 };;
                         ^^^^^^^^^^^^^^^^
Error: This expression may use "w" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

(* Functional update is not a record literal. *)

let rec u = { x with next = u };;
[%%expect{|
Line 1, characters 12-31:
1 | let rec u = { x with next = u };;
                ^^^^^^^^^^^^^^^^^^^
Error: This expression may use "u" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

(* Unboxed records are not blocks. *)

type 'a ub = #{ mutable_looking : 'a }
type holder = { mutable held : holder ub }
let rec hd = { held = #{ mutable_looking = hd } };;
[%%expect{|
type 'a ub = #{ mutable_looking : 'a; }
type holder = { mutable held : holder ub; }
Line 3, characters 22-47:
3 | let rec hd = { held = #{ mutable_looking = hd } };;
                          ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression may use "hd" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

(* Reference cells are records with a single mutable field. *)

let rec counter = ref 0
and next () = Stdlib.incr counter; !counter;;
[%%expect{|
val counter : int ref = {contents = 0}
val next : unit -> int = <fun>
|}];;

let () = assert (next () = 1 && next () = 2);;
[%%expect{|
|}];;

let rec loop = ref (fun () -> !loop ());;
[%%expect{|
val loop : (unit -> '_weak1) ref = {contents = <fun>}
|}];;

type node = { name : string; parent : node ref option }
let rec root = ref { name = "root"; parent = Some root };;
[%%expect{|
type node = { name : string; parent : node ref option; }
val root : node ref = {contents = {name = "root"; parent = Some <cycle>}}
|}];;

let () =
  match !root with
  | { parent = Some parent; _ } -> assert (parent == root)
  | { parent = None; _ } -> assert false;;
[%%expect{|
|}];;

let rec cell = Atomic.make (fun () -> (Atomic.get cell) ());;
[%%expect{|
val cell : (unit -> '_weak2) Atomic.t = {Atomic.contents = <fun>}
|}];;

let rec bad = ref (!bad + 1);;
[%%expect{|
Line 1, characters 18-28:
1 | let rec bad = ref (!bad + 1);;
                      ^^^^^^^^^^
Error: This expression may use "bad" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;

let my_ref x = ref x
let rec bad = my_ref (fun () -> !bad ());;
[%%expect{|
val my_ref : 'a -> 'a ref = <fun>
Line 2, characters 14-40:
2 | let rec bad = my_ref (fun () -> !bad ());;
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression may use "bad" before the "let rec" has defined it.
Hint: Each right-hand side of a "let rec" must have the form r:
    right-hand side r ::= b | fun ... -> e | lazy e | let x = r in r
    block           b ::= C (v, ...) | (v, ...) | { l = v; ... } | `C v | ref v
    value           v ::= x | c | b | fun ... -> e | lazy e | [| v; ... |]
  Here x is a variable, c a constant, C a constructor and e any
  expression. Expressions that do not mention the names defined by the
  "let rec" are also allowed.
|}];;
