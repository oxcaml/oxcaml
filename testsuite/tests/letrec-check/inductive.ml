(* TEST
 expect;
*)

(* Only values of inductive types may not be cyclic. Types are not inductive
   unless marked [@@inductive]; [list] and [option] are inductive. *)

type t = { next : t; v : int }
let rec x = { next = x; v = 1 };;
[%%expect{|
type t = { next : t; v : int; }
val x : t = {next = <cycle>; v = 1}
|}];;

let () = assert (x.next == x);;
[%%expect{|
|}];;

type a = A of b * int
and b = B of a * int
let rec a = A (b, 0)
and b = B (a, 1);;
[%%expect{|
type a = A of b * int
and b = B of a * int
val a : a = A (B (<cycle>, 1), 0)
val b : b = B (A (<cycle>, 0), 1)
|}];;

(* Cycles through lists and options are fine when they also go through a
   value of a type that is not inductive. *)

type tree = { label : string; children : tree list; parent : tree option }
let rec root = { label = "root"; children = [leaf]; parent = None }
and leaf = { label = "leaf"; children = []; parent = Some root };;
[%%expect{|
type tree = { label : string; children : tree list; parent : tree option; }
val root : tree =
  {label = "root";
   children = [{label = "leaf"; children = []; parent = Some <cycle>}];
   parent = None}
val leaf : tree =
  {label = "leaf"; children = [];
   parent = Some {label = "root"; children = [<cycle>]; parent = None}}
|}];;

let () =
  match root.children, leaf.parent with
  | [l], Some r -> assert (l == leaf && r == root)
  | _ -> assert false;;
[%%expect{|
|}];;

(* Inline records are part of the block of their constructor. *)

type node = Node of { name : string; self : node }
let rec n = Node { name = "n"; self = n };;
[%%expect{|
type node = Node of { name : string; self : node; }
val n : node = Node {name = "n"; self = <cycle>}
|}];;

(* Cycles of lists are rejected. *)

let rec xs = 1 :: xs;;
[%%expect{|
Line 1, characters 18-20:
1 | let rec xs = 1 :: xs;;
                      ^^
Error: This "let rec" would create a cycle of values of inductive types: "xs" ->
       "xs".
Hint: A cycle must go through a function, a "lazy" value, a mutable field
or a value of a type not marked "[@@inductive]".
|}];;

let rec ys = 1 :: zs
and zs = 2 :: ys;;
[%%expect{|
Line 1, characters 18-20:
1 | let rec ys = 1 :: zs
                      ^^
Error: This "let rec" would create a cycle of values of inductive types: "zs" ->
       "ys" -> "zs".
Hint: A cycle must go through a function, a "lazy" value, a mutable field
or a value of a type not marked "[@@inductive]".
|}];;

(* So are cycles of values of types marked [@@inductive]. *)

type 'a stream = Cons of 'a * 'a stream [@@inductive]
let rec s = Cons (1, s);;
[%%expect{|
type 'a stream = Cons of 'a * 'a stream
Line 2, characters 21-22:
2 | let rec s = Cons (1, s);;
                         ^
Error: This "let rec" would create a cycle of values of inductive types: "s" ->
       "s".
Hint: A cycle must go through a function, a "lazy" value, a mutable field
or a value of a type not marked "[@@inductive]".
|}];;

let rec s1 = Cons (1, s2) and s2 = Cons (2, s1);;
[%%expect{|
Line 1, characters 22-24:
1 | let rec s1 = Cons (1, s2) and s2 = Cons (2, s1);;
                          ^^
Error: This "let rec" would create a cycle of values of inductive types: "s2" ->
       "s1" -> "s2".
Hint: A cycle must go through a function, a "lazy" value, a mutable field
or a value of a type not marked "[@@inductive]".
|}];;

type ind = { me : ind } [@@inductive]
let rec i = { me = i };;
[%%expect{|
type ind = { me : ind; }
Line 2, characters 19-20:
2 | let rec i = { me = i };;
                       ^
Error: This "let rec" would create a cycle of values of inductive types: "i" ->
       "i".
Hint: A cycle must go through a function, a "lazy" value, a mutable field
or a value of a type not marked "[@@inductive]".
|}];;

type c = C of d [@@inductive]
and d = D of c [@@inductive]
let rec c = C d
and d = D c;;
[%%expect{|
type c = C of d
and d = D of c
Line 3, characters 14-15:
3 | let rec c = C d
                  ^
Error: This "let rec" would create a cycle of values of inductive types: "d" ->
       "c" -> "d".
Hint: A cycle must go through a function, a "lazy" value, a mutable field
or a value of a type not marked "[@@inductive]".
|}];;

(* Inline records of inductive types are inductive. *)

type inode = INode of { iself : inode } [@@inductive]
let rec inode = INode { iself = inode };;
[%%expect{|
type inode = INode of { iself : inode; }
Line 2, characters 32-37:
2 | let rec inode = INode { iself = inode };;
                                    ^^^^^
Error: This "let rec" would create a cycle of values of inductive types:
       "inode" -> "inode".
Hint: A cycle must go through a function, a "lazy" value, a mutable field
or a value of a type not marked "[@@inductive]".
|}];;

(* A cycle through an inductive type and an option is rejected. *)

type r = R of r option [@@inductive]
let rec r = R (Some r);;
[%%expect{|
type r = R of r option
Line 2, characters 20-21:
2 | let rec r = R (Some r);;
                        ^
Error: This "let rec" would create a cycle of values of inductive types: "r" ->
       "r".
Hint: A cycle must go through a function, a "lazy" value, a mutable field
or a value of a type not marked "[@@inductive]".
|}];;

(* Abbreviations and re-exports of inductive types are inductive. *)

type 'a l = 'a list
let rec al : int l = 1 :: al;;
[%%expect{|
type 'a l = 'a list
Line 2, characters 26-28:
2 | let rec al : int l = 1 :: al;;
                              ^^
Error: This "let rec" would create a cycle of values of inductive types: "al" ->
       "al".
Hint: A cycle must go through a function, a "lazy" value, a mutable field
or a value of a type not marked "[@@inductive]".
|}];;

module M = struct
  type 'a s = 'a stream = Cons of 'a * 'a stream
end
let rec ms = M.Cons (1, ms);;
[%%expect{|
module M : sig type 'a s = 'a stream = Cons of 'a * 'a stream end
Line 4, characters 24-26:
4 | let rec ms = M.Cons (1, ms);;
                            ^^
Error: This "let rec" would create a cycle of values of inductive types: "ms" ->
       "ms".
Hint: A cycle must go through a function, a "lazy" value, a mutable field
or a value of a type not marked "[@@inductive]".
|}];;

(* Cycles through functions, lazy values and mutable fields are fine, even
   for inductive types. *)

type f = F of (unit -> f) [@@inductive]
let rec f = F (fun () -> f);;
[%%expect{|
type f = F of (unit -> f)
val f : f = F <fun>
|}];;

type ls = LCons of int * ls Lazy.t [@@inductive]
let rec ls = LCons (1, lazy ls);;
[%%expect{|
type ls = LCons of int * ls Lazy.t
val ls : ls = LCons (1, <cycle>)
|}];;

type m = { mutable m : m } [@@inductive]
let rec m = { m };;
[%%expect{|
type m = { mutable m : m; }
val m : m = {m = <cycle>}
|}];;

(* Tuples and polymorphic variants count as inductive: they only break a
   cycle when it also goes through a value of a type that is not inductive. *)

type tp = Tp of (tp * int) [@@inductive]
let rec tp = Tp (tp, 0);;
[%%expect{|
type tp = Tp of (tp * int)
Line 2, characters 17-19:
2 | let rec tp = Tp (tp, 0);;
                     ^^
Error: This "let rec" would create a cycle of values of inductive types: "tp" ->
       "tp".
Hint: A cycle must go through a function, a "lazy" value, a mutable field
or a value of a type not marked "[@@inductive]".
|}];;

type ntp = Ntp of (ntp * int)
let rec ntp = Ntp (ntp, 0);;
[%%expect{|
type ntp = Ntp of (ntp * int)
val ntp : ntp = Ntp (<cycle>, 0)
|}];;

let rec pv = `A pv;;
[%%expect{|
Line 1, characters 16-18:
1 | let rec pv = `A pv;;
                    ^^
Error: This "let rec" would create a cycle of values of inductive types: "pv" ->
       "pv".
Hint: A cycle must go through a function, a "lazy" value, a mutable field
or a value of a type not marked "[@@inductive]".
|}];;

type pr = { pr : [ `A of pr ] }
let rec pr = { pr = `A pr };;
[%%expect{|
type pr = { pr : [ `A of pr ]; }
val pr : pr = {pr = `A <cycle>}
|}];;

(* The attribute is not misplaced on a type declaration. *)

type unused = U of unused [@@inductive];;
[%%expect{|
type unused = U of unused
|}];;
