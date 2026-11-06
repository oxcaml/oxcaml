(* TEST
 flags = "-extension laws -laws-rocq rocq_types.v";
 setup-ocamlc.byte-build-env;
 module = "rocq_types.ml";
 ocamlc.byte;
 output = "${test_build_directory}/rocq_types.v";
 reference = "${test_source_directory}/rocq_types.v.reference";
 check-program-output;
*)

(* The Rocq file of a unit with user-defined variants and records: their
   semantic types, constructors, fields and matches, and the definitions
   that are outside the supported fragment. *)

type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree

type shape =
  | Circle of int
  | Rect of int * int
  | Dot

type point = { x : int; y : int }
type 'a pair = 'a * 'a

type even = Zero | Succ_even of odd
and odd = Succ_odd of even

let rec size = function
  | Leaf -> 0
  | Node (l, _, r) -> size l + 1 + size r

let rec mirror t =
  match t with
  | Leaf -> Leaf
  | Node (l, v, r) -> Node (mirror r, v, mirror l)

let area s =
  match s with
  | Circle r -> 3 * r * r
  | Rect (w, h) -> w * h
  | Dot -> 0

let is_round = function Circle _ -> true | _ -> false

let origin = { x = 0; y = 0 }
let move p dx = { p with x = p.x + dx }
let swap { x; y } = { x = y; y = x }
let manhattan (p : point) = (if p.x < 0 then - p.x else p.x) + p.y

let classify n =
  match n with
  | 0 -> 0
  | n when n < 0 -> -1
  | _ -> 1

let half n = n / 2
let quotient a b = (a / b, a mod b)
let fst3 (a, _, _) = a
let both (a, b) = a && b

module Point = struct
  let add p q = { x = p.x + q.x; y = p.y + q.y }
end

let greeting = "hello"
let twice_greeting () = greeting ^ greeting
let counter = ref 0

law? mirror_mirror (t : 'a tree) : mirror (mirror t) =:= t

law? size_mirror (t : 'a tree) : size (mirror t) = size t

law? area_nonneg (s : shape) : is_round s ===> area s >= 0

law? add_origin (p : point) : Point.add p origin =:= p

law? swap_swap (p : point) : swap (swap p) =:= p

law? move_zero (p : point) (q : int pair) :
  move p 0 =:= p && fst q = fst q
