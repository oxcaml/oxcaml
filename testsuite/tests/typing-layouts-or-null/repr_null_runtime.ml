(* TEST
*)

(* [@repr null] coexisting with ordinary constant constructors, including a
   Marshal round-trip to confirm the runtime representation (null pointer +
   immediates) survives serialization. *)
type colour = Null_const [@repr null] | A | B

let classify = function Null_const -> 0 | A -> 1 | B -> 2

let () =
  assert (classify Null_const = 0);
  assert (classify A = 1);
  assert (classify B = 2);
  List.iter
    (fun v ->
       let v' : colour = Marshal.from_string (Marshal.to_string v []) 0 in
       assert (classify v = classify v'))
    [ Null_const; A; B ]

(* [@repr null] coexisting with a boxed non-constant constructor: deep tree
   build and traverse. *)
type tree = Empty [@repr null] | Node of int * tree * tree

let rec sum = function Empty -> 0 | Node (n, l, r) -> n + sum l + sum r
let rec depth = function Empty -> 0 | Node (_, l, r) -> 1 + max (depth l) (depth r)
let rec build n = if n = 0 then Empty else Node (n, build (n - 1), build (n - 1))

let () =
  let t =
    Node (5, Node (3, Empty, Empty), Node (7, Empty, Node (9, Empty, Empty)))
  in
  assert (sum t = 24);
  assert (depth t = 3);
  assert (not (match t with Empty -> true | Node _ -> false));
  let big = build 12 in
  assert (depth big = 12);
  assert (sum big = sum big) (* deterministic, non-crashing traversal *)

(* [@repr value] + [@repr null]: or_null-style, payload stored unboxed. *)
type ('a : value) opt : value_or_null = N [@repr null] | V of 'a [@repr value]

let to_option = function N -> None | V x -> Some x

let () =
  assert (to_option N = None);
  assert (to_option (V 42) = Some 42);
  assert (to_option (V "hi") = Some "hi");
  (match V 7 with N -> assert false | V x -> assert (x = 7))

(* [@repr unboxed]: single unary constructor, payload stored directly. *)
type wrapped = K of string [@repr unboxed]

let unwrap = function K s -> s

let () =
  assert (unwrap (K "hello") = "hello");
  let k = K "world" in
  assert (unwrap k = "world")
