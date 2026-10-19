(* TEST *)

(* Records bound by [let rec] refer to the group through mutable fields.
   The records are allocated first, in order; then the functions of the
   group are defined; then the fields referring to the group are
   assigned. *)

type cell = { mutable prev : cell; mutable succ : cell; data : int }

let rec a = { prev = b; succ = b; data = (print_endline "a allocated"; 0) }
and n = (print_endline "n evaluated"; 42)
and b = { prev = a; succ = a; data = (print_endline "b allocated"; 1) }
and length () = if a.succ == b && b.succ == a then 2 else -1

let () =
  Printf.printf "a.data = %d, b.data = %d, n = %d, length = %d\n"
    a.data b.data n (length ());
  assert (a.prev == b && a.succ == b && b.prev == a && b.succ == a)

(* Inline records, and functions and lazy values stored in the fields. *)

type node =
  | Node of
      { mutable parent : node;
        mutable children : node list Lazy.t;
        mutable depth : unit -> int;
        name : string }

let rec root =
  Node { parent = root; children = lazy [child]; depth = (fun () -> 0);
         name = "root" }
and child =
  Node { parent = root; children = lazy []; depth = (fun () -> 1 + depth root);
         name = "child" }
and depth (Node n) = n.depth ()

let () =
  (match root, child with
   | Node r, Node c ->
     assert (r.parent == root && c.parent == root);
     assert (List.hd (Lazy.force r.children) == child));
  Printf.printf "depth of child = %d\n" (depth child)

(* Local definitions allocate fresh records each time. *)

type t = { mutable next : t; v : int }

let mk v = let rec x = { next = x; v } in x

let () =
  let x1 = mk 1 and x2 = mk 2 in
  assert (x1 != x2 && x1.next == x1 && x2.next == x2);
  Printf.printf "x1.v = %d, x2.v = %d\n" x1.v x2.v

(* Flat float fields and mixed records. *)

type fr = { mutable fx : float; fy : float }
type mixed = { mutable m : mixed; u : float#; k : int }

external box_float : float# -> float = "%box_float"

let () =
  let rec one = 1.0
  and fr = { fx = one; fy = 2.0 }
  and mx = { m = mx; u = #3.0; k = 4 } in
  assert (mx.m == mx);
  Printf.printf "fr = (%g, %g), mx = (%g, %d)\n" fr.fx fr.fy
    (box_float mx.u) mx.k

(* Fields that do not refer to the group are evaluated at allocation time,
   whether or not they are mutable. *)

type r = { mutable self : r; mutable len : int; s : string }

let () =
  let rec r = { self = r; len = (print_endline "len evaluated"; 3); s = "abc" }
  and f () = print_endline "f called" in
  f ();
  assert (r.self == r);
  Printf.printf "r.len = %d, r.s = %s\n" r.len r.s

(* Reference cells are records with a single mutable field. *)

type named = { name : string; parent : named ref option }

let rec root = ref { name = "root"; parent = Some root }
and rename name = root := { !root with name }

let () =
  rename "renamed";
  match !root with
  | { name; parent = Some parent } ->
    assert (parent == root);
    print_endline name
  | { parent = None; _ } -> assert false

let () =
  let rec count = ref (fun n -> if n = 0 then 0 else !count (n - 1)) in
  Printf.printf "local = %d\n" (!count 3)

let rec cell = Atomic.make (fun n -> if n = 0 then 1 else (Atomic.get cell) 0)
let () = Printf.printf "atomic = %d\n" ((Atomic.get cell) 5)
