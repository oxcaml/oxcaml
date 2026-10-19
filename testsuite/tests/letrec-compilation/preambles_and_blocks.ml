(* TEST *)

(* Preambles that do not refer to the group are evaluated once, before the
   group, in order with the other bindings that do not refer to the group. *)

let rec f =
  print_endline "f: preamble";
  let tbl = Hashtbl.create 7 in
  let rec go n = if n = 0 then 0 else (Hashtbl.replace tbl n (); f (n - 1)) in
  fun n -> ignore (Hashtbl.length tbl); go n + g n
and x = (print_endline "x"; 1)
and g =
  print_endline "g: preamble";
  fun n -> if n = 0 then x else f (n - 1)

let () = Printf.printf "f 3 = %d\n" (f 3)

(* Immutable blocks are allocated with placeholders and patched once the
   functions are defined. *)

type t = { name : string; next : unit -> t; data : int }
type v = V of (unit -> int) * v option | W of { w : int; v : v }

let rec a = { name = "a"; next = (fun () -> b); data = (print_endline "a"; 1) }
and b = { name = "b"; next = next_of_b; data = (print_endline "b"; 2) }
and next_of_b () = a
and v = V (count, Some (W { w = 3; v = V ((fun () -> 4), None) }))
and count () = a.data + b.data + (match v with V (_, Some (W { w; _ })) -> w
                                               | _ -> 0)
and tuple = (a, b, `Variant (Some count))
and r = ref (Some a)

let () =
  assert ((a.next ()) == b);
  assert ((b.next ()) == a);
  assert ((a.next ()).next () == a);
  let (a', b', `Variant c) = tuple in
  let c = Option.get c in
  assert (a' == a && b' == b);
  assert (match !r with Some a' -> a' == a | None -> false);
  Printf.printf "count = %d, %d\n" (count ()) (c ())

(* Local recursive definitions. *)

let test n =
  let rec s = { name = string_of_int n; next = (fun () -> s); data = n } in
  assert (s.next () == s);
  s.data

let () = Printf.printf "test = %d\n" (test 5 + test 6)

(* Lazy values nested in blocks. *)

type stream = Cons of int * stream Lazy.t

let rec evens = Cons (0, lazy (map (( + ) 1) odds))
and odds = Cons (1, lazy (map (( + ) 1) evens))
and map f (Cons (x, tl)) = Cons (f x, lazy (map f (Lazy.force tl)))

let rec take n (Cons (x, tl)) =
  if n = 0 then [] else x :: take (n - 1) (Lazy.force tl)

let () =
  print_endline
    (String.concat " " (List.map string_of_int (take 5 evens)))

(* Preambles may allocate locally: they are evaluated in a region. *)

let rec countdown =
  let _must_type_check = fun (x : int) -> x + 1 in
  fun y -> if y > 0 then countdown (y - 1) else 0

let () = Printf.printf "countdown = %d\n" (countdown 3)

(* Members of preambles may have the same name as members of the group. *)

let rec eval =
  let eval n = if n <= 0 then 0 else n + eval (n - 1) in
  fun n -> eval n

let () = Printf.printf "eval = %d\n" (eval 4)

(* Members of preambles are heap-allocated, even when they are unused. *)

let rec unused_members =
  let _f x = x, unused_members in
  let _b = ((fun () -> unused_members), 1) in
  fun n -> if n > 0 then unused_members (n - 1) else 0

let () = Printf.printf "unused_members = %d\n" (unused_members 2)
