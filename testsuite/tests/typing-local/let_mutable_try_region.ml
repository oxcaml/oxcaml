(* TEST
 flags = "-extension let_mutable";
 native;
*)

(* The handler of a [try] frees everything stack-allocated in the body. A
   value assigned there to a mutable variable bound outside the [try] is still
   reachable afterwards, so it must not be stack-allocated in the body: the
   type checker has to force [{ a = n }] onto the heap. If it were freed, [y]
   would reuse its stack slot. Reads go through [get] so they are not resolved
   statically. The expected output is 7. *)

type r = { mutable a : int }

let[@inline never] get (local_ r : r) = r.a

let[@inline never] f n =
  let mutable x = { a = 1 } in
  (try
     x <- { a = n };
     raise Exit
   with Exit -> ());
  let y = { a = 0 } in
  get x + get y

let () = Printf.printf "%d\n" (f 7)
