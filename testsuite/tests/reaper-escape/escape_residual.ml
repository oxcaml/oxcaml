(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* A function made to return at mode local that cannot have a region of its own
   (see [Escape_exclave]) allocates in the region of its caller. Its local
   allocations that are not returned (the residual allocations) are made there
   too, as the type checker does under [exclave_]; with
   [-X reaper-escape-heap-residual=1] they are left on the heap, except the
   ones that may contain a local value, which have to stay local. [used]
   reports how much of the local stack is in use. *)
external local_stack_offset : unit -> int = "caml_local_stack_offset"

type triple =
  { a : int;
    b : int;
    c : int
  }

let[@inline never] sum3 t = t.a + t.b + t.c

let[@inline never] sum_pair (p : int * int) = fst p + snd p

let[@inline never] first (p : (int * int) * int) = fst (fst p) + snd p

let base = ref 0

let[@inline never] used () = local_stack_offset () - !base

module M : sig
  val run : int -> unit
end = struct
  (* [t] is used after the result has been allocated, so the function has no
     region of its own: [t] is a residual allocation. *)
  let[@inline never] plain x y =
    let r = x, y in
    let s = sum_pair r in
    let t = { a = s; b = y; c = x } in
    let u = sum3 t in
    if u > 0 then r else (y, x)

  (* Here [t] contains the result, which is local, so it stays local. *)
  let[@inline never] containing x y =
    let r = x, y in
    let t = r, x in
    let s = first t in
    if s > 0 then r else (y, x)

  let[@inline never] run n =
    base := local_stack_offset ();
    let p = plain n 1 in
    let u1 = used () in
    let q = containing n 2 in
    let u2 = used () in
    (Printf.printf [@inlined never]) "%d %d; used %d %d\n" (sum_pair p)
      (sum_pair q) u1 (u2 - u1)
end

let () = M.run 10
