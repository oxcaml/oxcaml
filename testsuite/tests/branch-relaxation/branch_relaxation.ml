(* TEST
 flags = "-dbranch-relaxation-max-displacement 32";
 arch_arm64;
 native;
*)

(* Regression test for the arm64 branch relaxation pass
   ([backend/branch_relaxation.ml] and [backend/arm64/emit.ml]).

   Relaxation only triggers for functions whose code spans more than the
   maximum branch displacement (~1MB), so exercising it directly would need a
   huge function.  The [-dbranch-relaxation-max-displacement] flag lowers that
   threshold, so this small function instead forces relaxation of:

   - far conditional branches (the [if] and the loop);
   - a far poll (the loop back-edge);
   - far allocations, both heap ([glob := b]) and local ([a] and [c]);
   - the prologue stack check (inserted because of the non-tail calls).

   Before the fixes, relaxing an already-relaxed instruction tripped an
   [assert false], the stack check had no far form, and local allocations were
   wrongly relaxed into far heap allocations. *)

let glob = ref []

let[@inline never] consume (local_ a) (local_ c) =
  (match a with x :: _ -> x | [] -> 0)
  + (match c with y :: _ -> y | [] -> 0)

let[@inline never] sink (x : int) = ignore (Sys.opaque_identity x)

let[@inline never] f n =
  let a = local_ [n] in
  let b = [n + 1] in
  let c = local_ [n + 2] in
  glob := b;
  let s = ref (consume a c) in
  for i = 0 to Sys.opaque_identity 3 do
    sink i;
    s := !s + i
  done;
  !s

let () = Printf.printf "%d\n" (f (Sys.opaque_identity 10))
