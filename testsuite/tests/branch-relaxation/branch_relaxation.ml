(* TEST
 flags = "-dbranch-relaxation-max-displacement 16";
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
   - the prologue stack check (inserted because of the non-tail calls);
   - with stack checks disabled, the probe loop of a large frame ([f_probed]).

   Before the fixes, relaxing an already-relaxed instruction tripped an
   [assert false], the stack check had no far form, local allocations were
   wrongly relaxed into far heap allocations, and relaxing a stack check with
   stack checks disabled tripped an assertion instead of emitting probes. *)

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

(* With stack checks disabled, a frame of more than four guard-page strides
   gets a probe loop instead of a stack check (see [codegen/stack_probes.ml]):
   relaxing it must emit the probes again, not a far stack check.  The callee's
   large unboxed-product result comes back in a large outgoing stack area in
   [f_probed]'s frame. *)

type t2 = #(float# * float#)
type t4 = #(t2 * t2)
type t8 = #(t4 * t4)
type t16 = #(t8 * t8)
type t32 = #(t16 * t16)
type t64 = #(t32 * t32)
type t128 = #(t64 * t64)
type t256 = #(t128 * t128)
type t512 = #(t256 * t256)
type t1024 = #(t512 * t512)
type t2048 = #(t1024 * t1024)
type t4097 = #(float# * #(t2048 * t2048))

external box_float : float# -> (float[@local_opt]) = "%box_float"
external unbox_float : (float[@local_opt]) -> float# = "%unbox_float"

let[@inline never] g4097 (x : float#) : t4097 =
  let p2 = #(x, x) in
  let p4 = #(p2, p2) in
  let p8 = #(p4, p4) in
  let p16 = #(p8, p8) in
  let p32 = #(p16, p16) in
  let p64 = #(p32, p32) in
  let p128 = #(p64, p64) in
  let p256 = #(p128, p128) in
  let p512 = #(p256, p256) in
  let p1024 = #(p512, p512) in
  let p2048 = #(p1024, p1024) in
  #(x, #(p2048, p2048))

let[@inline never] f_probed (x : float#) : float# =
  let #(y, _) = g4097 x in
  let #(z, _) = g4097 y in
  z

let () = Printf.printf "%.0f\n" (box_float (f_probed (unbox_float 7.)))
