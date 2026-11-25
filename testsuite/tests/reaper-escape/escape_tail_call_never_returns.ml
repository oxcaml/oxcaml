(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -experimental-reaper-escape -reaper-debug-flags=nostamps";
   flags += " -O3 -flambda2-result-types-all-functions";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* With result types for all functions, Simplify knows that these functions do
   not return, and their calls have no return continuation. Such a call is
   still a tail call when the exception it raises is not handled in the
   function: the region holding [t] must be closed before it, or the local
   stack would grow along the chain of calls. In [handled], the exception is
   handled in the function: the call is not a tail call, and [t] is used by the
   handler, so the region stays open across it. *)
external local_stack_offset : unit -> int = "caml_local_stack_offset"

exception Done of int

let[@inline never] sum_pair (p : int * int) = fst p + snd p

let base = ref 0

let[@inline never] last n = raise (Done (local_stack_offset () - !base + n))

let[@inline never] f3 n =
  let t = n, n + 1 in
  let s = sum_pair t in
  last (n + s - s)

let[@inline never] f2 n =
  let t = n, n + 1 in
  let s = sum_pair t in
  f3 (n + s - s)

let[@inline never] f1 n =
  let t = n, n + 1 in
  let s = sum_pair t in
  f2 (n + s - s)

let[@inline never] handled n =
  let t = n, n + 1 in
  let s = sum_pair t in
  try last (n + s - s) with Done d -> d + sum_pair t

let () =
  base := local_stack_offset ();
  let r = try f1 (Sys.opaque_identity 0) with Done n -> n in
  let h = handled (Sys.opaque_identity 0) in
  (Printf.printf [@inlined never]) "stack growth %d; handled %d\n" r h
