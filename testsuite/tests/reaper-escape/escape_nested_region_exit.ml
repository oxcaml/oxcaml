(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -X reaper-escape=1";
   native;
 *)

(* Exiting the last loop iteration also exits the function's region, so the
   local stack must return to the function's checkpoint. Restoring the inner
   region's saved pointer last would retain [p] after every call. Measuring the
   local stack catches this even though the function's result is unaffected. *)
external local_stack_offset : unit -> int = "caml_local_stack_offset"

let[@inline never] sum_pair (p : int * int) = fst p + snd p

let[@inline never] f n =
  let p = n, 1 in
  let stop = sum_pair p in
  for i = 1 to stop do
    let q = i, i in
    ignore (sum_pair q)
  done

(* On an exit leaving only the iteration region, [p] must remain live. The
   allocation of [r] after the loop would overwrite it if we closed too far. *)
let[@inline never] keep_outer n =
  let p = n, n + 1 in
  for i = 1 to 2 do
    let q = i, i in
    ignore (sum_pair q)
  done;
  let r = n + 10, n + 20 in
  sum_pair r + sum_pair p

let () =
  let before = local_stack_offset () in
  f (Sys.opaque_identity (-1));
  let after = local_stack_offset () in
  (Printf.printf [@inlined never]) "empty loop: %d\n" (after - before);
  let before = local_stack_offset () in
  f (Sys.opaque_identity 1);
  let after = local_stack_offset () in
  (Printf.printf [@inlined never]) "nonempty loop: %d\n" (after - before);
  let before = local_stack_offset () in
  for i = 1 to 1000 do
    f (Sys.opaque_identity 1)
  done;
  let after = local_stack_offset () in
  (Printf.printf [@inlined never]) "repeated calls: %d\n" (after - before);
  let before = local_stack_offset () in
  let result = keep_outer (Sys.opaque_identity 10) in
  let after = local_stack_offset () in
  (Printf.printf [@inlined never]) "live outer region: %d; stack growth %d\n"
    result (after - before)
