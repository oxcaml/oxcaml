(* TEST
 flambda2;
 {
   native;
 }{
   jsir;
 }
*)

(* Checks "for" loops whose bound is [max_int] or [min_int], and empty loops.
   JSIR keeps the counter as a tagged int and tests for the bound before
   incrementing it (see [Lambda_to_lambda_transforms.rec_catch_for_for_loop]),
   so a mistake there would either overflow and loop forever, or stop one
   iteration early. *)

let[@inline never] up lo hi =
  let lo = Sys.opaque_identity lo and hi = Sys.opaque_identity hi in
  let n = ref 0 and last = ref None in
  for i = lo to hi do
    incr n;
    last := Some i
  done;
  Printf.printf "up: iterations=%d last_is_hi=%b\n" !n
    (match !last with None -> false | Some i -> i = hi)

let[@inline never] down hi lo =
  let lo = Sys.opaque_identity lo and hi = Sys.opaque_identity hi in
  let n = ref 0 and last = ref None in
  for i = hi downto lo do
    incr n;
    last := Some i
  done;
  Printf.printf "down: iterations=%d last_is_lo=%b\n" !n
    (match !last with None -> false | Some i -> i = lo)

let () =
  up (max_int - 2) max_int;
  up max_int max_int;
  up min_int (min_int + 1);
  up 1 0;
  up max_int min_int;
  up 0 9;
  down (min_int + 2) min_int;
  down min_int min_int;
  down max_int (max_int - 1);
  down 0 1;
  down min_int max_int;
  down 9 0
