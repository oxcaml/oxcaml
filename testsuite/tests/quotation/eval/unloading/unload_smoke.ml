(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  runtime5;
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Smoke test for unloadable-CU GC: compile a closure-returning quotation,
   call it, drop the closure, force a major GC, and check that the unit was
   reclaimed. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

(* Wrap the eval+call in a function so the closure doesn't escape into a
   top-level binding (which would keep the unit live). *)
let[@inline never] run_once () =
  let f = Eval.eval <[ fun x -> x + 1 ]> in
  Printf.printf "Eval result: %d\n" (f 41)

let () =
  report "start";
  run_once ();
  report "after run_once";
  Gc.compact ();
  report "after Gc.compact"
