(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  runtime5;
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Partial application of a multi-arg eval'd closure produces a fresh
   closure (the curried partial-application object) that retains a
   pointer back into the original eval'd unit's code. The unit must
   stay live until the partial-application closure is also dropped.

   This exercises [closure_code_pointers = Full_and_partial_application]:
   each function slot is size 3, with the curry stub at offset 0, the
   closinfo at offset 1, and the eval'd function entry at offset 2.
   Holding the partial-application closure should prevent the unit
   being unloaded. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

let partial : (int -> int) ref = ref (fun x -> x)

let[@inline never] make_partial () =
  let f = Eval.eval <[ fun x y z -> (x * 100) + (y * 10) + z ]> in
  partial := f 4 5

let () =
  report "start";

  make_partial ();
  Printf.printf "Partial apply: %d\n" (!partial 6);
  report "after make_partial";
  Gc.compact ();
  report "after Gc.compact (partial held; unit must NOT unload)";

  (* The partial-application closure transitively keeps the original
     eval'd unit alive. Drop it now and confirm the unit unloads. *)
  partial := (fun x -> x);
  Gc.compact ();
  report "after release partial + Gc.compact"
