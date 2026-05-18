(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  runtime5;
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Verify that holding a closure produced by Eval.eval keeps its unit live,
   and that dropping the closure allows the unit to be reclaimed. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

let kept : (int -> int) ref = ref (fun x -> x)

let[@inline never] eval_and_keep () =
  kept := Eval.eval <[ fun x -> x * 2 ]>

let[@inline never] eval_and_drop () =
  let f = Eval.eval <[ fun x -> x + 100 ]> in
  Printf.printf "Drop call: %d\n" (f 1)

let () =
  report "start";
  eval_and_keep ();
  Printf.printf "Kept call: %d\n" (!kept 21);
  report "after eval_and_keep";
  Gc.compact ();
  report "after Gc.compact (kept still live)";
  eval_and_drop ();
  report "after eval_and_drop";
  Gc.compact ();
  report "after Gc.compact (only dropped should unload)";
  (* Now release the kept closure too. *)
  kept := (fun x -> x);
  Gc.compact ();
  report "after release kept + Gc.compact"
