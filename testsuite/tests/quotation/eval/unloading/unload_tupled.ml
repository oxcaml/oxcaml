(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Tupled functions ([fun (x, y) -> ...]) also classify as
   [Full_and_partial_application] regardless of arity, so they get a
   size-3 function slot. Verify the runtime closure-scan handles this. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

let kept : (int * int -> int) ref = ref (fun (x, _) -> x)

let[@inline never] keep_tupled () =
  kept := Eval.eval <[ fun (x, y) -> x * y ]>

let[@inline never] call_tupled_dropped () =
  let f = Eval.eval <[ fun (a, b, c) -> a + b + c ]> in
  Printf.printf "3-tuple call: %d\n" (f (1, 2, 3))

let () =
  report "start";
  keep_tupled ();
  Printf.printf "Kept 2-tuple call: %d\n" (!kept (6, 7));
  report "after keep_tupled";
  Gc.compact ();
  report "after Gc.compact (kept live)";

  call_tupled_dropped ();
  report "after call_tupled_dropped";
  Gc.compact ();
  report "after Gc.compact (drop unloads)";

  kept := (fun (x, _) -> x);
  Gc.compact ();
  report "after release kept + Gc.compact"
