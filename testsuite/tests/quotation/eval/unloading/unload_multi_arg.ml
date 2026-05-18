(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  runtime5;
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Multi-argument curried closures use size-3 function slots:
   [curry_stub; closinfo; code_symbol]. The [code_symbol] is at slot
   offset 2, not 0. The runtime closure-scan
   ([caml_darken_unloadable_code_blocks_in_closure]) must read the
   back-pointer from offset 2 in this case; offset 0 holds the curry stub
   which lives in non-unloadable runtime code. This test exercises that
   path. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

let kept : (int -> int -> int) ref = ref (fun x _ -> x)

let[@inline never] keep_two_arg () =
  kept := Eval.eval <[ fun x y -> x + y ]>

let[@inline never] call_three_arg_dropped () =
  let f = Eval.eval <[ fun x y z -> (x * 100) + (y * 10) + z ]> in
  Printf.printf "3-arg full apply: %d\n" (f 1 2 3)

let[@inline never] call_four_arg_dropped () =
  let f = Eval.eval <[ fun a b c d -> a + b + c + d ]> in
  Printf.printf "4-arg full apply: %d\n" (f 10 20 30 40)

let () =
  report "start";

  (* Kept multi-arg closure: unit must stay live across Gc.compact. *)
  keep_two_arg ();
  Printf.printf "Kept 2-arg full apply: %d\n" (!kept 7 35);
  Printf.printf "Kept 2-arg partial then full: %d\n"
    (let g = !kept 100 in g 23);
  report "after keep_two_arg";
  Gc.compact ();
  report "after Gc.compact (kept live)";

  (* Now also call dropped 3-arg and 4-arg closures. After return their
     units should unload. *)
  call_three_arg_dropped ();
  call_four_arg_dropped ();
  report "after dropped calls";
  Gc.compact ();
  report "after Gc.compact (drops only unload)";

  (* Release kept and verify it now unloads too. *)
  kept := (fun x _ -> x);
  Gc.compact ();
  report "after release kept + Gc.compact"
