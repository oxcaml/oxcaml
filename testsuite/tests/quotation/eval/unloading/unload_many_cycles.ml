(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Stress test: register and immediately drop many units in a row, with
   GC cycles in between. Verifies that:
     - Header color rotation works correctly across many cycles (the
       original GARBAGE-status assertion was a one-cycle stale-color
       bug; this would amplify any similar issue).
     - Buffer addresses can be reused between units without cross-talk.
     - Unloaded counters keep up with registered counters.
   Also exercises closures-returning-closures: each phrase produces a
   closure whose call returns another freshly-allocated closure that
   captures values. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

let[@inline never] one_round k =
  let mk =
    Eval.eval
      <[
        fun base ->
          fun delta -> base + delta
      ]>
  in
  let plus = mk k in
  let total = plus 1 + plus 2 + plus 3 in
  Printf.printf "round %d: total=%d\n" k total

let () =
  report "start";
  for k = 1 to 8 do
    one_round k;
    Gc.compact ()
  done;
  report "after 8 rounds + compacts";

  for _ = 0 to 4 do Gc.compact () done;
  report "after 5 idle Gc.compacts (no leaks)";

  (* Long-lived: keep one closure across many compacts, then drop. *)
  let kept = ref (fun x -> x + 1) in
  let[@inline never] make_kept () =
    kept := Eval.eval <[ fun x -> x * 3 + 1 ]>
  in
  make_kept ();
  for _ = 0 to 5 do
    Gc.compact ();
    let _ = !kept 10 in ()
  done;
  Printf.printf "kept !kept 7 = %d\n" (!kept 7);
  report "after 6 compacts holding kept";

  kept := (fun x -> x + 1);
  Gc.compact ();
  report "after release kept + Gc.compact"
