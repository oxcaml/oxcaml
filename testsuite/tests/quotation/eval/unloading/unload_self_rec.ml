(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Self-recursive function inside eval'd code. The recursive call goes
   to a fixed PC inside the eval'd unit's text section, so its stack
   frames keep the unit alive via the F.2 (frame-descriptor) scan even
   if the closure value were lost — but here we hold the closure too,
   exercising the F.1 (closure-scan) path as well. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

let kept = ref (fun _ -> 0)

let[@inline never] populate () =
  kept := Eval.eval <[
    let limit = 20 in
    let rec sum n = if n <= 0 then 0 else n + sum (n - 1) in
    fun n -> sum (min n limit)
  ]>

let () =
  report "start";
  populate ();
  Printf.printf "sum-clamped 5 = %d\n" (!kept 5);
  Printf.printf "sum-clamped 100 = %d\n" (!kept 100);
  report "after populate";

  Gc.compact ();
  report "after Gc.compact (held)";
  Printf.printf "stable: %d\n" (!kept 10);

  (* Trigger compactions repeatedly while invoking deeply. *)
  for _ = 1 to 5 do
    let _ = !kept 20 in
    Gc.compact ()
  done;
  report "after stress + Gc.compact (held)";
  Printf.printf "still works: %d\n" (!kept 19);

  kept := (fun _ -> 0);
  Gc.compact ();
  report "after release"
