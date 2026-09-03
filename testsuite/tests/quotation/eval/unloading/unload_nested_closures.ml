(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Nested closures: an outer eval'd closure returns inner closures that
   capture both static data and outer parameters. The inner closures
   live in the eval'd unit's text/data too, so the unit must stay alive
   for as long as ANY inner closure is held. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

let () = Random.init 99

let outer_kept = ref (fun _ -> fun _ -> 0)
let inner_only = ref (fun _ -> 0)

let[@inline never] populate () =
  (* Curried: outer takes [scale], returns inner that uses scale and a
     captured static-data offset. The inner closure block lives in the
     eval'd unit; its environment includes both [scale] (passed at call
     time) and [offset] (captured from the outer let). *)
  outer_kept := Eval.eval <[
    let offset = Random.int 100 in
    fun scale n -> n * scale + offset
  ]>;
  (* Drop the outer immediately; keep only an inner closure. The inner
     captures [offset] from the eval'd unit's static data and [base]
     from the call site. The unit must stay alive while [inner_only]
     is held. *)
  let outer = Eval.eval <[
    let offset = Random.int 100 in
    fun base ->
      fun n -> n + base + offset
  ]> in
  inner_only := outer 1000

let () =
  report "start";
  populate ();
  Printf.printf "outer 3 5 = %d\n" (!outer_kept 3 5);
  Printf.printf "outer 10 7 = %d\n" (!outer_kept 10 7);
  Printf.printf "inner_only 1 = %d\n" (!inner_only 1);
  Printf.printf "inner_only 2 = %d\n" (!inner_only 2);
  report "after populate";

  Gc.compact ();
  Gc.compact ();
  report "after 2x Gc.compact (both held)";
  Printf.printf "stable: outer 1 1 = %d, inner_only 0 = %d\n"
    (!outer_kept 1 1) (!inner_only 0);

  (* Drop the outer; inner_only still holds its eval'd unit. *)
  outer_kept := (fun _ -> fun _ -> 0);
  Gc.compact ();
  report "after release outer (inner_only still held)";
  Printf.printf "inner_only still: %d\n" (!inner_only 0);

  inner_only := (fun _ -> 0);
  Gc.compact ();
  report "after release inner_only too"
