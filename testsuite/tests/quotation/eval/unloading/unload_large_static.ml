(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  runtime5;
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Eval'd code that accumulates a large amount of statically-allocated
   data: many string literals, several constant lists. Each of these
   becomes its own static block in the unit's data section and is
   tracked as a [data_block]. The unit's [data_blocks] array can become
   large — exercise the iteration over it during the unload pass. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

let lookup = ref (fun _ -> "")

let[@inline never] populate () =
  lookup := Eval.eval <[
    fun n ->
      match n with
      | 0 -> "alpha"
      | 1 -> "bravo"
      | 2 -> "charlie"
      | 3 -> "delta"
      | 4 -> "echo"
      | 5 -> "foxtrot"
      | 6 -> "golf"
      | 7 -> "hotel"
      | 8 -> "india"
      | 9 -> "juliet"
      | 10 -> "kilo"
      | 11 -> "lima"
      | 12 -> "mike"
      | 13 -> "november"
      | 14 -> "oscar"
      | 15 -> "papa"
      | _ -> "unknown"
  ]>

let () =
  report "start";
  populate ();
  for i = 0 to 16 do
    Printf.printf "%d -> %s\n" i (!lookup i)
  done;
  report "after populate";

  Gc.compact ();
  Gc.compact ();
  Gc.compact ();
  Printf.printf "stable: 7=%s 13=%s\n" (!lookup 7) (!lookup 13);
  report "after 3x Gc.compact (held)";

  lookup := (fun _ -> "");
  Gc.compact ();
  report "after release"
