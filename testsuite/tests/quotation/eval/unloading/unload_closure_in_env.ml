(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Eval'd code captures another closure (built inside the same eval'd
   unit) into the env of a returned closure. Both closure blocks live
   in the unit's data section. The returned (outer) closure's value
   slot points at the inner closure block; the runtime's regular field
   scan follows that pointer, and the inner closure's slot scan must
   then darken its own Code_block back-pointer. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

let composed = ref (fun _ -> 0)

let[@inline never] populate () =
  composed := Eval.eval <[
    let inner = fun x -> x * x in
    let twice f x = f (f x) in
    fun n -> twice inner n + 7
  ]>

let () =
  report "start";
  populate ();
  Printf.printf "composed 2 = %d\n" (!composed 2);
  Printf.printf "composed 3 = %d\n" (!composed 3);
  report "after populate";

  Gc.compact ();
  Gc.compact ();
  report "after 2x Gc.compact (held)";
  Printf.printf "stable: composed 4 = %d\n" (!composed 4);

  composed := (fun _ -> 0);
  Gc.compact ();
  report "after release"
