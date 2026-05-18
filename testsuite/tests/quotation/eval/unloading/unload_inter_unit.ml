(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  runtime5;
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Cross-unit interaction: one eval'd unit produces a closure that the
   user passes to another eval'd unit's closure as a callback. While
   the second unit is alive holding the callback, the first unit's
   code is still being indirectly invoked (through the callback). The
   GC mark should follow value-slot pointers from the second unit's
   closure to the first unit's closure (a heap-shape closure block in
   the first unit's data section), keeping both alive.

   When we drop the second unit's closure, both should unload (the
   first was only kept alive transitively through the second). *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

let composed : (int -> int) ref = ref (fun x -> x)

let[@inline never] populate () =
  let f1 = Eval.eval <[ fun x -> x * 2 ]> in
  let mk_g = Eval.eval <[ fun (h : int -> int) y -> h (h y) + 1 ]> in
  composed := mk_g f1

let dummy = (fun x -> x)

let () =
  report "start";
  populate ();
  Printf.printf "composed 5 = %d\n" (!composed 5);
  Printf.printf "composed 10 = %d\n" (!composed 10);
  report "after populate";
  Gc.compact ();
  report "after Gc.compact (composed held — both units must stay)";

  Printf.printf "composed still works: %d\n" (!composed 7);

  composed := dummy;
  Gc.compact ();
  report "after release composed + Gc.compact"
