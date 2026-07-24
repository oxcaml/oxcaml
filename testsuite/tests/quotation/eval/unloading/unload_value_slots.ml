(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Closures with free variables get value slots in the closure block
   after the function-slot prefix. The runtime closure-scan walks
   only the function-slot prefix for back-pointers, but the regular
   mark scan also walks the value-slot fields, so any heap allocations
   captured by the closure stay live too. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

let kept : (int -> int) ref = ref (fun x -> x)

let[@inline never] make_counter () =
  (* The eval'd code creates a closure capturing a mutable cell.
     The cell is allocated on the OCaml heap; the closure block
     itself is in the JIT'd unit. *)
  let f =
    Eval.eval
      <[
        let counter = ref 0 in
        fun n ->
          counter := !counter + n;
          !counter
      ]>
  in
  kept := f

let () =
  report "start";
  make_counter ();
  Printf.printf "counter += 1 => %d\n" (!kept 1);
  Printf.printf "counter += 2 => %d\n" (!kept 2);
  Printf.printf "counter += 10 => %d\n" (!kept 10);
  report "after make_counter";
  Gc.compact ();
  report "after Gc.compact (counter held)";

  Printf.printf "counter still works: counter += 5 => %d\n" (!kept 5);

  Gc.compact ();
  Gc.compact ();
  report "after two more Gc.compact (counter still held)";

  Printf.printf "counter still works: counter += 0 => %d\n" (!kept 0);

  kept := (fun x -> x);
  Gc.compact ();
  report "after release + Gc.compact"
