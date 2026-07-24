(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Closures capturing boxed floats. A boxed [float] is a pointer to a
   [Double_tag] block, so it sits in the closure's scannable env. Mark
   propagation has to follow the closure's value-slot field to that
   [Double_tag] block. (The unboxed-numerics case — [float#], [int64#]
   etc. inlined into the closure's non-scannable prefix — is exercised
   in [unload_unboxed_nums.ml].) *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

let () = Random.init 7

let f_unboxed = ref (fun n -> float_of_int n)
let f_boxed = ref (fun n -> n)

let[@inline never] populate () =
  (* Single float capture: typically unboxed into the closure's
     non-scannable env. *)
  f_unboxed := Eval.eval <[
    let scale = 1.0 +. Random.float 9.0 in
    fun n -> float_of_int n *. scale
  ]>;
  (* Float array capture: boxed/heap. *)
  f_boxed := Eval.eval <[
    let xs = Array.init 5 (fun _ -> Random.float 100.0) in
    fun n ->
      let s = ref 0.0 in
      Array.iter (fun x -> s := !s +. x) xs;
      n + int_of_float !s
  ]>

let () =
  report "start";
  populate ();
  Printf.printf "f_unboxed(10) = %.2f\n" (!f_unboxed 10);
  Printf.printf "f_unboxed(20) = %.2f\n" (!f_unboxed 20);
  let a = !f_boxed 100 in
  let b = !f_boxed 200 in
  Printf.printf "f_boxed: a=%d b=%d (b-a=%d)\n" a b (b - a);
  report "after populate";

  Gc.compact ();
  Gc.compact ();
  Printf.printf "stable: f_unboxed(10) = %.2f\n" (!f_unboxed 10);
  let c = !f_boxed 100 in
  assert (c = a);
  report "after 2x Gc.compact (held)";

  f_unboxed := (fun n -> float_of_int n);
  f_boxed := (fun n -> n);
  Gc.compact ();
  report "after release"
