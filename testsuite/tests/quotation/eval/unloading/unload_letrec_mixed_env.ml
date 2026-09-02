(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Cross-product of [let rec] (multiple function slots with infix
   headers) and a closure env that mixes non-scannable words (captured
   int) with scannable words (captured ref). The closure-scan walks
   each function slot using the closinfo's arity (not by looking for
   following infix headers, which would conflate non-scannable env
   words with the inter-slot infix marker), then the regular field
   scan visits scannable env. Both paths must work together, with the
   GC properly skipping past Infix_tag headers. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

let () = Random.init 19

let f_kept = ref (fun _ -> 0)
let g_kept = ref (fun _ _ -> 0)

let[@inline never] populate () =
  (* Mutual recursion + non-scannable int captures + scannable ref. *)
  let evald = Eval.eval <[
    let bias_a = Random.int 100 in           (* int, non-scannable *)
    let bias_b = Random.int 100 in           (* int, non-scannable *)
    let counter = ref 0 in                   (* ref, scannable *)
    let label = "letrec-mixed" in            (* string, scannable *)
    let rec f n =
      counter := !counter + 1;
      if n <= 0 then bias_a + String.length label
      else g (n - 1) bias_b
    and g x y =
      counter := !counter + 1;
      if x <= 0 then bias_b + y
      else f (x - 1) + bias_a
    in
    (f, g)
  ]> in
  let (f, g) = evald in
  f_kept := f;
  g_kept := g

let () =
  report "start";
  populate ();
  Printf.printf "f 0 = %d, f 3 = %d, g 2 7 = %d\n"
    (!f_kept 0) (!f_kept 3) (!g_kept 2 7);
  report "after populate";

  Gc.compact ();
  Gc.compact ();
  Printf.printf "stable: f 5 = %d, g 4 1 = %d\n"
    (!f_kept 5) (!g_kept 4 1);
  report "after 2x Gc.compact (both held)";

  (* Drop f, keep g. They share a closure block, so the unit must
     stay alive while either is held. *)
  f_kept := (fun _ -> 0);
  Gc.compact ();
  Printf.printf "g still: %d\n" (!g_kept 3 5);
  report "after release f (g still held)";

  g_kept := (fun _ _ -> 0);
  Gc.compact ();
  report "after release g"
