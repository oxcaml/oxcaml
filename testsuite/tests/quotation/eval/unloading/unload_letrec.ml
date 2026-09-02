(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* [let rec] inside eval'd code produces a closure block with multiple
   function slots separated by infix headers. Each slot has its own
   closinfo, and slot sizes can be mixed (some Full_application_only,
   some Full_and_partial_application). The closure-scan walks every
   slot and must darken each Code_block back-pointer correctly. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

let even_kept = ref (fun _ -> true)
let pair_kept = ref (fun (_, _) -> 0)

let[@inline never] populate () =
  (* Mutually recursive even/odd over a captured ref. The let-rec
     produces a closure block with two function slots (size 2 each
     since arity 1) plus an infix header between them. *)
  even_kept := Eval.eval <[
    let counter = ref 0 in
    let rec even n =
      counter := !counter + 1;
      if n = 0 then true else odd (n - 1)
    and odd n =
      counter := !counter + 1;
      if n = 0 then false else even (n - 1)
    in
    even
  ]>;
  (* Mix of slot sizes: a 1-arg function and a 2-tuple function. The
     1-arg gets size-2; the tupled gets size-3. *)
  pair_kept := Eval.eval <[
    let n_calls = ref 0 in
    let rec g (a, b) =
      n_calls := !n_calls + 1;
      if a <= 0 then b else h (a - 1) + b
    and h a =
      n_calls := !n_calls + 1;
      if a <= 0 then 0 else g (a - 1, a)
    in
    g
  ]>

let () =
  report "start";
  populate ();
  Printf.printf "even 7 = %b, even 8 = %b\n"
    (!even_kept 7) (!even_kept 8);
  Printf.printf "pair (3,10) = %d, pair (5,1) = %d\n"
    (!pair_kept (3, 10)) (!pair_kept (5, 1));
  report "after populate";

  Gc.compact ();
  Gc.compact ();
  report "after 2x Gc.compact (held)";
  Printf.printf "even 5 = %b, pair (4,7) = %d\n"
    (!even_kept 5) (!pair_kept (4, 7));

  even_kept := (fun _ -> true);
  pair_kept := (fun (_, _) -> 0);
  Gc.compact ();
  report "after release"
