(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Hold several units alive at the same time, then drop them in a
   non-LIFO order. Verifies the unloadable-units linked list bookkeeping
   in [caml_unloadable_check_and_unload_dead] (the [link] / [*link]
   walk that splices unloaded entries out of the middle of the list). *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

let h0 : (int -> int) ref = ref (fun x -> x)
let h1 : (int -> int) ref = ref (fun x -> x)
let h2 : (int -> int) ref = ref (fun x -> x)
let h3 : (int -> int) ref = ref (fun x -> x)
let h4 : (int -> int) ref = ref (fun x -> x)

let[@inline never] populate () =
  h0 := Eval.eval <[ fun x -> x + 100 ]>;
  h1 := Eval.eval <[ fun x -> x + 200 ]>;
  h2 := Eval.eval <[ fun x -> x + 300 ]>;
  h3 := Eval.eval <[ fun x -> x + 400 ]>;
  h4 := Eval.eval <[ fun x -> x + 500 ]>

let dummy = (fun x -> x)

let () =
  report "start";
  populate ();
  Printf.printf "h0..h4 of 1: %d %d %d %d %d\n"
    (!h0 1) (!h1 1) (!h2 1) (!h3 1) (!h4 1);
  report "after populate";
  Gc.compact ();
  report "after Gc.compact (all 5 held)";

  (* Drop the middle one first. *)
  h2 := dummy;
  Gc.compact ();
  report "after drop h2";
  Printf.printf "h0..h4 of 2: %d %d %d %d %d\n"
    (!h0 2) (!h1 2) (!h2 2) (!h3 2) (!h4 2);

  (* Drop the head and the tail. *)
  h0 := dummy;
  h4 := dummy;
  Gc.compact ();
  report "after drop h0 and h4";
  Printf.printf "h1..h3 of 3: %d %d %d\n"
    (!h1 3) (!h2 3) (!h3 3);

  (* Drop the rest. *)
  h1 := dummy;
  h3 := dummy;
  Gc.compact ();
  report "after drop everything"
