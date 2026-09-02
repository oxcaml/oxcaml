(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Mutually recursive closures live inside a single set-of-closures
   block: [F_0] (infix [F_k])* (env). The runtime's closure-scan
   walks every function slot in the prefix and darkens the [Code_block]
   for any whose closinfo has the unloadable bit set, distinguishing
   slot sizes via the following infix header (see
   [caml_is_infix_header_at]).

   This test exercises that walk:
     - even/odd: 1-arg mutual recursion (size-2 slots).
     - aaa/bbb: 2-arg mutual recursion (size-3 slots, size-3 slots
       require [slot_start + 2] indexing for the function entry).
   Holding the entry to one closure keeps both alive (they share a
   set-of-closures block); dropping releases the whole unit. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

let even_held : (int -> bool) ref = ref (fun _ -> false)
let aaa_held : (int -> int -> int) ref = ref (fun x _ -> x)

let[@inline never] keep_even () =
  let e =
    Eval.eval
      <[
        let rec even n = if n = 0 then true else odd (n - 1)
        and odd n = if n = 0 then false else even (n - 1) in
        even
      ]>
  in
  even_held := e

let[@inline never] keep_aaa () =
  let a =
    Eval.eval
      <[
        let rec aaa x y = if x = 0 then y else bbb (x - 1) (y * 2)
        and bbb x y = if x = 0 then y else aaa (x - 1) (y + 1) in
        aaa
      ]>
  in
  aaa_held := a

let () =
  report "start";

  keep_even ();
  Printf.printf "even 10 = %b, even 7 = %b\n" (!even_held 10) (!even_held 7);
  report "after keep_even";
  Gc.compact ();
  report "after Gc.compact (even held)";

  keep_aaa ();
  Printf.printf "aaa 0 5 = %d\n" (!aaa_held 0 5);
  Printf.printf "aaa 3 1 = %d\n" (!aaa_held 3 1);
  report "after keep_aaa";
  Gc.compact ();
  report "after Gc.compact (both held)";

  (* Drop even — its unit should unload, aaa's unit stays. *)
  even_held := (fun _ -> false);
  Gc.compact ();
  report "after release even + Gc.compact";

  (* Drop aaa too. *)
  aaa_held := (fun x _ -> x);
  Gc.compact ();
  report "after release aaa + Gc.compact"
