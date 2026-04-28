(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  runtime5;
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Cross of [let rec] (multiple function slots, infix headers) with
   the inter-unit curry-stub-on-heap pattern. The eval'd let-rec
   bundle in unit U_A is only reachable via a partial-application
   curry stub from unit U_B; if the unit-registration "born marked"
   fix (or the slot-size detection fix) regresses, U_A unloads while
   the stub still holds an infix-tag pointer into it, and the next
   call segfaults. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

let composed = ref (fun _ -> 0)

let[@inline never] populate () =
  (* In U_A: let-rec returning the SECOND function (b), so the value
     held is an Infix_tag pointer mid-block. *)
  let h = Eval.eval <[
    let rec a n = if n <= 0 then 0 else b (n - 1) + 1
    and b n = if n <= 0 then 100 else a (n - 1) + 2
    in
    b
  ]> in
  (* In U_B: a 2-arg function that wraps a callback. *)
  let mk = Eval.eval <[
    fun (cb : int -> int) n -> cb (cb n) + 7
  ]> in
  (* Partial application: [mk h] allocates a heap curry stub holding
     [h] (an Infix_tag pointer into U_A) at field 3. After populate
     returns, only [composed] keeps both U_A and U_B alive. *)
  composed := mk h

let dummy = (fun n -> n)

let () =
  report "start";
  populate ();
  Printf.printf "composed 0 = %d\n" (!composed 0);
  Printf.printf "composed 4 = %d\n" (!composed 4);
  report "after populate";

  Gc.compact ();
  Gc.compact ();
  Printf.printf "stable: composed 5 = %d\n" (!composed 5);
  report "after 2x Gc.compact (composed held)";

  composed := dummy;
  Gc.compact ();
  report "after release composed"
