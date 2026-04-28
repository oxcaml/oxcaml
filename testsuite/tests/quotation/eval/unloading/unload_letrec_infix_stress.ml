(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  runtime5;
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Stress test for closures with infix-tag pointers under repeated GC.
   We construct a 4-way mutually recursive bundle, hold infix-tag
   pointers to the 2nd, 3rd, and 4th functions (the 1st is the only
   non-infix entry), and run many compactions while invoking each. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

let () = Random.init 31

let f0 = ref (fun _ -> 0)
let f1 = ref (fun _ -> 0)
let f2 = ref (fun _ -> 0)
let f3 = ref (fun _ -> 0)

let[@inline never] populate () =
  let bundle = Eval.eval <[
    let off = Random.int 1000 in
    let counter = ref 0 in
    let rec a n =
      counter := !counter + 1;
      if n <= 0 then off else b (n - 1) + 1
    and b n =
      counter := !counter + 1;
      if n <= 0 then off + 1 else c (n - 1) + 2
    and c n =
      counter := !counter + 1;
      if n <= 0 then off + 2 else d (n - 1) + 3
    and d n =
      counter := !counter + 1;
      if n <= 0 then off + 3 else a (n - 1) + 4
    in
    (a, b, c, d)
  ]> in
  let (a, b, c, d) = bundle in
  f0 := a;  (* non-infix; first slot *)
  f1 := b;  (* infix-tagged into the closure block *)
  f2 := c;  (* infix-tagged *)
  f3 := d   (* infix-tagged *)

let calls () =
  Printf.sprintf "f0(2)=%d f1(2)=%d f2(2)=%d f3(2)=%d"
    (!f0 2) (!f1 2) (!f2 2) (!f3 2)

let () =
  report "start";
  populate ();
  Printf.printf "%s\n" (calls ());
  report "after populate";

  (* Hammer the GC with many compactions while invoking via each
     infix-tag pointer. Each call exercises the whole let-rec chain. *)
  let baseline = calls () in
  for _ = 1 to 10 do
    Gc.compact ();
    let now = calls () in
    assert (now = baseline)
  done;
  Printf.printf "stable across 10x Gc.compact: %s\n" baseline;
  report "after 10x compact (all held)";

  (* Drop them progressively. The unit should stay alive until the
     LAST infix pointer is released. *)
  f0 := (fun _ -> 0);
  Gc.compact ();
  report "after release f0 (f1/f2/f3 still hold via infix)";
  f1 := (fun _ -> 0);
  Gc.compact ();
  report "after release f1";
  f2 := (fun _ -> 0);
  Gc.compact ();
  Printf.printf "f3 still: %d\n" (!f3 3);
  report "after release f2 (only f3 held via infix)";
  f3 := (fun _ -> 0);
  Gc.compact ();
  report "after release f3"
