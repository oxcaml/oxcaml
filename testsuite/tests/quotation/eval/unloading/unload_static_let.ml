(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Eval'd code with a [let x = <non-constant>] before returning a
   closure. The bound value is computed at eval'd-unit init time. The
   closure body references [x] so the binding must outlive the unit's
   initialiser and stay reachable for as long as the closure does.

   This exercises the case where the eval'd unit's initialiser produces
   a value that ends up in a captured value-slot of the returned
   closure (rather than, say, being a plain top-level constant).

   We use a deterministic "random" so the test is reproducible. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

(* Seed deterministically so the test reference is stable across runs. *)
let () = Random.init 42

let f1 = ref (fun n -> n)
let f2 = ref (fun n -> n)
let f3 = ref (fun n -> n)

let[@inline never] populate () =
  (* Simple: add a precomputed offset. *)
  f1 := Eval.eval <[
    let x = Random.int 1000 in
    fun n -> n + x
  ]>;
  (* String allocated once at init, used in body via length. *)
  f2 := Eval.eval <[
    let s = "precomputed-" ^ string_of_int (Random.int 1000) in
    fun n -> n + String.length s
  ]>;
  (* Heap-allocated array filled once at init, summed each call. *)
  f3 := Eval.eval <[
    let a = Array.init 5 (fun _ -> Random.int 100) in
    fun n -> n + Array.fold_left (+) 0 a
  ]>

let () =
  report "start";
  populate ();
  let v1a = !f1 0 in
  let v2a = !f2 0 in
  let v3a = !f3 0 in
  Printf.printf "f1(0)=%d f2(0)=%d f3(0)=%d\n" v1a v2a v3a;
  report "after populate";

  (* Multiple cycles with the closures held: results must remain stable
     because the captured values must not be reclaimed. *)
  Gc.compact ();
  Gc.compact ();
  Gc.compact ();
  let v1b = !f1 0 in
  let v2b = !f2 0 in
  let v3b = !f3 0 in
  assert (v1a = v1b);
  assert (v2a = v2b);
  assert (v3a = v3b);
  Printf.printf "stable across 3x Gc.compact: f1=%d f2=%d f3=%d\n"
    v1b v2b v3b;
  report "after 3x compact (held)";

  (* Drop and verify unload. *)
  f1 := (fun n -> n);
  f2 := (fun n -> n);
  f3 := (fun n -> n);
  Gc.compact ();
  report "after release + Gc.compact"
