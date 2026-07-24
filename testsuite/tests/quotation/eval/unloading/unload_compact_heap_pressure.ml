(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Heap-pressure compaction. Many short-lived heap allocations are
   made between [Gc.compact ()] calls so the compactor finds pool
   pages to evacuate; a long-lived heap closure into JIT'd code is
   held throughout. Failures here would be silent corruption of the
   closure's env (returned int wrong / crash on env field
   dereference) or false unloads (live count drops while still
   held). *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

(* Closure captures [xs] and [s] (lifted constants in the JIT
   unit's static data) so its env carries two pointers into the
   unit's [.data]. Every call must return the same constant; if the
   compactor corrupts either env field, the call would either
   crash or return wrong data. *)
let kept : (int -> int) ref = ref (fun n -> n)

let[@inline never] populate () =
  kept := Eval.eval <[
    let xs = [11; 22; 33; 44; 55] in
    let s = "kept-static-string" in
    fun n ->
      String.length s + List.length xs + List.fold_left (+) 0 xs + n
  ]>

let expected n =
  String.length "kept-static-string" + 5 + (11 + 22 + 33 + 44 + 55) + n

(* Allocate enough heap garbage to make the compactor see
   evacuation candidates. ~256 KiB per call. *)
let[@inline never] make_garbage () =
  for _ = 1 to 8 do
    let _ : int array = Array.init 1024 (fun i -> i) in
    let _ : bytes = Bytes.create 2048 in
    let _ : string list = List.init 64 (fun i -> string_of_int i) in
    ()
  done

let () =
  report "start";
  populate ();
  report "after populate";
  let bad = ref 0 in
  for k = 1 to 8 do
    make_garbage ();
    Gc.compact ();
    let got = !kept k in
    let want = expected k in
    if got <> want then begin
      incr bad;
      Printf.printf "FAIL at %d: got=%d want=%d\n" k got want
    end
  done;
  if !bad = 0 then
    Printf.printf "kept produced correct result across 8 pressure compacts\n";
  report "after 8 pressure compacts (kept held)";
  kept := (fun n -> n);
  Gc.full_major ();
  Gc.compact ();
  report "after release"
