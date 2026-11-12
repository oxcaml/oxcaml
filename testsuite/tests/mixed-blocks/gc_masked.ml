(* TEST
 modules = "gc_masked_stubs.c";
 native;
*)

(* Push blocks with scan masks through the garbage collector: minor
   collections, incremental and full major collections, and
   compaction. The compiler only emits a few of the possible layouts,
   so a C stub builds blocks in all three masked header formats, with
   arbitrary masks, and checks afterwards that every unscannable word
   is bit-for-bit intact and that everything reachable through a
   scannable word is still there. *)

external build : int -> int -> Obj.t = "gc_masked_build"
external check : Obj.t -> int -> unit = "gc_masked_check"

let slots = 32 (* SLOTS in the stubs *)
let live : Obj.t option array = Array.make slots None

let check_slot seed s =
  match live.(s) with
  | None -> ()
  | Some b ->
    (try check b s with Failure m ->
       Printf.printf "seed %d, slot %d: %s\n" seed s m; exit 1)

let check_all seed = for s = 0 to slots - 1 do check_slot seed s done

(* Fill the major heap's recently freed small blocks with new ones, so
   that anything the collector wrongly freed is seen to have changed. *)
let churned = Array.make 4096 (ref 0)

let churn () =
  for i = 0 to Array.length churned - 1 do churned.(i) <- ref (-1 - i) done;
  Gc.minor ()

(* A mixed record in each of many stack frames, across a major
   collection: enough roots to overflow the mark stack, so that its
   entries for these blocks are compressed. *)
type r = { a : int ref; b : string; f : float#; g : float# }

let[@inline never] check_r (r : r) n =
  if !(r.a) <> n || r.b <> string_of_int n then begin
    Printf.printf "deep %d: record damaged\n" n; exit 1
  end

let rec deep n =
  let r = { a = ref n; b = string_of_int n; f = #1.5; g = #2.5 } in
  if n = 0 then (Gc.minor (); Gc.full_major (); churn ()) else deep (n - 1);
  check_r r n

let () =
  let n = if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1)
    else 3000 in
  for seed = 1 to n do
    let s = seed mod slots in
    let b = build seed s in
    live.(s) <- Some b;
    Gc.minor ();
    check_slot seed s;
    if seed mod 3 = 0 then ignore (Gc.major_slice 0 : int);
    if seed mod 97 = 0 then begin
      Gc.full_major (); churn (); check_all seed
    end;
    if seed mod 601 = 0 then begin
      Gc.compact (); churn (); check_all seed
    end
  done;
  Gc.full_major ();
  churn ();
  check_all n;
  deep 100_000;
  check_all n;
  print_endline "OK"
