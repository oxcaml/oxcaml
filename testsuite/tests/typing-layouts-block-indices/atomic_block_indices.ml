(* TEST
 reference = "${test_source_directory}/atomic_block_indices.reference";
 flambda2;
 include stdlib_stable;
 {
   bytecode;
 }{
   native;
 } {
   flags = "-Oclassic";
   native;
 } {
   flags = "-O3";
   native;
 }
*)

open Stdlib_stable

type t = { x : int; mutable y : int [@atomic] }
type tstr = { s : string; mutable u : string [@atomic] }
type multi = { a : int; b : string; mutable c : int [@atomic]; d : int }

let () =
  let r = { x = 1; y = 2 } in
  let i = (.y) in
  Printf.printf "get y = %d\n" (Idx_atomic.get r i);
  Idx_atomic.set r i 10;
  Printf.printf "after set, get y = %d\n" (Idx_atomic.get r i);
  let old = Idx_atomic.exchange r i 20 in
  Printf.printf "exchange returned %d, get y = %d\n" old (Idx_atomic.get r i);
  let cas_ok = Idx_atomic.compare_and_set r i 20 30 in
  Printf.printf "cas (20->30) success=%b, get y = %d\n"
    cas_ok (Idx_atomic.get r i);
  let cas_fail = Idx_atomic.compare_and_set r i 99 40 in
  Printf.printf "cas (99->40) success=%b, get y = %d\n"
    cas_fail (Idx_atomic.get r i);
  let prev_add = Idx_atomic.fetch_and_add r i 5 in
  Printf.printf "fetch_and_add prev=%d, get y = %d\n"
    prev_add (Idx_atomic.get r i);
  Idx_atomic.add r i 100;
  Printf.printf "after add 100, get y = %d\n" (Idx_atomic.get r i);
  Idx_atomic.sub r i 5;
  Printf.printf "after sub 5, get y = %d\n" (Idx_atomic.get r i);
  Idx_atomic.set r i 0xff;
  Idx_atomic.logand r i 0x0f;
  Printf.printf "after set 0xff, logand 0x0f, get y = 0x%x\n"
    (Idx_atomic.get r i);
  Idx_atomic.logor r i 0xf0;
  Printf.printf "after logor 0xf0, get y = 0x%x\n" (Idx_atomic.get r i);
  Idx_atomic.logxor r i 0xff;
  Printf.printf "after logxor 0xff, get y = 0x%x\n" (Idx_atomic.get r i);
  Printf.printf "get_contended y = %d\n" (Idx_atomic.get_contended r i)

let () =
  let s = { s = "init"; u = "two" } in
  let j = (.u) in
  Printf.printf "get u = %s\n" (Idx_atomic.get s j);
  Idx_atomic.set s j "ten";
  Printf.printf "after set, get u = %s\n" (Idx_atomic.get s j);
  let prev = Idx_atomic.exchange s j "zero" in
  Printf.printf "exchange returned %s, get u = %s\n" prev (Idx_atomic.get s j)

let () =
  let m = { a = -1; b = "filler"; c = 7; d = 99 } in
  let k = (.c) in
  Printf.printf "multi: get c = %d\n" (Idx_atomic.get m k);
  Idx_atomic.set m k 77;
  Printf.printf "multi: after set, get c = %d\n" (Idx_atomic.get m k);
  let prev_ex = Idx_atomic.compare_exchange m k 77 88 in
  Printf.printf "multi: compare_exchange prev=%d, now c = %d\n"
    prev_ex (Idx_atomic.get m k);
  Printf.printf "multi: unchanged a=%d b=%s d=%d\n" m.a m.b m.d

(* Deepening through a singleton unboxed record containing a value. *)
type val_box = #{ v : int }
type tb = { filler : string; mutable boxed : val_box [@atomic] }

let () =
  let r = { filler = "_"; boxed = #{ v = 42 } } in
  let i = (.boxed) in
  let i2 = (.idx_atomic(i).#v) in
  Printf.printf "deepened: get = %d\n" (Idx_atomic.get r i2);
  Idx_atomic.set r i2 100;
  Printf.printf "deepened: after set, get = %d\n" (Idx_atomic.get r i2);
  let prev = Idx_atomic.exchange r i2 200 in
  Printf.printf "deepened: exchange returned %d, now = %d\n"
    prev (Idx_atomic.get r i2);
  let prev_add = Idx_atomic.fetch_and_add r i2 1 in
  Printf.printf "deepened: fetch_and_add 1 prev=%d, now = %d\n"
    prev_add (Idx_atomic.get r i2)

