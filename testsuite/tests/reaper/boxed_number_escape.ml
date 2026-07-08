(* TEST
   flambda2;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* [r] is exported by the .mli, so the store into it makes the boxed int64
   escape: it must not be unboxed, and a [%box_num] operation must remain in
   the output. *)

let r = ref 0L

let escape n =
  let b = Int64.of_int n in
  r := b;
  Int64.to_int b
