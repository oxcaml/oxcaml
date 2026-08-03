(* TEST
   flambda2;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   { native with dump-simplify, dump-reaper; check-fexpr-dump; }
 *)

(* The continuation [joined] has a parameter [a] of value_kind boxed int32.
   [a] has usages (its flow is merged with that of [b] inside [merge_usages]),
   but none of those usages read the contents of an int32 box.  The reaper
   must therefore erase the [boxed_int32] value_kind of [a], since the values
   flowing into it may be replaced by poison values (which are not boxed
   int32s). *)

let f b x y =
  let[@inline never] [@local never] merge_usages u = u in
  let[@local] joined (a : int32) b =
    let _ = merge_usages a in
    let b = merge_usages b in
    Int64.add b b
  in
  if b
  then joined (Int32.add x x) (Int64.add y y)
  else joined (Int32.mul x x) (Int64.mul y y)
