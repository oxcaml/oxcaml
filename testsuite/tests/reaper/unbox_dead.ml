(* TEST
 flambda2;
 flags += "-O3 -flambda2-reaper -reaper-debug-flags=nostamps";
 native with dump-reaper;
 check-fexpr-dump;
 check-program-output;
*)

(* Inside [go2], [b] is unboxed, since it is used by [r1]. However,
   it is unused in the second call to [go2]. After the [if], we then
   get a variable [r2] which is not dominated by [a], and which is dead.
   This variable must both be dead (it is unused) and unboxed (it is
   the result of a call to [go2]), which used to produce an invalid. *)
(*
 * let f x =
 *   let a = sqrt 2.0 in
 *   apply go2 (a) -> k1
 * where
 *   k1 (r1):
 *     if r1 > 1.0
 *     then goto k2
 *     else goto k3
 *   k2:
 *     let () = print_string "x" in
 *     goto k3
 *   k3:
 *     if x
 *     then goto k4 (3.0)
 *     else apply go2 (a) -> k4
 *   k4 (r2):
 *     apply go(r2) -> k5
 *   k5 (_):
 *     let () = print_string "y" in
 *     ret ()
 *)
let[@inline never] f x =
  let[@inline never][@local never] go b = b in
  let[@inline never][@local never] go2 b = b in
  let a = Float.sqrt 2.0 in
  let r1 = go2 a in
  if r1 > 1.0 then print_string "x";
  ignore (go (if x then 3.0 else go2 a));
  print_string "y"
let () = f false; print_string "z"