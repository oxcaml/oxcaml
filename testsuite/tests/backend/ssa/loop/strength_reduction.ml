(* TEST
 flags = "-O3 -ssa-strength-reduce -ssa-lftr";
 native;
*)

(* Exercises induction-variable strength reduction and linear-function test
   replacement. Each loop carries a derived induction variable (a scaled index
   or [i * const]) that these passes rewrite into a lockstep counter. The point
   is that the results stay correct regardless of the rewrite; a miscompilation
   would change the printed values. *)

let[@inline never] opaque x = Sys.opaque_identity x

(* Scaled offset [3 * i]: a derived IV with a non-trivial constant scale. *)
let[@inline never] sum_scaled (a : int array) (n : int) =
  let acc = ref 0 in
  for i = 0 to n - 1 do
    acc := !acc + a.(i) + (3 * i)
  done;
  !acc

(* Two independent scaled indices [a.(i)] and [b.(i)]. *)
let[@inline never] dot (a : int array) (b : int array) (n : int) =
  let acc = ref 0 in
  for i = 0 to n - 1 do
    acc := !acc + (a.(i) * b.(i))
  done;
  !acc

(* [i * const + const] fuses to a multiply-add on some targets; the value is
   passed through an opaque call so it is a maximal reducible expression. *)
let[@inline never] muladd (n : int) =
  let acc = ref 0 in
  for i = 0 to n - 1 do
    acc := !acc + opaque ((i * 5) + 3)
  done;
  !acc

(* A large coefficient stresses the immediate-range guard on the synthesized
   increment (a naive [Intop_imm (Iadd, step)] would be an illegal immediate). *)
let[@inline never] big_step (b : int array) (n : int) =
  for i = 0 to n - 1 do
    b.(i) <- i * 100_000_000
  done

(* Top-tested loop whose dead counter can be retired by test replacement once
   [i * 5 + 3] becomes a lockstep counter; guarded so the non-overflow proof can
   succeed. *)
let[@inline] lftr_body (limit : int) =
  let r = ref 0 and i = ref 0 in
  while !i < limit do
    r := !r + opaque ((!i * 5) + 3);
    incr i
  done;
  !r

let[@inline never] lftr (n : int) =
  if n >= 0 && n < 1_000 then lftr_body n else -1

let () =
  let n = 1000 in
  let a = Array.init n (fun i -> (i * 2) - 3) in
  let b = Array.init n (fun i -> (i mod 7) - 3) in
  Printf.printf "sum_scaled=%d\n" (sum_scaled a n);
  Printf.printf "dot=%d\n" (dot a b n);
  Printf.printf "muladd=%d\n" (muladd n);
  let c = Array.make 10 0 in
  big_step c 10;
  Printf.printf "big_step=%d %d %d\n" c.(0) c.(1) c.(9);
  Printf.printf "lftr=%d %d %d\n" (lftr 0) (lftr 100) (lftr 999)
