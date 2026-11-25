(* TEST
 {
   flags = "-no-float-op-fusion";
   native;
 }{
   arch_amd64;
   {
     flags = "-float-op-fusion";
     native;
   }{
     native;
   }
 }{
   arch_arm64;
   reference = "${test_source_directory}/float_op_fusion.arm64.reference";
   {
     flags = "-float-op-fusion";
     native;
   }{
     native;
   }
 }
*)

(* [-[no-]float-op-fusion] controls whether floating-point operations may be
   fused into single operations that round only once. It is on by default, but
   only arm64 performs such fusion (into fused multiply-add/sub, tested here),
   hence its separate reference file.

   [a *. b] is exactly [1 - 2^-60], which rounds to [1.0]. Adding or
   subtracting [1.0] therefore gives zero if the product is rounded first, and
   [+/- 2^-60] if the operations are fused. *)

let a = Sys.opaque_identity 0x1.00000004p+0 (* 1 + 2^-30 *)

let b = Sys.opaque_identity 0x1.fffffff8p-1 (* 1 - 2^-30 *)

let one = Sys.opaque_identity 1.0

let minus_one = Sys.opaque_identity (-1.0)

let[@inline never] mul_add a b c = (a *. b) +. c

let[@inline never] add_mul a b c = c +. (a *. b)

let[@inline never] mul_sub a b c = (a *. b) -. c

let[@inline never] sub_mul a b c = c -. (a *. b)

let () =
  Printf.printf "mul_add: %h\n" (mul_add a b minus_one);
  Printf.printf "add_mul: %h\n" (add_mul a b minus_one);
  Printf.printf "mul_sub: %h\n" (mul_sub a b one);
  Printf.printf "sub_mul: %h\n" (sub_mul a b one)
