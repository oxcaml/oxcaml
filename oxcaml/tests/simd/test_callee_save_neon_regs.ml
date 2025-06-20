module Builtins = Builtins_u
module Utils = Utils_u
open Utils
include Builtins.Float32x4

let[@inline never] check_binop _msg _scalar _vector f0 f1 =
  let msg = Sys.opaque_identity "add" in
  let scalar = Float32.add in
  let vector = add in
  let r0 = scalar f0 f1 in
  let r1 = scalar f1 f0 in
  let expect = Float32.to_float32x4 r0 r1 r0 r1 in
  let v1 = Float32.to_float32x4 f0 f1 f0 f1 in
  let v2 = Float32.to_float32x4 f1 f0 f1 f0 in
  let result = vector v1 v2 in
  (failmsg
     := fun () ->
          (Printf.printf [@inlined never]) "check_binop32 %s %lx %lx\n%!" msg f0
            f1);
  eq_float32x4 ~result ~expect

let () = Float32.check_floats (check_binop "add" Float32.add add)
