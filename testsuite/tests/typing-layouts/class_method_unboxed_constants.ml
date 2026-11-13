(* TEST
 include stdlib_stable;
 include stdlib_upstream_compatible;
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

(* Methods whose bodies apply a function to constants may be compiled using
   CamlinternalOO builtins, which store the constants in a block of values.
   This must not happen for unboxed constants. *)

module Float_u = Stdlib_upstream_compatible.Float_u
module Int32_u = Stdlib_upstream_compatible.Int32_u
module Int64_u = Stdlib_upstream_compatible.Int64_u
module Nativeint_u = Stdlib_upstream_compatible.Nativeint_u
module Float32_u = Stdlib_stable.Float32_u
module Int_u = Stdlib_stable.Int_u
module Int8_u = Stdlib_stable.Int8_u
module Int16_u = Stdlib_stable.Int16_u

let add_float_u x y = Float_u.to_int x + Float_u.to_int y
let add_int_u x (y : int) = Int_u.to_int x + y

class c = object (self)
  method float_u = (fun x -> Float_u.to_int x) #1.
  method float32_u = (fun x -> Float32_u.to_int x) #2.s
  method int32_u = (fun x -> Int32_u.to_int x) #3l
  method int64_u = (fun x -> Int64_u.to_int x) #4L
  method nativeint_u = (fun x -> Nativeint_u.to_int x) #5n
  method int_u = (fun x -> Int_u.to_int x) #6m
  method int8_u = (fun x -> Int8_u.to_int x) #7s
  method int16_u = (fun x -> Int16_u.to_int x) #8S
  method m x = Float_u.to_int x
  method meth_app = self#m #9.
  method app_two_consts = add_float_u #10. #11.
  method app_mixed_consts = add_int_u #12m 13
end

let () =
  let o = new c in
  Printf.printf "float_u: %d\n" o#float_u;
  Printf.printf "float32_u: %d\n" o#float32_u;
  Printf.printf "int32_u: %d\n" o#int32_u;
  Printf.printf "int64_u: %d\n" o#int64_u;
  Printf.printf "nativeint_u: %d\n" o#nativeint_u;
  Printf.printf "int_u: %d\n" o#int_u;
  Printf.printf "int8_u: %d\n" o#int8_u;
  Printf.printf "int16_u: %d\n" o#int16_u;
  Printf.printf "meth_app: %d\n" o#meth_app;
  Printf.printf "app_two_consts: %d\n" o#app_two_consts;
  Printf.printf "app_mixed_consts: %d\n" o#app_mixed_consts
