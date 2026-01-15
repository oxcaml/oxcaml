(* TEST
 reference = "${test_source_directory}/obj_raw_field.reference";
 flambda2;
 {
   bytecode;
 }{
   native;
 }{
   flags = "-Oclassic";
   native;
 }{
   flags = "-O3";
   native;
 }
*)

type immutable_t = { a : int; b : int; c : int }

let () =
  print_endline "All-immutable record";
  let r = { a = 1; b = 2; c = 3 } in
  let obj = Obj.repr r in
  let raw0 = Obj.raw_field obj 0 in
  let raw1 = Obj.raw_field obj 1 in
  let raw2 = Obj.raw_field obj 2 in
  Printf.printf "raw_field 0: %nd\n" raw0;
  Printf.printf "raw_field 1: %nd\n" raw1;
  Printf.printf "raw_field 2: %nd\n" raw2

type mixed_t = { x : int; mutable y : int; z : int }

let () =
  print_endline "\nMixed mutable/immutable record";
  let r = { x = 10; y = 20; z = 30 } in
  let obj = Obj.repr r in
  Printf.printf "raw_field 0 (immut): %nd\n" (Obj.raw_field obj 0);
  Printf.printf "raw_field 1 (mut): %nd\n" (Obj.raw_field obj 1);
  Printf.printf "raw_field 2 (immut): %nd\n" (Obj.raw_field obj 2);
  Obj.set_raw_field obj 1 41n;  (* 41n = tagged 20 *)
  Printf.printf "after set y: %d\n" r.y

type mutable_t = { mutable p : int; mutable q : int; mutable r : int }

let () =
  print_endline "\nAll-mutable record";
  let r = { p = 100; q = 200; r = 300 } in
  let obj = Obj.repr r in
  Printf.printf "initial: p=%d q=%d r=%d\n" r.p r.q r.r;
  let raw0 = Obj.raw_field obj 0 in
  let raw1 = Obj.raw_field obj 1 in
  let raw2 = Obj.raw_field obj 2 in
  Printf.printf "raw: %nd %nd %nd\n" raw0 raw1 raw2;
  Obj.set_raw_field obj 0 raw2;
  Obj.set_raw_field obj 2 raw0;
  Printf.printf "after swap: p=%d q=%d r=%d\n" r.p r.q r.r

type float_t = { mutable f1 : float; mutable f2 : float; mutable f3 : float }

let () =
  print_endline "\nAll-float record";
  let r = { f1 = 1.5; f2 = 2.5; f3 = 3.5 } in
  let obj = Obj.repr r in
  Printf.printf "initial: f1=%g f2=%g f3=%g\n" r.f1 r.f2 r.f3;
  let raw0 = Obj.raw_field obj 0 in
  let raw1 = Obj.raw_field obj 1 in
  let raw2 = Obj.raw_field obj 2 in
  Printf.printf "raw: %nd %nd %nd\n" raw0 raw1 raw2;
  Obj.set_raw_field obj 0 raw2;
  Obj.set_raw_field obj 2 raw0;
  Printf.printf "after swap: f1=%g f2=%g f3=%g\n" r.f1 r.f2 r.f3
