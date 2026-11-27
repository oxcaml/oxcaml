(* With [-flambda2-reaper], the float arguments of a probe handler are passed
   unboxed. There are more of them here than registers for floats, so that the
   wrapper of the probe copies some of them from one stack slot to another. *)

let floats = Array.init 18 float_of_int

let seen = Array.make 18 (-1.)

let[@inline never] fire () =
  let x0 = Array.unsafe_get floats 0 in
  let x1 = Array.unsafe_get floats 1 in
  let x2 = Array.unsafe_get floats 2 in
  let x3 = Array.unsafe_get floats 3 in
  let x4 = Array.unsafe_get floats 4 in
  let x5 = Array.unsafe_get floats 5 in
  let x6 = Array.unsafe_get floats 6 in
  let x7 = Array.unsafe_get floats 7 in
  let x8 = Array.unsafe_get floats 8 in
  let x9 = Array.unsafe_get floats 9 in
  let x10 = Array.unsafe_get floats 10 in
  let x11 = Array.unsafe_get floats 11 in
  let x12 = Array.unsafe_get floats 12 in
  let x13 = Array.unsafe_get floats 13 in
  let x14 = Array.unsafe_get floats 14 in
  let x15 = Array.unsafe_get floats 15 in
  let x16 = Array.unsafe_get floats 16 in
  let x17 = Array.unsafe_get floats 17 in
  [%probe
    "floats" ~enabled_at_init:true
      (Array.unsafe_set seen 0 x0;
       Array.unsafe_set seen 1 x1;
       Array.unsafe_set seen 2 x2;
       Array.unsafe_set seen 3 x3;
       Array.unsafe_set seen 4 x4;
       Array.unsafe_set seen 5 x5;
       Array.unsafe_set seen 6 x6;
       Array.unsafe_set seen 7 x7;
       Array.unsafe_set seen 8 x8;
       Array.unsafe_set seen 9 x9;
       Array.unsafe_set seen 10 x10;
       Array.unsafe_set seen 11 x11;
       Array.unsafe_set seen 12 x12;
       Array.unsafe_set seen 13 x13;
       Array.unsafe_set seen 14 x14;
       Array.unsafe_set seen 15 x15;
       Array.unsafe_set seen 16 x16;
       Array.unsafe_set seen 17 x17)]

let () =
  fire ();
  Array.iter (Printf.printf " %g") seen;
  print_newline ()
