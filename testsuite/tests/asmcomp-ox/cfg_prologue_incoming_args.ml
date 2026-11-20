(* TEST
 flambda2;
 {
   flags += " -O3 -cfg-prologue-shrink-wrap";
   native;
 }{
   flags += " -O3 -no-cfg-prologue-shrink-wrap";
   native;
 }
*)

(* Arguments [a80] and [a81] are passed on the stack on both amd64 and arm64.
   Shrink wrapping used to let [cond_call] read incoming arguments before the
   prologue, using offsets that assumed an allocated frame. [leaf] checks
   that genuinely frameless functions can still read incoming arguments. *)

let calls = ref 0

let[@inline never] slow x =
  incr calls;
  x

let[@inline never] cond_call a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13
    a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31
    a32 a33 a34 a35 a36 a37 a38 a39 a40 a41 a42 a43 a44 a45 a46 a47 a48 a49
    a50 a51 a52 a53 a54 a55 a56 a57 a58 a59 a60 a61 a62 a63 a64 a65 a66 a67
    a68 a69 a70 a71 a72 a73 a74 a75 a76 a77 a78 a79 a80 a81 =
  if a0 > 1000 then slow a79 + slow a80 else a81

let[@inline never] leaf a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14
    a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32
    a33 a34 a35 a36 a37 a38 a39 a40 a41 a42 a43 a44 a45 a46 a47 a48 a49 a50
    a51 a52 a53 a54 a55 a56 a57 a58 a59 a60 a61 a62 a63 a64 a65 a66 a67 a68
    a69 a70 a71 a72 a73 a74 a75 a76 a77 a78 a79 a80 a81 =
  if a0 > 1000 then a80 else a81

let[@inline never] run_cond_call x =
  cond_call (Sys.opaque_identity x) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17
    18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42
    43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67
    68 69 70 71 72 73 74 75 76 77 78 79 80 81

let[@inline never] run_leaf x =
  leaf (Sys.opaque_identity x) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18
    19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43
    44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68
    69 70 71 72 73 74 75 76 77 78 79 80 81

let () =
  assert (run_cond_call 0 = 81);
  assert (!calls = 0);
  assert (run_cond_call 2000 = 159);
  assert (!calls = 2);
  assert (run_leaf 0 = 81);
  assert (run_leaf 2000 = 80)
