(* TEST
 probes;
 arch_amd64;
 flambda2;
 {
   native;
 }{
   flags = "-O3";
   native;
 }{
   flags = "-Oclassic";
   native;
 }{
   flags = "-dcfg-invariants";
   native;
 }{
   flags = "-O3 -dcfg-invariants";
   native;
 }
*)

(* The handler of a probe may never return, but a disabled probe does not
   call its handler, so execution must continue after the probe. *)

let[@inline never] disabled_string () : string =
  try
    [%probe "p1" (raise Exit)];
    "returned normally"
  with Exit -> "probe raised"

let[@inline never] disabled_int () : int =
  try
    [%probe "p2" (raise Exit)];
    42
  with Exit -> 0

let[@inline never] disabled_float x : float =
  try
    [%probe "p3" (raise Exit)];
    x +. 1.
  with Exit -> 0.

let[@inline never] disabled_no_handler x : int =
  [%probe "p4" (raise Exit)];
  x + 1

let[@inline never] enabled_int () : int =
  try
    [%probe "p5" ~enabled_at_init:true (raise Exit)];
    42
  with Exit -> 0

let () =
  print_endline (disabled_string ());
  Printf.printf "%d\n" (disabled_int ());
  Printf.printf "%.1f\n" (disabled_float (Sys.opaque_identity 2.));
  Printf.printf "%d\n" (disabled_no_handler (Sys.opaque_identity 9));
  Printf.printf "%d\n" (enabled_int ())
