(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let f x =
  match x with
  | 1.0s -> 1
  | 2.0s -> 2
  | _ -> 3

(* Float32 match errors only have a file as their location, so this error is
   deduplicated with the one above. *)
let g x =
  match x with
  | #1.0s -> 1
  | #2.0s -> 2
  | _ -> 3

(* [f] and [g] are still bound. *)
let _ : float32 -> int = f
let _ : float32_u -> int = g

(* This error should still be raised, despite the prior errors. *)
let t : string = 10
