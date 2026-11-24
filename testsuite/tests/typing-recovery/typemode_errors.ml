(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

(* Unrecognized mode *)
let f (x @ bogus) = x

(* Duplicated mode axis *)
let g (x @ local local) = x

(* Unrecognized modality *)
type r = { a : int @@ bogus }

(* Forbidden modality *)
type s = { b : int ref @@ global unique }

(* Unrecognized modifier *)
type t : value mod bogus

(* Duplicated modifier axis *)
type u : value mod portable portable

(* All of the above are still bound. *)
let _ : int -> int = f
let _ : int @ local -> int @ local = g
let _ : r = { a = 1 }
let _ : s = { b = ref 1 }
type t' = t
type u' = u

(* This error should still be raised, despite the prior errors. *)
let t : string = 10
