(* TEST
 compile_only = "true";
 flags = "-extension layouts_alpha -w +a-70";
 flambda2;
 {
   ocamlopt_flags = "-Oclassic";
   compiler_directory_suffix = ".Oclassic";
   setup-ocamlopt.byte-build-env;
   ocamlopt.opt;
 }{
   ocamlopt_flags = "-O3";
   compiler_directory_suffix = ".O3";
   setup-ocamlopt.byte-build-env;
   ocamlopt.opt;
 }
*)

type void : void

external unsafe_set_void
  : ('a : value).
  'a or_null @ local -> ('a, void) idx_mut -> void -> unit
  = "%unsafe_set_idx"

let[@inline] unsafe_set_void (type a) (idx : (a, void) idx_mut) v =
  unsafe_set_void Null idx v

(* Emission of warning 55 would be a test failure. *)
let f idx v = (unsafe_set_void [@inlined]) idx v
