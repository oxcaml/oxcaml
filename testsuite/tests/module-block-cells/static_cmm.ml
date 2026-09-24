(* TEST
 readonly_files = "static_lib.ml";
 flambda2;
 {
   setup-ocamlopt.byte-build-env;
   module = "static_lib.ml";
   ocamlopt.byte;
   ocamlopt_flags = "-dcmm -dcanonical-ids";
   module = "static_cmm.ml";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }{
   ocamlopt_flags = "-Oclassic";
   compiler_directory_suffix = ".Oclassic";
   compiler_reference =
     "${test_source_directory}/static_cmm.Oclassic.compilers.reference";
   setup-ocamlopt.byte-build-env;
   module = "static_lib.ml";
   ocamlopt.byte;
   ocamlopt_flags = "-Oclassic -dcmm -dcanonical-ids";
   module = "static_cmm.ml";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }
*)

(* [Static_lib] has only static fields, so with its .cmx present, in both
   Normal and Classic mode, the Cmm dump has no allocation and no load from a
   cell:
   - [Static_lib.f] and [Static_lib.c] fold to the closure symbol and the
     constant, and [apply_f] reduces to the body of [f];
   - [(module Static_lib : S)], at toplevel and inside a function, is a
     static data block holding the closure symbols and constants. *)

module type S = sig
  val f : int -> int
  val g : int -> int
  val c : int
  val s : string
end

let apply_f x = Static_lib.f x
let f = Static_lib.f
let c = Static_lib.c
let whole = (module Static_lib : S)
let make_whole () = (module Static_lib : S)
