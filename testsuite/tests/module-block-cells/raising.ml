(* TEST
 readonly_files = "raising_lib.ml";
 flambda2;
 {
   ocamlopt_flags = "-O3";
   setup-ocamlopt.byte-build-env;
   module = "raising_lib.ml";
   ocamlopt.byte;
   module = "raising.ml";
   ocamlopt.byte;
   module = "";
   all_modules = "raising_lib.cmx raising.cmx";
   program = "${test_build_directory}/raising.exe";
   ocamlopt.byte;
   exit_status = "2";
   run;
   check-program-output;
 }{
   ocamlopt_flags = "-Oclassic";
   compiler_directory_suffix = ".Oclassic";
   setup-ocamlopt.byte-build-env;
   module = "raising_lib.ml";
   ocamlopt.byte;
   module = "raising.ml";
   ocamlopt.byte;
   module = "";
   all_modules = "raising_lib.cmx raising.cmx";
   program = "${test_build_directory}/raising.exe";
   ocamlopt.byte;
   exit_status = "2";
   run;
   check-program-output;
 }
*)

(* [Raising_lib]'s initialiser raises, so its cells are never defined by the
   initialiser; the references below (a field and the whole block) still link,
   against placeholder definitions, and the program dies with the exception
   before reaching them. *)

module type S = sig
  val f : int -> int
  val n : int
  val r : int ref
end

let packed = (module Raising_lib : S)

let () =
  let module M = (val packed) in
  Printf.printf "unreachable: %d %d %d\n" (Raising_lib.f Raising_lib.n) M.n
    !M.r
