(* TEST
 flambda2;
 readonly_files = "env_types_lib.mli env_types_lib.ml env_types_use.ml";
 setup-ocamlopt.byte-build-env;
 (* [-nocwd] is needed because [-Ix .] doesn't override the implicit [-I .]. *)
 flags = "-extension layout_poly_alpha -nocwd -Ix .";
 module = "env_types_lib.mli";
 ocamlopt.byte;
 {
   ocamlopt_flags = "-O3 -flambda2-reaper";
   module = "env_types_lib.ml";
   ocamlopt.byte;
   ocamlopt_flags = "-O3";
   module = "env_types_use.ml";
   ocamlopt.byte;
   unset module;
   program = "${test_build_directory}/env_types_modes.exe";
   all_modules = "env_types_lib.cmx env_types_use.cmx";
   ocamlopt.byte;
   run;
   check-program-output;
 }{
   ocamlopt_flags = "-O3";
   module = "env_types_lib.ml";
   ocamlopt.byte;
   ocamlopt_flags = "-O3 -flambda2-reaper";
   module = "env_types_use.ml";
   ocamlopt.byte;
   unset module;
   program = "${test_build_directory}/env_types_modes.exe";
   all_modules = "env_types_lib.cmx env_types_use.cmx";
   ocamlopt.byte;
   run;
   check-program-output;
 }{
   ocamlopt_flags = "-Oclassic";
   module = "env_types_lib.ml";
   ocamlopt.byte;
   ocamlopt_flags = "-O3";
   module = "env_types_use.ml";
   ocamlopt.byte;
   unset module;
   program = "${test_build_directory}/env_types_modes.exe";
   all_modules = "env_types_lib.cmx env_types_use.cmx";
   ocamlopt.byte;
   run;
   check-program-output;
 }{
   ocamlopt_flags = "-O3";
   module = "env_types_lib.ml";
   ocamlopt.byte;
   ocamlopt_flags = "-Oclassic";
   module = "env_types_use.ml";
   ocamlopt.byte;
   unset module;
   program = "${test_build_directory}/env_types_modes.exe";
   all_modules = "env_types_lib.cmx env_types_use.cmx";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* [env_types_use.ml] uses the types of the environments of the templates of
   [env_types_lib.ml], which are exported by its .cmx file, with the two units
   compiled at different optimisation levels, with and without the reaper. *)
