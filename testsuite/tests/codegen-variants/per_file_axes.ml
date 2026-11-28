(* TEST
 native-compiler;
 setup-ocamlopt.byte-build-env;
 script = "sh ${test_source_directory}/per_file_axes.sh";
 script;
 check-program-output;
*)

(* A line of ocaml_compiler_internal_params that names a source file can
   set an axis of the codegen variant for that file.  The setting holds
   for that file only: the sources that the invocation compiles after it,
   and the link, have the axes of the invocation again.  The script checks
   this with the settings of every codegen variant that the installation
   under test has a library tree for, which exercise all four axes when
   the trees do, and with the other poll-insertion setting, which needs no
   tree. *)
