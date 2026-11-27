(* TEST
 native-dynlink;
 setup-ocamlopt.byte-build-env;
 module = "startup_object.ml";
 ocamlopt.byte;
 module = "";
 flags = "-shared -cc false";
 program = "startup_object.cmxs";
 all_modules = "startup_object.cmx";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 script = "sh ${test_source_directory}/check-startup-object.sh";
 script;
*)

(* The linker, here a command that fails, is run on the object file of the
   startup code: that file must not be left behind. *)
let () = ()
