(* TEST
 shared-libraries;
 native-dynlink;
 setup-ocamlopt.byte-build-env;
 script = "sh ${test_source_directory}/toplevel_load_cmx.sh";
 script;
 check-program-output;
*)

(* #load of a .cmx file in the native toplevel links the file into a
   plugin and loads that.  The file that #load found is the one to link,
   as it is: when it was found in a directory of the load path that has a
   relative name, its path is no name for the linker to search for again.
   The script loads such a file with the current directory left out of the
   load path, by its name and by its path. *)
