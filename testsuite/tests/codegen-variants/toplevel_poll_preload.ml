(* TEST
 shared-libraries;
 native-dynlink;
 script = "sh ${test_source_directory}/poll_pair.sh available";
 config_script;
 setup-ocamlopt.byte-build-env;
 script = "sh ${test_source_directory}/toplevel_poll_preload.sh";
 script;
 check-program-output;
*)

(* The native toplevel accepts the poll-insertion setting that its own code
   does not have, and the libraries that it then loads are those of the
   tree for that setting, as for a link. That holds for an object given on
   the command line by name, which is looked for in the load path: the
   entry of the load path for the standard library, which is computed
   before the command line is read, follows the flags. That includes the
   flags that take a directory out of the load path: -nostdlib for the
   standard library, and -nocwd for the current directory. An object that
   a directive could not load afterwards is not loaded from the command
   line either.

   The script runs the toplevel of the default variant in a library root
   where each of the two trees has a plugin that says which tree it is. It
   needs the tree of the variant that differs from the default one in poll
   insertion only (poll_pair.sh). *)
