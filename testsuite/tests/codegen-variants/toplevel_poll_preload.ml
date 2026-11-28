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
   the command line by name, which is looked for in the load path, as it
   does for one given by path inside the library root: the entry of the
   load path for the standard library, which is computed before the
   command line is read, follows the flags. That includes the flags that
   take a directory out of the load path: -nostdlib takes out the standard
   library and its directory for camlp4, and -nocwd the current directory.
   Without -nostdlib the command line also looks in that directory for
   camlp4, which the directives do not, as before: with or without -nocwd
   it then loads an object from there that a directive afterwards would
   not find. For both, the current directory is the counterpart
   in the selected tree of a directory of the library root, by what it was
   when the toplevel started: #cd does not change what the entry stands
   for. Outside the library root that is the current directory itself;
   inside, #directory names the directory changed to.

   The script runs the toplevel of the base variant in a library root
   where each of the two trees has a plugin that says which tree it is. It
   needs the tree of the variant that differs from base in poll
   insertion only (poll_pair.sh). *)
