(* TEST
 script = "sh ${test_source_directory}/root_lookups.sh \
   ${ocamlsrcdir}/stdlib ${codegen_variant}";
 {
   codegen_variant = "base";
   variant_available;
   compiler_directory_suffix = ".base";
   setup-ocamlopt.opt-build-env;
   script;
   check-program-output;
 }{
   codegen_variant = "fp";
   variant_available;
   compiler_directory_suffix = ".fp";
   setup-ocamlopt.opt-build-env;
   script;
   check-program-output;
 }
*)

(* A file inside the default library root is looked up in the tree of the
   selected variant whichever way the compiler comes to it, and the outcome
   does not depend on how the root or the file is named.  root_lookups.sh
   makes a root whose two trees hold different files under the same names,
   and links from inside it, where the root is the current directory: with
   OCAMLLIB as "." and as an absolute path, with the current directory on
   the load path and without it.  A name is looked for as it is written, as
   the compiler has always done, and the file that is found is then taken
   from the variant's tree: the libraries that the compiler adds to a link
   and a file named with or without a directory are the variant's, a C
   object is a path from the current directory, and what the invocation
   compiles itself is linked as it was written, also when the variant's
   tree has a file of that name.

   The rest is done from another directory: an interface given to
   -open-cmi-x, a manifest that is itself inside the root, and an
   interface that the invocation compiles before a unit that reads it
   through a manifest.  What the invocation wrote is recognized file by
   file: the interface of a unit is its own only when compiling the unit
   wrote it, which it does not for a unit with a .mli file or with
   -cmi-file, and an archive that is merely named like one of its sources
   is searched for like any other.

   Some cases are refused, on purpose, where the variant has a tree of its
   own.  A directory of the root stands for the one of the variant's tree
   as a whole, whether -I names it or it is the current directory.  So
   two sources that depend on each other are not compiled there: the
   second sees the installed unit of that name, and the link reports the
   two as inconsistent, or it sees none.  And a file that an earlier
   command left there is not found by its bare name; it is by its path.

   One block compiles for the default variant, whatever the configure
   default: its tree is the root itself, and nothing changes there. *)
