(* TEST
 native-compiler;
 setup-ocamlopt.byte-build-env;
 script = "sh ${test_source_directory}/poll_tree_pairs.sh";
 script;
 check-program-output;
*)

(* Poll insertion is a property of each unit, and the compiler uses the
   library trees of two codegen variants that differ only in it for one
   another: a unit compiled against one of them is linked against the
   other when the link has the other setting, or when the installation
   lacks one of the two (see Codegen_variant.selected_tree in
   utils/codegen_variant.mli). This is only sound while the two trees are
   the same libraries but for the polls in their code. So, for every two
   such trees of the installation under test, the two must hold the same
   units, archives and object files, every unit must have the same
   implementation CRC in both, on its own and in an archive, and the
   archives and objects must define the same symbols.

   If this test fails, a unit is missing from one of the trees, or
   something that a .cmx file records, or the name of a symbol, has come
   to depend on poll insertion, or the build of the two trees differs in
   something else than poll insertion (the script prints which files
   differ). The library root also holds the compiler's own libraries,
   which the trees of the other variants do not have: those are not
   compared. This file is not compiled. *)
