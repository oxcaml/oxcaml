(* TEST
 readonly_files = "sc_unit.ml";
 codegen_variant = "+sc";
 variant_available;
 setup-ocamlopt.byte-build-env;
 modules = "sc_unit.ml";
 program = "sc_library";
 ocamlmklib;
 program = "${test_build_directory}/ocamlmklib_variant.exe";
 libraries = "sc_library";
 flags = "-noautolink";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* The native compiler that ocamlmklib starts compiles for the variant of
   the block.  The link that follows is the assertion: it is for the
   variant of the block and would refuse a library compiled for another
   one.  (ocamlmklib records a library of C stubs in the archive even when
   it is given no C object, hence -noautolink.) *)

let () = assert (Sc_unit.f () = 42)
