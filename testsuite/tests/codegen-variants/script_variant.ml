(* TEST
 set OCAMLLIB = "${ocamlsrcdir}/stdlib";
 codegen_variant = "sc";
 variant_available;
 setup-ocamlopt.byte-build-env;
 script = "${ocamlrun} ${ocamlopt_byte} -c script_variant.ml";
 script;
 program = "-no-code -no-approx script_variant.cmx";
 ocamlobjinfo;
 check-program-output;
*)

(* A script sees the compilers with the variant that the test runs under
   as their default, set in OCAMLPARAM: the script above
   passes no flag that selects a variant and names no library tree, and
   compiles this file for the variant of the block. *)

let answer () = 42
