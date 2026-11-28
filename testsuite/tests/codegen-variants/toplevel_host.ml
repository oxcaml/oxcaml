(* TEST
 codegen_variant = "+sc";
 variant_available;
 toplevel.opt;
*)

(* A native toplevel test that selects a codegen variant runs in the build
   of ocamlnat for that variant.  Here the test asks for a variant with
   stack checks. *)
#directory "+compiler-libs";;
(Codegen_variant.current ()).stack_checks;;
List.fold_left ( + ) 0 [1; 2; 3];;
