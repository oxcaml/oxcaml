(* TEST
 codegen_variant = "sc";
 variant_available;
 toplevel.opt;
*)

(* A native toplevel test that selects a codegen variant runs in the build
   of ocamlnat for that variant. *)
#directory "+compiler-libs";;
Codegen_variant.to_string (Codegen_variant.current ());;
List.fold_left ( + ) 0 [1; 2; 3];;
