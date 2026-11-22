(* TEST
 flags = "-I ${ocamlsrcdir}/utils";
 {
   codegen_variant = "fp";
   variant_available;
   expect.opt;
 }
*)

(* A native expect test that selects a codegen variant runs in the build
   of expect.opt for that variant: the phrases are compiled for it, against
   its library tree, and loaded into a program that was itself built for
   it. *)

let variant = Codegen_variant.to_string (Codegen_variant.current ())
[%%expect{|
val variant : string = "fp"
|}]

let sum = List.fold_left ( + ) 0 [1; 2; 3]
[%%expect{|
val sum : int = 6
|}]
