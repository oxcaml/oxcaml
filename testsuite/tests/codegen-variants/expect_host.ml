(* TEST
 flags = "-I ${ocamlsrcdir}/utils";
 {
   codegen_variant = "+fp";
   variant_available;
   expect.opt;
 }
*)

(* A native expect test that selects a codegen variant runs in the build
   of expect.opt for that variant: the phrases are compiled for it, against
   its library tree, and loaded into a program that was itself built for
   it.  Here the test asks for a variant with frame pointers. *)

let frame_pointers = (Codegen_variant.current ()).frame_pointers
[%%expect{|
val frame_pointers : bool = true
|}]

let sum = List.fold_left ( + ) 0 [1; 2; 3]
[%%expect{|
val sum : int = 6
|}]
