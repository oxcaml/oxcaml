(* TEST
 ocamlopt_flags = "-asan -no-frame-pointers -no-stack-checks";
 jsir;
*)

(* A program compiles and links for js_of_ocaml and for wasm_of_ocaml
   whatever the codegen-variant flags say, since JSIR has no variants: the
   flags here select AddressSanitizer without frame pointers, which is
   refused for native code on every platform. *)

let () = print_endline "The flags of the axes are ignored"
