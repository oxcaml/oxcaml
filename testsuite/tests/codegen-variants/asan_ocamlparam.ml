(* TEST
 codegen_variant = "asan-fp";
 variant_available;
 flags = "-no-asan";
 set OCAMLPARAM = "_,asan=1";
 native;
*)

(* asan=1 in the post-underscore part of OCAMLPARAM overrides the command
   line (here an explicit -no-asan, which itself overrides the -asan that
   codegen_variant = "asan-fp" injects), so the asan mode of this binary
   comes from OCAMLPARAM alone.  Success is the assertion: ocamltest
   compiles and links against the asan-fp library root, and a unit
   compiled without asan would be rejected there by the codegen-fingerprint
   check. *)

let () = print_endline "asan ok"
