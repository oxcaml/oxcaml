(* TEST
 codegen_variant = "sc";
 variant_available;
 flags = "-no-stack-checks";
 set OCAMLPARAM = "_,stack-checks=1";
 native;
*)

(* stack-checks=1 in the post-underscore part of OCAMLPARAM overrides the
   command line (here an explicit -no-stack-checks, which itself overrides
   the -stack-checks that codegen_variant = "sc" injects), so the sc mode
   of this binary comes from OCAMLPARAM alone.  Success is the assertion:
   ocamltest compiles and links against the sc library root, and a unit
   compiled without stack checks would be rejected there by the
   codegen-fingerprint check. *)

let () = print_endline "stack-checks ok"
