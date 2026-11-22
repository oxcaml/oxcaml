(* TEST
 codegen_variant = "+sc";
 variant_available;
 flags = "-no-stack-checks";
 set OCAMLPARAM = "_,stack-checks=1";
 native;
*)

(* stack-checks=1 in the post-underscore part of OCAMLPARAM overrides the
   command line (here an explicit -no-stack-checks, which itself overrides
   the -stack-checks that the selected variant injects), so the sc mode
   of this binary comes from OCAMLPARAM alone.  Success is the assertion:
   ocamltest compiles and links against the library root of a variant with
   stack checks, and a unit compiled without them would be rejected there
   by the codegen-fingerprint check. *)

let () = print_endline "stack-checks ok"
