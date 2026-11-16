(* TEST
 codegen_variant = "fp";
 variant_available;
 flags = "-no-frame-pointers";
 set OCAMLPARAM = "_,frame-pointers=1";
 native;
*)

(* frame-pointers=1 in the post-underscore part of OCAMLPARAM overrides the
   command line (here an explicit -no-frame-pointers, which itself overrides
   the -frame-pointers that codegen_variant = "fp" injects), so the fp mode
   of this binary comes from OCAMLPARAM alone.  codegen_variant = "fp" is
   still needed so that ocamltest compiles and links against the fp library
   root. *)

external frame_pointers : bool = "%frame_pointers"

let () = Printf.printf "frame_pointers: %b\n" frame_pointers
