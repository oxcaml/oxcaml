(* TEST
 codegen_variant = "+fp";
 variant_available;
 flags = "-no-frame-pointers";
 set OCAMLPARAM = "_,frame-pointers=1";
 native;
*)

(* frame-pointers=1 in the post-underscore part of OCAMLPARAM overrides the
   command line (here an explicit -no-frame-pointers, which itself overrides
   the -frame-pointers that the selected variant injects), so the fp mode
   of this binary comes from OCAMLPARAM alone.  A variant with frame
   pointers is still selected so that ocamltest compiles and links against
   a library root built with them. *)

external frame_pointers : bool = "%frame_pointers"

let () = Printf.printf "frame_pointers: %b\n" frame_pointers
