(* TEST
   compile_only = "true";
   flambda2;
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-raw, dump-simplify;
   check-fexpr-dump;
 *)

(* h ang g are simplified once when simplifying the code of make, and
   once again when inlining make.
   There is no benefit to simplifying g, so the old version should
   be kept. But there is a reference to the new code_id of g in the
   simplified code of h, so g must be kept.

   This limitation is expected to be lifted at some point.
*)
let[@inline] make x =
  let rec g () = ()
  and h () =
    g ();
    x
  in
  h
;;

let h = make 0
