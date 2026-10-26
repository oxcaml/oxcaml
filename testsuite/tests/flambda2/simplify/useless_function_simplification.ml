(* TEST
   compile_only = "true";
   flambda2;
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-raw, dump-simplify;
   check-fexpr-dump;
 *)

let[@inline] make x y =
  let rec g () = x + y in
  g
;;

let h a b =
  (* There is no benefit to making a copy of g specialised for a b
     instead of x y, so we should keep the original instead *)
  make a b
