(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Wrong result: The default inlining parameters make [g]
   ineligible for inlining (size 12 vs. large function threshold of 10.
   This makes [h] depend on [x] through [g], so it cannot be lifted. *)

let f x l =
  let g b = if b then x else 42 in
  let h acc x = acc + x + g false in
  List.fold_left h 0 l
