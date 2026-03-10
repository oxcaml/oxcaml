(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-flambda2-reaper -reaper-max-unbox-size 3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-reaper;
 check-fexpr-dump;
*)

let f x =
  (* [small] is only size 3 for unboxing, as [d] is unused. *)
  let[@inline never][@local never] small (a, b, c, d) () = a + b + c in
  (* [large] is size 4, putting it over the max-unbox-size limit. *)
  let[@inline never][@local never] large (a, b, c, d) () = a + b + c + d in
  small (x + 1, x + 2, x + 3, x + 4) () + large (x + 5, x + 6, x + 7, x + 8) ()