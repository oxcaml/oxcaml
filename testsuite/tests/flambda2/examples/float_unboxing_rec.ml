(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

let f n x =
  let r = ref x in
  for i = 0 to n do
    r := !r +. x
  done;
  !r +. 0.
