(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-flambda2-reaper -reaper-debug-flags=nostamps";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-reaper;
 check-fexpr-dump;
*)

let tupled (_a, _b) = true
let () =
  let[@inline never][@local never] id f = f in
  let pair = (0, 0) in
  assert (id tupled pair)