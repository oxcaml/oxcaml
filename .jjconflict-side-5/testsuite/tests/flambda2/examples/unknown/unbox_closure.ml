(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

let foobar b x =
  let bar y =
    Sys.opaque_identity ();
    let aux z = x + y + z in
    aux
  in
  let f, _ = if b then bar 3, 1 else bar 42, 2 in
  f x
