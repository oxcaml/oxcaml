(* TEST
 setup-ocamlc.byte-build-env;
 flags += "-extension mode_polymorphism_beta -dsolver-graph-trace";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

let foo () =
  let foo x = x in
  let x = ref 42 in
  let x = foo x in
  let (y @ contended) = ref !x in
  foo y
