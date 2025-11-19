(* TEST
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 run;
 check-program-output;
*)

(* Test classic OCaml mangling scheme:
   - caml prefix
   - __ -> . (module separator)
   - $xx -> hex-encoded characters
*)
