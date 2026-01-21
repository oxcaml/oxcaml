(* TEST
  flags = "-extension runtime_metaprogramming";
  ocamlc_byte_exit_status = "2";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
  check-ocamlc.byte-output;
*)

#syntax quotations on

let eval : <[int64#]> expr -> int64# = [%eval: int64#]
