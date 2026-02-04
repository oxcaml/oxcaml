(* TEST
  include camlinternaleval;
  flags = "-extension runtime_metaprogramming";
  ocamlc_byte_exit_status = "2";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
  check-ocamlc.byte-output;
*)

#syntax quotations on
open Camlinternaleval

let test_non_value_eval =
  Printf.printf "\nTest simple eval\n";
  let eval : <[int64#]> expr -> int64# = eval in
  let output = eval <[ #42L ]> in
  Printf.printf "Output: %d\n" (Int64_u.to_int_exn output);
;;
