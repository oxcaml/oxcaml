(* TEST
 readonly_files = "gen_types.sh types.ml";
 setup-ocamlc.byte-build-env;

 (* Generate a file that defines some large polymorphic variants. *)
 script = "/bin/bash gen_types.sh types.ml";
 script;
 module = "types.ml";
 ocamlc.byte;

 expect;
*)

#directory "ocamlc.byte";;
#load "types.cmo";;

type foo : immutable_data = Types.poly_variant_with_100
[%%expect {|
type foo = Types.poly_variant_with_100
|}]

(* CR layouts v2.8: This isn't currently accepted because we have a restriction
   that we don't do inference when the polymorphic variant has more than 100
   rows. In the future, we should remove this restriction. Internal ticket
   5435. *)
type foo : immutable_data = Types.poly_variant_with_101
[%%expect {|
type foo = Types.poly_variant_with_101
|}]
