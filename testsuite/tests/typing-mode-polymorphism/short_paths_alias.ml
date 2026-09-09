(* TEST
 readonly_files =
   "short_paths_lib__Inner.ml short_paths_lib.ml short_paths_provider.ml";
 setup-ocamlc.byte-build-env;
 module = "short_paths_lib__Inner.ml";
 ocamlc.byte;
 module = "short_paths_lib.ml";
 ocamlc.byte;
 module = "short_paths_provider.ml";
 ocamlc.byte;
 flags += "-short-paths -I ocamlc.byte ocamlc.byte/short_paths_lib__Inner.cmo \
   ocamlc.byte/short_paths_lib.cmo ocamlc.byte/short_paths_provider.cmo";
 expect;
*)

module My_inner = Short_paths_lib.Inner
[%%expect{|
module My_inner = Short_paths_lib.Inner
|}]

let f = Short_paths_provider.f
[%%expect{|
val f : My_inner.t option -> unit = <fun>
|}]
