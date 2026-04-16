(* TEST
 readonly_files = "uniform_product.ml";
 setup-ocamlopt.opt-build-env;
 module = "uniform_product.ml";
 ocamlopt.opt;
 module = "uniform_product_test.ml";
 ocamlopt.opt;
 module = "";
 all_modules = "uniform_product.cmx uniform_product_test.cmx";
 ocamlopt.opt;
 run;
*)

let foo = let #(foo, _) = Uniform_product.foobar in foo
