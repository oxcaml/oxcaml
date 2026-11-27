(* TEST
 readonly_files = "stubs.c option.c with_stubs.ml later.ml later_again.ml";
 native-dynlink;
 setup-ocamlnat-build-env;
 module = "stubs.c";
 ocamlopt.byte;
 module = "option.c";
 ocamlopt.byte;
 module = "with_stubs.ml";
 ocamlopt.byte;
 module = "later.ml";
 ocamlopt.byte;
 module = "later_again.ml";
 ocamlopt.byte;
 module = "";
 flags = "-a -linkall -ccopt option.${objext}";
 program = "with_stubs.cmxa";
 all_modules = "with_stubs.cmx stubs.${objext}";
 ocamlopt.byte;
 flags = "";
 ocamlnat;
 check-ocamlnat-output;
*)

(* The archive has a C object, and an option that cannot be repeated either,
   being another object. No later link must be given either of them twice,
   not even that of a phrase, which is given the options: an object given
   twice is defined twice. *)
#load "with_stubs.cmxa";;
#load "later.cmx";;
#load "later_again.cmx";;
print_endline "phrase evaluated";;
