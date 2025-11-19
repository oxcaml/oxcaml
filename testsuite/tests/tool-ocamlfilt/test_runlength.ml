(* TEST
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 run;
 check-program-output;
*)

(* Test runlength mangling scheme with various path items:
   - M: Module
   - F: NamedFunction
   - L: AnonymousFunction (formatted as fn(file:line:col))
   - S: AnonymousModule (formatted as mod(file:line:col))
   - P: PartialFunction
   - u prefix: Unicode encoding
*)

(* TODO: Consider using ocamlfilt.byte instead of the native version for
   consistency with other tool tests (dumpobj, ocamlmklib, ocamlobjinfo all
   use .byte versions). This would require updating the Makefile.common-ox
   to symlink to ocamlfilt.byte instead of ocamlfilt, and updating all .run
   files to use ${ocamlrun} prefix. *)
