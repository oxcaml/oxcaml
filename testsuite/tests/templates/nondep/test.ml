(* TEST
 readonly_files = "\
   foo.ml bar.mli bar.reference \
   abs.mli absuser.mli absuser.reference \
   elt.mli lib.mli lib.ml user.mli user.reference \
   user-objinfo.reference \
 ";
 {
   setup-ocamlc.byte-build-env;
   ocamlc_flags = "-w -misplaced-attribute -w -bad-module-name";

   (* A regular module whose concrete type can be erased. *)
   module = "foo.ml";
   ocamlc.byte;
   {
     (* [-nondep Foo] erases [Foo.t] from the saved interface: the printed
        signature shows [int] instead of [Foo.t]. *)
     flags = "-nondep Foo -i";
     module = "bar.mli";
     compiler_output = "bar.output";
     ocamlc.byte;

     compiler_reference = "bar.reference";
     check-ocamlc.byte-output;
   }{
     (* An abstract type cannot be erased, so [-nondep] is a hard error. *)
     flags = "";
     module = "abs.mli";
     ocamlc.byte;
     {
       flags = "-nondep Abs";
       module = "absuser.mli";
       compiler_output = "absuser.output";
       ocamlc_byte_exit_status = "2";
       ocamlc.byte;

       compiler_reference = "absuser.reference";
       check-ocamlc.byte-output;
     }
   }{
     (* A parameterised module: [-nondep Lib] both erases [Lib.u] from the
        signature and drops the incomplete global [Lib{Elt}] from the
        [cmi_globals] ("Globals in scope") of the saved [.cmi]. *)
     flags = "-as-parameter";
     module = "elt.mli";
     ocamlc.byte;

     flags = "-parameter Elt";
     module = "lib.mli lib.ml";
     ocamlc.byte;

     flags = "-parameter Elt -nondep Lib";
     module = "user.mli";
     ocamlc.byte;
     {
       program = "user.cmi";
       output = "user-objinfo.output";
       ocamlobjinfo;

       reference = "user-objinfo.reference";
       check-program-output;
     }
   }
 }
*)
