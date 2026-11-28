(* TEST
 modules = "nocrypto.mli fortuna.ml rng.ml";
 {
   setup-ocamlc.byte-build-env;
   {
     module = "nocrypto.mli";
     ocamlc.byte;
   }{
     flags = "-for-pack Nocrypto";
     module = "fortuna.ml";
     ocamlc.byte;
   }{
     flags = "-for-pack Nocrypto";
     module = "rng.ml";
     ocamlc.byte;
   }{
     program = "nocrypto.cmo";
     flags = "-pack";
     all_modules = "fortuna.cmo rng.cmo";
     ocamlc.byte;
   }
 }{
   setup-ocamlopt.byte-build-env;
   {
     module = "nocrypto.mli";
     ocamlopt.byte;
   }{
     flags = "-for-pack Nocrypto";
     module = "fortuna.ml";
     ocamlopt.byte;
   }{
     flags = "-for-pack Nocrypto";
     module = "rng.ml";
     ocamlopt.byte;
   }{
     (* CR-someday zqian: packing should succeed. [Rng] requires [Fortuna]
        without reading its .cmx file, but the linker requires the CRC of the
        .cmx file of a packed unit from all its importers. *)
     program = "nocrypto.cmx";
     flags = "-pack";
     all_modules = "fortuna.cmx rng.cmx";
     ocamlopt_byte_exit_status = "2";
     ocamlopt.byte;
     compiler_reference =
       "${test_source_directory}/pr8769.ocamlopt.reference";
     check-ocamlopt.byte-output;
   }
 }
*)
