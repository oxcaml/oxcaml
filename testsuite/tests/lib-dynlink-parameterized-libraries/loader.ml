(* TEST
 include dynlink;
 libraries = "";
 readonly_files = "\
   elem.mli \
   int_elem.mli int_elem.ml \
   printer.mli printer.ml \
   runner.mli runner.ml \
   plugin.ml \
   loader.ml \
 ";
 shared-libraries;
 {
   setup-ocamlc.byte-build-env;

   flags = "-as-parameter";
   module = "elem.mli";
   ocamlc.byte;

   flags = "-as-argument-for Elem";
   module = "int_elem.mli int_elem.ml";
   ocamlc.byte;

   flags = "-parameter Elem";
   module = "printer.mli printer.ml";
   ocamlc.byte;

   flags = "-parameter Elem";
   module = "runner.mli runner.ml";
   ocamlc.byte;

   module = "";
   flags = "-instantiate";
   program = "printer-Int_elem.cmo";
   all_modules = "printer.cmo int_elem.cmo";
   ocamlc.byte;

   module = "";
   flags = "-instantiate";
   program = "runner-Int_elem.cmo";
   all_modules = "runner.cmo int_elem.cmo";
   ocamlc.byte;

   flags = "-w -misplaced-attribute";
   module = "plugin.ml";
   ocamlc.byte;

   flags = "";
   program = "plugin.cma";
   module = "";
   all_modules = "\
     runner.cmo \
     runner-Int_elem.cmo \
     plugin.cmo \
   ";
   flags = "-a";
   ocamlc.byte;

   program = "${test_build_directory}/loader.byte";
   flags = "-linkall";
   include ocamlcommon;
   libraries += "dynlink";
   all_modules = "int_elem.cmo printer.cmo printer-Int_elem.cmo loader.ml";
   ocamlc.byte;

   arguments = "plugin.cma";
   exit_status = "0";
   run;

   reference = "${test_source_directory}/byte.reference";
   check-program-output;
 }{
   native-dynlink;
   setup-ocamlopt.byte-build-env;

   flags = "-as-parameter";
   module = "elem.mli";
   ocamlopt.byte;

   flags = "-as-argument-for Elem";
   module = "int_elem.mli int_elem.ml";
   ocamlopt.byte;

   flags = "-parameter Elem";
   module = "printer.mli printer.ml";
   ocamlopt.byte;

   flags = "-parameter Elem";
   module = "runner.mli runner.ml";
   ocamlopt.byte;

   module = "";
   flags = "-instantiate";
   program = "printer-Int_elem.cmx";
   all_modules = "printer.cmx int_elem.cmx";
   ocamlopt.byte;

   module = "";
   flags = "-instantiate";
   program = "runner-Int_elem.cmx";
   all_modules = "runner.cmx int_elem.cmx";
   ocamlopt.byte;

   flags = "-w -misplaced-attribute";
   module = "plugin.ml";
   ocamlopt.byte;

   program = "plugin.cmxs";
   flags = "-shared";
   module = "";
   all_modules = "\
     runner.cmx \
     runner-Int_elem.cmx \
     plugin.cmx \
   ";
   ocamlopt.byte;

   program = "${test_build_directory}/loader.exe";
   flags = "-linkall";
   include ocamlcommon;
   libraries += "dynlink";
   all_modules = "int_elem.cmx printer.cmx printer-Int_elem.cmx loader.ml";
   ocamlopt.byte;

   arguments = "plugin.cmxs";
   exit_status = "0";
   run;

   reference = "${test_source_directory}/native.reference";
   check-program-output;
 }
*)
let () =
  try
    Dynlink.loadfile Sys.argv.(1)
  with
  | Dynlink.Error error ->
    prerr_endline (Dynlink.error_message error)
