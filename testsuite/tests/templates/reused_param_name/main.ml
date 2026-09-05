(* TEST
 readonly_files = "\
   p_param.mli use_p.ml p_string.mli p_string.ml use_inst.ml p.ml \
 ";
 setup-ocamlopt.byte-build-env;
 script = "mkdir param/";
 script;
 script = "cp use_p.ml p_string.mli p_string.ml use_inst.ml param/";
 script;
 script = "cp p_param.mli param/p.mli";
 script;
 cwd = "param";
 cd;
 flags = "-as-parameter";
 module = "p.mli";
 ocamlopt.byte;
 flags = "-parameter P";
 module = "use_p.ml";
 ocamlopt.byte;
 flags = "-as-argument-for P";
 module = "p_string.mli p_string.ml";
 ocamlopt.byte;
 module = "";
 flags = "-instantiate";
 program = "use_p-P_string.cmx";
 all_modules = "use_p.cmx p_string.cmx";
 ocamlopt.byte;
 flags = "-w -53";
 module = "use_inst.ml";
 ocamlopt.byte;
 cwd = "..";
 cd;
 flags = "";
 module = "p.ml";
 ocamlopt.byte;
 module = "main.ml";
 ocamlopt.byte;
 module = "";
 program = "${test_build_directory}/main.exe";
 all_modules = "\
   param/use_p.cmx param/p_string.cmx param/use_p-P_string.cmx \
   param/use_inst.cmx p.cmx main.cmx \
 ";
 ocamlopt.byte;
 output = "main.output";
 run;
 reference = "main.reference";
 check-program-output;
*)

(* The parameter [P] (which [Use_p] is parameterised by, and which
   [Use_inst]'s instance fills with [P_string]) and the regular module [P]
   defined at the root are completely different: the former names an
   interface, the latter an implementation. Referencing one and linking
   both into the same program must not be reported as a collision. *)

let () = print_endline (P.regular ())
