(* TEST
 native-compiler;
 set OCAMLLIB = "${ocamlsrcdir}/stdlib";
 codegen_variant = "+poll";
 variant_available;
 readonly_files = "poll_object_unit.ml poll_object_packed.ml";
 readonly_files += " poll_object_loop.ml";
 setup-ocamlopt.byte-build-env;
 script = "sh ${test_source_directory}/poll_object.sh";
 script;
 check-program-output;
*)

(* Whether a unit has polls is recorded in its .cmx file, and its code is
   in another file. A program is taken to be fully polled on the word of
   the .cmx files, so the linker must refuse an object file compiled
   without polls beside a .cmx file that says that the unit has them: an
   object file left behind by an earlier compilation, for one. Every object
   file of OCaml code without polls refers to a symbol that the startup
   code of a program defines only when the program is not fully polled, so
   such an object file is an undefined symbol in a fully polled program,
   as a member of a pack too. The other mismatch is harmless and accepted:
   the program is then taken not to be fully polled.

   This needs libraries with polls: with the others no program is fully
   polled, and an object file without polls takes nothing away.

   This file is the main unit of the programs that the script links. *)

external fully_polled : unit -> bool
  = "caml_sys_exit" "caml_code_is_fully_polled"
[@@noalloc]

let () =
  Printf.printf "sum %d, %s\n"
    (Poll_object_unit.sum 0 4)
    (if fully_polled () then "fully polled" else "not fully polled")
