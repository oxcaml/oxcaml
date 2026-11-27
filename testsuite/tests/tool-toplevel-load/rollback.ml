(* TEST
 readonly_files = "a.ml b.ml c.ml";
 subdirectories = "other_a";
 native-dynlink;
 setup-ocamlnat-build-env;
 module = "a.ml";
 ocamlopt.byte;
 module = "b.ml";
 ocamlopt.byte;
 module = "c.ml";
 ocamlopt.byte;
 cwd = "other_a";
 cd;
 module = "a.ml";
 ocamlopt.byte;
 module = "uses_a.ml";
 ocamlopt.byte;
 cwd = "..";
 cd;
 module = "";
 flags = "-a -linkall -cclib -lmissing_lib -ccopt -lmissing_opt";
 program = "refused.cmxa";
 all_modules = "a.cmx b.cmx";
 ocamlopt.byte;
 program = "unlinkable.cmxa";
 all_modules = "c.cmx";
 ocamlopt.byte;
 flags = "-w -58";
 directories = "${ocamlsrcdir}/compilerlibs";
 set TMPDIR = "${test_build_directory}";
 ocamlnat;
 check-ocamlnat-output;
*)

(* Both archives ask to be linked with a C library and an option that make the
   linker fail, so a link that is given what a refused archive asked for
   fails. *)

#load "a.cmx";;

(* Refused while it is scanned: [A] is already loaded. The units of an archive
   are scanned last to first, so [B] is recorded before [A] is refused. *)
#load "refused.cmxa";;

(* Nothing of the refused archive is kept: [B] can be loaded. *)
#load "b.cmx";;

(* Refused by the linker, which is here a command that fails. *)
let () =
  let c_compiler = !Clflags.c_compiler in
  Clflags.c_compiler := Some "false";
  (try ignore (Opttoploop.load_file Format.std_formatter "unlinkable.cmxa")
   with exn -> Location.report_exception Format.std_formatter exn);
  Clflags.c_compiler := c_compiler
;;

(* Again, nothing of it is kept: [C] can be loaded. *)
#load "c.cmx";;

(* What was recorded before the refused loads is still known: [A] is defined,
   and [other_a/uses_a.cmx] was compiled against another interface of [A]. *)
#load "a.cmx";;
#load "other_a/uses_a.cmx";;

(* The temporary [.cmxs] files of the refused loads are removed. *)
Sys.readdir (Filename.get_temp_dir_name ())
|> Array.to_list
|> List.filter (fun file -> Filename.check_suffix file ".cmxs")
;;
