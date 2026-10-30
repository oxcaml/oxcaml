(* TEST
 readonly_files = "mixed_lib.ml";
 native-compiler;
 frametable_sections;
 unset OCAMLPARAM;
 reference = "${test_source_directory}/mixed.reference";
 {
   (* Only the library uses frametable sections. *)
   setup-ocamlopt.byte-build-env;
   flags = "-g -function-sections -frametable-sections";
   module = "mixed_lib.ml";
   ocamlopt.byte;
   flags = "-g";
   module = "mixed.ml";
   ocamlopt.byte;
   module = "";
   all_modules = "mixed_lib.cmx mixed.cmx";
   program = "${test_build_directory}/lib_sections.exe";
   flags = "-g -ccopt -Wl,--gc-sections";
   ocamlopt.byte;
   run;
   check-program-output;
 }{
   (* Only the main unit uses frametable sections. *)
   setup-ocamlopt.byte-build-env;
   flags = "-g";
   module = "mixed_lib.ml";
   ocamlopt.byte;
   flags = "-g -function-sections -frametable-sections";
   module = "mixed.ml";
   ocamlopt.byte;
   module = "";
   all_modules = "mixed_lib.cmx mixed.cmx";
   program = "${test_build_directory}/main_sections.exe";
   flags = "-g -ccopt -Wl,--gc-sections";
   ocamlopt.byte;
   run;
   check-program-output;
 }{
   (* The main unit's entry function is in .text.startup.caml, which linkers
      place before the rest of the code, so its descriptors must still land
      inside the unit's range. *)
   arch_amd64;
   setup-ocamlopt.byte-build-env;
   flags = "-g";
   module = "mixed_lib.ml";
   ocamlopt.byte;
   flags = "-g -function-sections -frametable-sections -module-entry-functions-section";
   module = "mixed.ml";
   ocamlopt.byte;
   module = "";
   all_modules = "mixed_lib.cmx mixed.cmx";
   program = "${test_build_directory}/entry_section.exe";
   flags = "-g -ccopt -Wl,--gc-sections";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* Units with and without -frametable-sections are linked together (with
   --gc-sections, so the pieces must survive the linker's garbage collection).
   Backtraces cross both units, reaching each unit's debuginfo, and a major GC
   scans a deep stack of frames from both. *)

let[@inline never] via_main n =
  Mixed_lib.raise_deep (n + 1);
  0

let[@inline never] rec main_rec n =
  if n = 0
  then Mixed_lib.deep_alloc 500 []
  else Mixed_lib.call_back (fun () -> main_rec (n - 1)) + 1

let () =
  Printexc.record_backtrace true;
  (try ignore (via_main 3) with
  | Mixed_lib.Deep n ->
    Printf.printf "caught Deep %d\n" n;
    print_string (Printexc.get_backtrace ()));
  Printf.printf "deep %d\n" (main_rec 200)
