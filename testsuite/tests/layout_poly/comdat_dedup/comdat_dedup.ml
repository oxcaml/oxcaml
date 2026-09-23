(* TEST
 arch_amd64;
 not-macos;
 readonly_files = "lpoly_lib.mli lpoly_lib.ml user1.ml user2.ml";
 setup-ocamlopt.byte-build-env;
 {
  flags = "-extension layout_poly_alpha -nocwd -Ix .";
  module = "lpoly_lib.mli";
  ocamlopt.byte;
  module = "lpoly_lib.ml";
  ocamlopt.byte;
  module = "user1.ml";
  ocamlopt.byte;
  module = "user2.ml";
  ocamlopt.byte;
  module = "comdat_dedup.ml";
  ocamlopt.byte;
  unset module;
  program = "${test_build_directory}/comdat_dedup.exe";
  all_modules = "lpoly_lib.cmx user1.cmx user2.cmx comdat_dedup.cmx";
  ocamlopt.byte;
  run;
 }{
  flags = "-extension layout_poly_alpha -nocwd -Ix . -Oclassic";
  module = "lpoly_lib.mli";
  ocamlopt.byte;
  module = "lpoly_lib.ml";
  ocamlopt.byte;
  module = "user1.ml";
  ocamlopt.byte;
  module = "user2.ml";
  ocamlopt.byte;
  module = "comdat_dedup.ml";
  ocamlopt.byte;
  unset module;
  program = "${test_build_directory}/comdat_dedup.exe";
  all_modules = "lpoly_lib.cmx user1.cmx user2.cmx comdat_dedup.cmx";
  ocamlopt.byte;
  run;
 }
*)

(* [user1.ml] and [user2.ml] both instantiate [Lpoly_lib.lpoly_pair] at
   (float64, float64), so each object file carries a weak copy of the
   instance's code and of its closure block, both named after the cohort.
   After linking, COMDAT deduplication must leave exactly one of each.

   The test runs the linked program and then shells out to [nm] to inspect
   the symbol table. *)

let () =
  let _ = Sys.opaque_identity User1.pair () in
  let _ = Sys.opaque_identity User2.pair () in
  ()

let expected_symbol_res =
  [ "camlLpoly_lib__cohort__Lpoly_lib_lpoly_pair_.*float64_float64_code$";
    "camlLpoly_lib__cohort__Lpoly_lib_lpoly_pair_.*float64_float64$" ]

let () =
  let exe = Filename.quote Sys.executable_name in
  let ok = ref true in
  List.iter
    (fun re ->
      let check =
        Printf.sprintf "test $(nm %s | grep -c '%s') -eq 1" exe re
      in
      if Sys.command check <> 0
      then begin
        ok := false;
        Printf.eprintf
          "expected exactly one copy of weak symbol matching /%s/; \
           relevant nm output:\n%!"
          re;
        let dump = Printf.sprintf "nm %s | grep Lpoly_lib >&2 || true" exe in
        ignore (Sys.command dump)
      end)
    expected_symbol_res;
  if not !ok then exit 1
