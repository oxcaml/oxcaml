(* TEST
 arch_amd64;
 not-macos;
 readonly_files = "lpoly_lib.mli lpoly_lib.ml user1.ml user2.ml user3.ml";
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
  module = "user3.ml";
  ocamlopt.byte;
  module = "comdat_dedup.ml";
  ocamlopt.byte;
  unset module;
  program = "${test_build_directory}/comdat_dedup.exe";
  all_modules = "lpoly_lib.cmx user1.cmx user2.cmx user3.cmx comdat_dedup.cmx";
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
  module = "user3.ml";
  ocamlopt.byte;
  module = "comdat_dedup.ml";
  ocamlopt.byte;
  unset module;
  program = "${test_build_directory}/comdat_dedup.exe";
  all_modules = "lpoly_lib.cmx user1.cmx user2.cmx user3.cmx comdat_dedup.cmx";
  ocamlopt.byte;
  run;
 }{
  flags = "-extension layout_poly_alpha -nocwd -Ix . -g";
  module = "lpoly_lib.mli";
  ocamlopt.byte;
  module = "lpoly_lib.ml";
  ocamlopt.byte;
  flags = "-extension layout_poly_alpha -nocwd -Ix . -g -O3";
  module = "user1.ml";
  ocamlopt.byte;
  flags = "-extension layout_poly_alpha -nocwd -Ix . -g -Oclassic";
  module = "user2.ml";
  ocamlopt.byte;
  flags = "-extension layout_poly_alpha -nocwd -Ix . -g";
  module = "user3.ml";
  ocamlopt.byte;
  module = "comdat_dedup.ml";
  ocamlopt.byte;
  unset module;
  program = "${test_build_directory}/comdat_dedup.exe";
  all_modules = "lpoly_lib.cmx user1.cmx user2.cmx user3.cmx comdat_dedup.cmx";
  ocamlopt.byte;
  run;
 }
*)

(* [user1.ml] and [user2.ml] both instantiate [Lpoly_lib.lpoly_pair] at
   (float64, float64), so each object file carries a weak copy of the
   instance's code under the cohort's shared symbol. After linking, COMDAT
   deduplication must leave exactly one. [user3.ml] calls [User1]'s instance
   from outside [User1], which only links if that call also uses the shared
   symbol.

   [lpoly_call_between] has a frame descriptor, which must be discarded along
   with each losing copy of the code. The last block compiles [user1.ml] and
   [user2.ml] at different optimization levels, so that the copies differ.
   Calling both instances runs the GC through the surviving copy's frame.

   The test runs the linked program and then shells out to [nm] to inspect
   the symbol table. *)

external box : float# -> float = "%box_float"

let () =
  let _ = Sys.opaque_identity User1.pair () in
  let _ = Sys.opaque_identity User2.pair () in
  let _ = Sys.opaque_identity User3.call_user1 () in
  let #(a, b) = User1.call_between () in
  let #(c, d) = User2.call_between () in
  assert (box a = 2.0 && box b = 1.0 && box c = 4.0 && box d = 3.0)

let expected_symbol_res =
  [ "camlLpoly_lib__Lpoly_lib_lpoly_pair_.*float64_float64__cohort_code$";
    "camlLpoly_lib__Lpoly_lib_lpoly_call_between_.*float64_float64__cohort_code$"
  ]

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
