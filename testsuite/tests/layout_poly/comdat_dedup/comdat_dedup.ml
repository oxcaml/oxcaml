(* TEST
 arch_amd64;
 not-macos;
 flags = "-extension-universe alpha";
 readonly_files = "lpoly_lib.mli lpoly_lib.ml user1.ml user2.ml";
 setup-ocamlopt.opt-build-env;
 all_modules = "lpoly_lib.mli lpoly_lib.ml user1.ml user2.ml comdat_dedup.ml";
 ocamlopt.opt;
 run;
*)

(* Regression test for COMDAT deduplication of monomorphized
   layout-polymorphic function instances.

   [user1.ml] and [user2.ml] both monomorphize [Lpoly_lib.lpoly_pair] at
   (#float64, #float64), so each object file carries weak copies of

   - [caml__Lpoly_lib_0_float64_float64_code] — the compiled body, and
   - [caml__Lpoly_lib_0_float64_float64]      — the closure-record block
     (a zero-value-slot set of closures; see [slambda_fracture.ml] for why
      it is always closed from flambda2's perspective).

   After linking, the ELF COMDAT mechanism (.weak linkage + comdat
   section group) must collapse each of these symbols down to a single
   definition in the final binary.

   The test does double duty: it runs the linked program (must not crash)
   and then shells out to [nm] to verify the symbol table. *)

(* Force both instantiations to reach the linker. *)
let () =
  let _ = Sys.opaque_identity User1.pair () in
  let _ = Sys.opaque_identity User2.pair () in
  ()

(* Symbols we expect to see deduplicated to exactly one copy each:
   - ..._code : the function body
   - (no suffix) : the closure-record block *)
let expected_symbol_res =
  [ "caml__Lpoly_lib_.*_float64_float64_code$";
    "caml__Lpoly_lib_.*_float64_float64$"
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
      then (
        ok := false;
        Printf.eprintf
          "expected exactly one copy of weak symbol matching /%s/; \
           relevant nm output:\n%!"
          re;
        let dump = Printf.sprintf "nm %s | grep Lpoly_lib >&2 || true" exe in
        let _ = Sys.command dump in
        ()))
    expected_symbol_res;
  if not !ok then exit 1
