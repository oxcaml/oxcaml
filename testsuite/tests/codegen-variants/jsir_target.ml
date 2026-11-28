(* TEST
 include ocamlcommon;
 include ocamloptcomp;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 run;
 check-program-output;
*)

(* What the driver checks of the codegen variant before it generates code
   or links, by target.  A JSIR compilation ignores the selection of axes,
   so a selection that is refused for native code is accepted for
   js_of_ocaml and wasm_of_ocaml: here AddressSanitizer without frame
   pointers, which no platform has.  (The check is called as the driver
   calls it, so that the test does not need the JSIR tools;
   jsir_ignores_axes.ml links with them.) *)

let check description target =
  Clflags.set_target target;
  match Oxcaml_flags.validate_codegen_variant () with
  | () -> Printf.printf "%s: accepted\n%!" description
  | exception Compenv.Exit_with_status _ ->
    Printf.printf "%s: refused\n%!" description

let () =
  Clflags.address_sanitizer := true;
  Clflags.frame_pointers := false;
  Clflags.stack_checks := false;
  check "Native code" Native;
  check "js_of_ocaml" Js_of_ocaml;
  check "wasm_of_ocaml" Wasm_of_ocaml
