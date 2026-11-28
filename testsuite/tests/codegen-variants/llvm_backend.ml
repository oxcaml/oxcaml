(* TEST
 codegen_variant = "asan-fp";
 variant_available;
 include ocamlcommon;
 include ocamloptcomp;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 run;
 check-program-output;
*)

(* What the driver checks of the codegen variant before it generates native
   code or links, for the LLVM backend, which emits neither AddressSanitizer
   instrumentation nor stack checks.  In an AddressSanitizer variant, where
   this test runs, the backend is refused for an instrumented unit and
   accepted for one compiled with -fno-asan, which stays a unit of the
   variant.  Each refusal names flags that can be passed.  (The backend
   itself needs an LLVM toolchain, so the
   checks are called here as the driver calls them.) *)

let check description =
  match Oxcaml_flags.validate_codegen_variant () with
  | () -> Printf.printf "%s: accepted\n%!" description
  | exception Compenv.Exit_with_status _ ->
    Printf.printf "%s: refused\n%!" description

let () =
  Clflags.llvm_backend := true;
  Clflags.frame_pointers := true;
  Clflags.stack_checks := false;
  Clflags.address_sanitizer := true;
  check "An instrumented unit";
  Oxcaml_flags.asan_instrumentation := false;
  check "A unit compiled with -fno-asan";
  Clflags.address_sanitizer := false;
  Clflags.stack_checks := true;
  check "A unit with stack checks"
