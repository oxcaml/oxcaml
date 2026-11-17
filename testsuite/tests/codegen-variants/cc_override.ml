(* TEST
 modules = "cc_override_.c";
 flags = "-cc 'sh ${test_source_directory}/cc_override.sh ${cc}'";
 {
   native;
 }{
   codegen_variant = "base";
   variant_available;
   compiler_directory_suffix = ".base";
   native;
 }{
   codegen_variant = "fp";
   variant_available;
   compiler_directory_suffix = ".fp";
   native;
 }{
   codegen_variant = "sc";
   variant_available;
   compiler_directory_suffix = ".sc";
   native;
 }{
   codegen_variant = "fp-sc";
   variant_available;
   compiler_directory_suffix = ".fp-sc";
   native;
 }{
   codegen_variant = "asan-fp";
   variant_available;
   compiler_directory_suffix = ".asan-fp";
   native;
 }
*)

(* A C compiler given with -cc replaces the configured command and its
   flags, among them the ones that the default variant needs.  ocamlopt
   therefore gives it the flags of every axis that is on, whether or not
   the axis is on by default: -fno-omit-frame-pointer for a stub of a
   variant with frame pointers, and -fsanitize=address, for the stub and
   for the link, under AddressSanitizer (without it at the link, the
   program of an AddressSanitizer variant does not link).  The first block
   is the variant of the test run, which is the configure default unless
   the run selects another one; the others each select one. *)

external frame_pointers : unit -> bool = "cc_override_frame_pointers"

external sanitizer : unit -> bool = "cc_override_sanitizer"

let () =
  Printf.printf
    "-fno-omit-frame-pointer exactly when the variant has frame pointers: %b\n"
    (frame_pointers ());
  Printf.printf
    "-fsanitize=address exactly when the variant has AddressSanitizer: %b\n"
    (sanitizer ())
