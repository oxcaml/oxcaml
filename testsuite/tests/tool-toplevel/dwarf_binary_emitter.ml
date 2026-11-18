(* TEST
 {
   not-macos;
   no-address-sanitizer;
   flags = "-internal-assembler";
   toplevel.opt;
 }
*)

(* This test checks that the native toplevel does not crash with the
   internal assembler when debug info and the binary backend are enabled.
   The binary emitter does not support all directives emitted by the
   DWARF emitter, so DWARF must be disabled for toplevel compilation.

   It needs compiler-libs, which AddressSanitizer-default installs do not
   provide for the asan-off variant the native toplevel is built for. *)
#directory "+compiler-libs";;
Clflags.debug := true;;
Emitaux.binary_backend_available := true;;
let x = 1;;
