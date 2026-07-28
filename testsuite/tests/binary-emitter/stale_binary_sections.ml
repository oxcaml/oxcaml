(* TEST
 arch_arm64;
 flambda2;
 readonly_files = "lib.ml";
 setup-ocamlopt.byte-build-env;
 ocamlopt_flags = "-verify-binary-emitter -flambda2-inline-small-function-size 1";
 module = "lib.ml";
 ocamlopt.byte;
 {
   ocamlopt_flags = "-verify-binary-emitter -flambda2-inline-threshold 0";
   module = "stale_binary_sections.ml";
   ocamlopt.byte;
 }{
   ocamlopt_flags = "-verify-binary-emitter -O3";
   module = "stale_binary_sections.ml";
   ocamlopt.byte;
 }
*)

(* Regression test for stale files in the [<prefix>.binary-sections]
   directory used by [-verify-binary-emitter].

   This module is compiled twice with the same output prefix. [Lib] is
   compiled with a small function size that makes [f] speculatively
   inlinable rather than must-inline, so that the call sites below are
   governed by the inlining threshold. The first compilation uses a zero
   threshold: the tail call to [Lib.f] survives, so the text section
   contains a relocation for it, which the binary emitter records in
   [stale_binary_sections.binary-sections/section_text.relocs]. The second
   compilation, at [-O3], inlines [Lib.f], leaving no text relocations at
   all, in which case no relocations file is written. If the relocations
   file from the first compilation is not removed, verification compares
   the first compilation's relocations against the second compilation's
   object file and fails with a relocation mismatch. *)

let h x = Lib.f x
