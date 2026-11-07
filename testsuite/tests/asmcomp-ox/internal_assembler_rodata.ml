(* TEST
 arch_amd64;
 not-windows;
 not-macos;
 flags = "-internal-assembler";
 setup-ocamlopt.opt-build-env;
 compile_only = "true";
 ocamlopt.opt;
 script = "sh ${test_source_directory}/internal_assembler_rodata.sh \
   ${test_build_directory}/internal_assembler_rodata.o";
 script;
*)

(* The internal assembler must mark [.rodata] as allocated (SHF_ALLOC),
   as GNU as does implicitly. Otherwise linkers that treat the section
   as non-allocated, such as lld, reject relocations within it. The jump
   table for this match is emitted into [.rodata]. *)

let[@inline never] f x =
  match x with
  | 0 -> 10
  | 1 -> 21
  | 2 -> 32
  | 3 -> 43
  | 4 -> 54
  | 5 -> 65
  | 6 -> 76
  | 7 -> 87
  | _ -> 0
