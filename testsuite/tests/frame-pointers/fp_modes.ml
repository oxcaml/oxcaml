(* TEST
 {
   codegen_variant = "+fp";
   variant_available;
   compiler_directory_suffix = ".fp";
   reference = "${test_source_directory}/fp_modes.fp.reference";
   native;
 }{
   codegen_variant = "-fp";
   variant_available;
   compiler_directory_suffix = ".base";
   reference = "${test_source_directory}/fp_modes.base.reference";
   native;
 }
*)

(* One compiler, both frame-pointer modes: each block builds and runs this
   program for an explicitly selected codegen variant and checks that
   %frame_pointers reflects the selected mode. *)

external frame_pointers : bool = "%frame_pointers"

let () = Printf.printf "frame_pointers: %b\n" frame_pointers
