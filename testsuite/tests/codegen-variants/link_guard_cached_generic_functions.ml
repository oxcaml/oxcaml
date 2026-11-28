(* TEST
 set OCAMLLIB = "${ocamlsrcdir}/stdlib";
 guard = "sh ${test_source_directory}/link_guard.sh";
 generate = "${ocamlsrcdir}/generate_cached_generic_functions.exe";
 cached = "-use-cached-generic-functions -cached-generic-functions-path";
 {
   codegen_variant = "-fp";
   variant_available;
   codegen_variant = "+fp";
   variant_available;
   compiler_directory_suffix = ".fp";
   setup-ocamlopt.byte-build-env;
   script = "${generate} cached";
   script;
   flags = "${cached} cached.${libext}";
   ocamlopt.byte;
   run;
   check-program-output;
   codegen_variant = "-fp";
   program = "other.exe";
   compiler_output = "compiler-output.raw";
   ocamlopt_byte_exit_status = "2";
   ocamlopt.byte;
   script = "${guard} refusal cached.${libext} compiler-output.raw";
   output = "refusal.output";
   script;
   reference = "${test_source_directory}/link_guard.undefined.reference";
   check-program-output;
 }{
   codegen_variant = "-sc";
   variant_available;
   codegen_variant = "+sc";
   variant_available;
   compiler_directory_suffix = ".sc";
   setup-ocamlopt.byte-build-env;
   script = "${generate} cached";
   script;
   flags = "${cached} cached.${libext}";
   ocamlopt.byte;
   run;
   check-program-output;
   codegen_variant = "-sc";
   program = "other.exe";
   compiler_output = "compiler-output.raw";
   ocamlopt_byte_exit_status = "2";
   ocamlopt.byte;
   script = "${guard} refusal cached.${libext} compiler-output.raw";
   output = "refusal.output";
   script;
   reference = "${test_source_directory}/link_guard.undefined.reference";
   check-program-output;
 }
*)

(* An archive of cached generic functions is generated for one frame-pointer,
   stack-check and AddressSanitizer setting, and it is a plain archive of
   object files: nothing in it tells the compiler which setting that was.
   Its members refer to the link-time guard of their setting, like every
   object file of generated code, so the linker refuses the archive in a
   program built for another one.

   Each block generates an archive for one codegen variant: the generator
   runs as a script, which gets the variant of the block the way ocamlopt
   does (from OCAMLPARAM, unless it is base). A program compiled for
   that variant links with the archive and runs; the same program compiled
   for another variant does not link.

   The program calls functions of unknown arity and applies functions
   partially, which is what the generic functions are for. *)

let apply3 f x y z = (Sys.opaque_identity f) x y z

let add3 x y z = x + y + z

let partial = Sys.opaque_identity (apply3 (fun a b c d -> a + b + c + d) 1 2 3)

let () =
  let l = List.map (Sys.opaque_identity (add3 1) 2) [1; 2; 3] in
  Printf.printf "%d %d %s\n%!" (apply3 add3 1 2 3) (partial 4)
    (String.concat "," (List.map string_of_int l))
