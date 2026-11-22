(* TEST
 native-compiler;
 set OCAMLLIB = "${ocamlsrcdir}/stdlib";
 codegen_variant = "+poll";
 variant_available;
 shared-libraries;
 native-dynlink;
 not-macos;
 readonly_files = "poll_object_unit.ml poll_object_loop.ml";
 setup-ocamlopt.byte-build-env;
 script = "sh ${test_source_directory}/poll_object_images.sh";
 script;
 check-program-output;
*)

(* A process can hold more than one OCaml image: a program and a shared
   object made with -output-complete-obj, each with its own startup code.
   An image that is not fully polled defines the symbol that the object
   files without polls refer to (see poll_object.ml), and a shared object
   exports what it defines: here that symbol alone, so that the program
   keeps its own runtime. That definition must not serve another image:
   a program that would be marked fully polled says, in its startup code,
   that the symbol may only be defined inside it, and still fails to link
   with an object file without polls. A program without such an object
   file links with the shared object and is fully polled. A plugin whose
   header says that its unit has polls says the same in its startup code:
   a program that is linked with the shared object would otherwise load
   it, whatever its object file is. So that plugin does not link either.

   This relies on the visibility that ELF gives to an undefined symbol:
   there is no such thing in Mach-O, where the dynamic linker of a fully
   polled program refuses such a plugin when no other image is there
   (poll_object_plugin.ml).

   The script links the programs; this file is the code of the second
   image. *)

let image = Sys.opaque_identity "another image"
