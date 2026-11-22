(* TEST
 set OCAMLLIB = "${ocamlsrcdir}/stdlib";
 flags = "-I ${ocamlsrcdir}/stdlib/unix ${ocamlsrcdir}/stdlib/unix/unix.cmxa";
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
   codegen_variant = "poll";
   variant_available;
   compiler_directory_suffix = ".poll";
   native;
 }{
   codegen_variant = "fp-sc";
   variant_available;
   compiler_directory_suffix = ".fp-sc";
   native;
 }{
   codegen_variant = "fp-poll";
   variant_available;
   compiler_directory_suffix = ".fp-poll";
   native;
 }{
   codegen_variant = "poll-sc";
   variant_available;
   compiler_directory_suffix = ".poll-sc";
   native;
 }{
   codegen_variant = "fp-poll-sc";
   variant_available;
   compiler_directory_suffix = ".fp-poll-sc";
   native;
 }{
   codegen_variant = "asan-fp";
   variant_available;
   compiler_directory_suffix = ".asan-fp";
   native;
 }{
   codegen_variant = "asan-fp-poll";
   variant_available;
   compiler_directory_suffix = ".asan-fp-poll";
   native;
 }
*)

(* Build systems name the libraries of the installation by path inside the
   default library root, whatever the codegen variant: here the root is the
   one OCAMLLIB names, and the unix library is given as an include directory
   and as an archive, both inside it.  The compiler has to use the copies in
   the tree of the variant it compiles for (the first block is the variant
   of the test run; the others each select one).  The call below needs
   unix.cmx when it is compiled, and the link needs unix.cmxa: with the
   default tree's copies, one or the other is refused. *)

let () =
  Unix.handle_unix_error
    (fun () -> Printf.printf "pid > 0: %b\n" (Unix.getpid () > 0))
    ()
