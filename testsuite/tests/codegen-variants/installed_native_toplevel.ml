(* TEST
 flags = "-I +compiler-libs -I +unix -I +dynlink -linkall";
 libraries = "unix dynlink ocamlcommon ocamlfrontend ocamloptcomp";
 libraries += " ocamlopttoplevel";
 {
   codegen_variant = "base";
   variant_available;
   arguments = "base";
   compiler_directory_suffix = ".base";
   native;
 }{
   codegen_variant = "fp";
   variant_available;
   arguments = "fp";
   compiler_directory_suffix = ".fp";
   native;
 }{
   codegen_variant = "sc";
   variant_available;
   arguments = "sc";
   compiler_directory_suffix = ".sc";
   native;
 }{
   codegen_variant = "fp-sc";
   variant_available;
   arguments = "fp-sc";
   compiler_directory_suffix = ".fp-sc";
   native;
 }{
   codegen_variant = "poll";
   variant_available;
   arguments = "poll";
   compiler_directory_suffix = ".poll";
   native;
 }{
   codegen_variant = "asan-fp";
   variant_available;
   arguments = "asan-fp";
   compiler_directory_suffix = ".asan-fp";
   native;
 }{
   codegen_variant = "fp-poll";
   variant_available;
   arguments = "fp-poll";
   compiler_directory_suffix = ".fp-poll";
   native;
 }{
   codegen_variant = "poll-sc";
   variant_available;
   arguments = "poll-sc";
   compiler_directory_suffix = ".poll-sc";
   native;
 }{
   codegen_variant = "fp-poll-sc";
   variant_available;
   arguments = "fp-poll-sc";
   compiler_directory_suffix = ".fp-poll-sc";
   native;
 }{
   codegen_variant = "asan-fp-poll";
   variant_available;
   arguments = "asan-fp-poll";
   compiler_directory_suffix = ".asan-fp-poll";
   native;
 }
*)

(* The native toplevel library that a program links is the build of it for
   the program's codegen variant: it compiles phrases for that variant,
   against the libraries of that variant's tree, and loads them into the
   program. Each block passes the name of its variant to the program. *)

let () =
  Clflags.set_target Clflags.Target.Native;
  Clflags.Opt_flag_handler.set Oxcaml_flags.opt_flag_handler;
  let host_variant = Codegen_variant.to_string Opttoploop.host_variant in
  if not (String.equal host_variant Sys.argv.(1))
  then
    Printf.printf "a program built for %s has host variant %s\n" Sys.argv.(1)
      host_variant;
  Opttoploop.initialize_toplevel_env ();
  let phrases =
    !Opttoploop.parse_use_file
      (Lexing.from_string
         "let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2);;\n\
          let () = Printf.printf \"fib 20 = %d\\n%!\" (fib 20);;")
  in
  List.iter
    (fun phrase ->
      if not (Opttoploop.execute_phrase false Format.std_formatter phrase)
      then print_endline "the phrase failed")
    phrases
