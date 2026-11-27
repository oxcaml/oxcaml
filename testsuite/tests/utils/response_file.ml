(* TEST
 flags = "-I ${ocamlsrcdir}/utils";
 include ocamlcommon;
 {
   bytecode;
 }{
   native;
 }
*)

(* The name of a response file is one argument of the command, even when it
   contains a space. *)
let () =
  let dir = "temporary directory" in
  Sys.mkdir dir 0o777;
  (* Runs after the removal of the response file, which is set up later. *)
  at_exit (fun () -> Sys.rmdir dir);
  Filename.set_temp_dir_name dir;
  let files = List.init 2000 (Printf.sprintf "%040d.o") in
  let arguments = Ccomp.quote_files ~response_files:true files in
  (* Short enough that it is a response file, not the files themselves. *)
  assert (String.length arguments < 1000);
  let one_response_file =
    "test $# -eq 1 && test \"@${1#@}\" = \"$1\" && test -f \"${1#@}\""
  in
  assert (Sys.command ("sh -c '" ^ one_response_file ^ "' sh " ^ arguments) = 0)
