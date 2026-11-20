(* TEST
 readonly_files = "ppx_shared_location.ml";
 native-compiler;
 setup-ocamlopt.byte-build-env;
 {
   include ocamlcommon;
   program = "${test_build_directory}/ppx_shared_location.exe";
   all_modules = "ppx_shared_location.ml";
   ocamlopt.byte;
 }{
   program = "${test_build_directory}/names_shared_location.exe";
   flags = "-g -ppx ${test_build_directory}/ppx_shared_location.exe";
   all_modules = "names_shared_location.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* [first] and [second] call [callee] at the same location. Each call site
   should still get the name of the function it's in. *)

let[@inline never] callee () = Printexc.get_callstack 2

let[@inline never] first () = Sys.opaque_identity [%shared_loc callee ()]

let[@inline never] second () = Sys.opaque_identity [%shared_loc callee ()]

let print_caller name f =
  match Printexc.backtrace_slots (f ()) with
  | Some [| _; caller |] ->
    let name_of_caller =
      Option.value (Printexc.Slot.name caller) ~default:"<none>"
    in
    let location =
      match Printexc.Slot.location caller with
      | Some { line_number; start_char; end_char; _ } ->
        Printf.sprintf "line %d, characters %d-%d"
          line_number start_char end_char
      | None -> "<none>"
    in
    Printf.printf "%s: called from %s at %s\n" name name_of_caller location
  | _ -> Printf.printf "%s: unexpected backtrace\n" name

let () =
  print_caller "first" first;
  print_caller "second" second
