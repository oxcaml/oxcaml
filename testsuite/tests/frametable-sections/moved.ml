(* TEST
 readonly_files = "hot_first.ld check_moved.sh";
 native-compiler;
 frametable_sections;
 unset OCAMLPARAM;
 setup-ocamlopt.byte-build-env;
 flags = "-g -function-sections -frametable-sections";
 module = "moved.ml";
 ocamlopt.byte;
 module = "";
 all_modules = "moved.cmx";
 program = "${test_build_directory}/moved.exe";
 flags = "-g -ccopt -Wl,-T,hot_first.ld -ccopt -Wl,--gc-sections";
 ocamlopt.byte;
 script = "sh ${test_source_directory}/check_moved.sh";
 script;
 run;
 check-program-output;
*)

(* A linker script moves [hot]'s code ahead of all other code, so its frame
   descriptors, which follow its code, are placed apart from the rest of the
   unit's. The runtime must still find them: a major GC scans [hot]'s frames,
   and a backtrace crosses them. *)

exception Boom of int

let[@inline never] rec hot n acc =
  if n = 0
  then (
    Gc.full_major ();
    if List.length acc > 0 then raise (Boom (List.length acc));
    0)
  else hot (n - 1) (string_of_int n :: acc) + 1

let () =
  Printexc.record_backtrace true;
  try ignore (hot 3 []) with
  | Boom n ->
    Printf.printf "caught Boom %d\n" n;
    print_string (Printexc.get_backtrace ())
