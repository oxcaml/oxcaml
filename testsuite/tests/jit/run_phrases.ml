(* Compiles toplevel phrases and runs them in this process with the JIT. *)

let run phrases =
  Clflags.set_target Clflags.Target.Native;
  Clflags.Opt_flag_handler.set Oxcaml_flags.opt_flag_handler;
  Jit.init_top ();
  Opttoploop.initialize_toplevel_env ();
  List.iter
    (fun phrase ->
      match Opttoploop.execute_phrase false Format.std_formatter phrase with
      | true -> ()
      | false -> print_endline "the phrase failed"
      | exception exn ->
        Printf.printf "exception: %s\n%!" (Printexc.to_string exn))
    (!Opttoploop.parse_use_file (Lexing.from_string phrases))
