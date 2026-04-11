let capture_output f =
  flush_all ();
  let read_fd, write_fd = Unix.pipe () in
  let saved_stdout = Unix.dup Unix.stdout in
  let saved_stderr = Unix.dup Unix.stderr in
  let restore () =
    flush_all ();
    Unix.dup2 saved_stdout Unix.stdout;
    Unix.dup2 saved_stderr Unix.stderr;
    Unix.close saved_stdout;
    Unix.close saved_stderr
  in
  Unix.dup2 write_fd Unix.stdout;
  Unix.dup2 write_fd Unix.stderr;
  Unix.close write_fd;
  let result =
    try Ok (f ())
    with exn -> Error exn
  in
  restore ();
  let input = Unix.in_channel_of_descr read_fd in
  let output =
    Fun.protect
      (fun () -> In_channel.input_all input)
      ~finally:(fun () -> close_in_noerr input)
  in
  match result with
  | Ok value -> output, value
  | Error exn -> raise exn

let check_string ~filename ~source =
  Web_bytecode_check.check_string ~browser:false ~filename ~source

let run_string ~filename ~source =
  let output, diagnostics =
    capture_output (fun () ->
        Web_bytecode_run.run_string ~browser:false ~filename ~source)
  in
  if String.equal diagnostics "" then output else output ^ diagnostics
