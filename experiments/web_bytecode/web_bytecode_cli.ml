let read_source path =
  if String.equal path "-"
  then In_channel.input_all In_channel.stdin
  else In_channel.with_open_bin path In_channel.input_all

let usage () =
  prerr_endline "usage: web_bytecode_cli (check|interface|run|utop) <file|-> [logical-filename]";
  exit 2

let () =
  if Array.length Sys.argv <> 3 && Array.length Sys.argv <> 4 then usage ();
  let mode = Sys.argv.(1) in
  let path = Sys.argv.(2) in
  let source = read_source path in
  let filename =
    if Array.length Sys.argv = 4 then Sys.argv.(3)
    else if String.equal path "-" then "stdin.ml"
    else path
  in
  let output =
    match mode with
    | "check" -> Web_bytecode_native.check_string ~filename ~source
    | "interface" -> Web_bytecode_native.interface_string ~filename ~source
    | "run" -> Web_bytecode_native.run_string ~filename ~source
    | "utop" -> Web_bytecode_native.utop_string ~filename ~source
    | _ -> usage ()
  in
  output_string stdout output;
  flush stdout
