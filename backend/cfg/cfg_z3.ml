[@@@ocaml.warning "+a-40-41-42"]

let run_z3 code =
  let with_temp_file suffix f =
    let filename = Filename.temp_file "oxcaml-z3-" suffix in
    Misc.try_finally
      (fun () -> f filename)
      ~always:(fun () -> Misc.remove_file filename)
  in
  with_temp_file ".smt2" @@ fun input_file ->
  with_temp_file ".out" @@ fun output_file ->
  Out_channel.with_open_text input_file (fun out_channel ->
      Out_channel.output_string out_channel code);
  let command =
    Filename.quote_command "z3" ["-smt2"; input_file] ~stderr:output_file
      ~stdout:output_file
  in
  let ret = Ccomp.command command in
  let output = In_channel.with_open_text output_file In_channel.input_all in
  if ret <> 0
  then
    Misc.fatal_errorf "Z3 failed with return code %d. Input: @.%s@.Output: @.%s"
      ret code output;
  output

let run_validation_fallback code =
  match run_z3 code |> String.trim with
  | "unsat" -> "Z3 accepted the compiler result; internal Datalog failed"
  | "sat" -> "Z3 also rejected the compiler result"
  | output -> Format.sprintf "unexpected Z3 output: %S" output
  | exception exn -> Format.sprintf "Z3 raised: %s" (Printexc.to_string exn)

let fmt_fact fmt relation arguments =
  let fmt_argument fmt argument = Format.fprintf fmt " %s" argument in
  Format.fprintf fmt "(rule (%s%a))@." relation
    (Format.pp_print_list fmt_argument)
    arguments
