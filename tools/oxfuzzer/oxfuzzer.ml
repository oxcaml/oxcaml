let usage_msg = "Usage: oxfuzzer -seed <seed> [-cpp-output <file>]"

let seed : int option ref = ref None

let cpp_output : string option ref = ref None

let speclist =
  [ ( "-seed",
      Arg.Int (fun n -> seed := Some n),
      "<int>  Random number generator initial seed (required)" );
    ( "-cpp-output",
      Arg.String (fun path -> cpp_output := Some path),
      "<file>  Also emit an independent C++17 reference from the same IR" ) ]

let () =
  Arg.parse speclist
    (fun _ -> raise (Arg.Bad "Unexpected anonymous argument"))
    usage_msg;
  match !seed with
  | None ->
    Arg.usage speclist usage_msg;
    exit 2
  | Some seed ->
    let random = Random.State.make [| seed |] in
    let program = Generator.generate random in
    Option.iter
      (fun path ->
        Out_channel.with_open_bin path (fun channel ->
            output_string channel (Cpp.to_code program)))
      !cpp_output;
    Printf.printf "%s" (Program.to_code program)
