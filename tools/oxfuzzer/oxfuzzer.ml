let usage_msg = "Usage: oxfuzzer -seed <seed>"

let seed : int option ref = ref None

let speclist =
  [ ( "-seed",
      Arg.Int (fun n -> seed := Some n),
      "<int>  Random number generator initial seed (required)" ) ]

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
    Printf.printf "%s" (Program.to_code program)
