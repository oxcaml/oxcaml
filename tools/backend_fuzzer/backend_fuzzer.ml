(* CR-soon hwasilewski: Add a seed option to make generation deterministic. *)
let () =
  let random = Random.State.make_self_init () in
  let program = Generator.generate random in
  Printf.printf "%s" (Program.to_code program)
