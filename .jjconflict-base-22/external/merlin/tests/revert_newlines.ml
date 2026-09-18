(* merlin-wrapper, which the tests use to call ocamlmerlin, replaces \n occurrences
   inside string literals with actual newlines for readability. This results in invalid
   json, which jq cannot handle. This script undoes that replacement of \n occurrences. *)

type parser_state =
  | In_a_string of { prev_char_was_escape : bool }
  | Not_in_a_string

let transform_str s =
  String.to_seq s |> List.of_seq
  |> List.fold_left_map
       (fun state curr_char ->
         let new_state, new_str =
           match (state, curr_char) with
           (* Toggle whether we're in a state *)
           | In_a_string { prev_char_was_escape = false }, '"' ->
             (Not_in_a_string, None)
           | Not_in_a_string, '"' ->
             (In_a_string { prev_char_was_escape = false }, None)
           (* Deal with backslashes *)
           | In_a_string { prev_char_was_escape = true }, '"' ->
             (In_a_string { prev_char_was_escape = false }, None)
           | In_a_string { prev_char_was_escape = false }, '\\' ->
             (In_a_string { prev_char_was_escape = true }, None)
           (* Deal with newline *)
           | In_a_string { prev_char_was_escape = _ }, '\n' ->
             (In_a_string { prev_char_was_escape = false }, Some "\\n")
           (* Everything else *)
           | Not_in_a_string, _ -> (Not_in_a_string, None)
           | In_a_string { prev_char_was_escape = _ }, _ ->
             (In_a_string { prev_char_was_escape = false }, None)
         in
         let new_str =
           Option.value new_str ~default:(String.make 1 curr_char)
         in
         (new_state, new_str))
       Not_in_a_string
  |> snd |> List.to_seq |> Seq.flat_map String.to_seq |> String.of_seq

let () =
  let input = In_channel.input_all stdin in
  let output = transform_str input in
  print_string output
