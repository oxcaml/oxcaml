let is_interesting_line line =
  List.exists
    (fun prefix -> String.starts_with ~prefix line)
    ["  use_regalloc:"; "  regalloc_params:"; "cfg for "]
  && not (String.ends_with ~suffix:"__entry" line)

let function_name = Str.regexp "cfg for caml\\(.*\\)_[0-9]+_[0-9]+\\(_code\\)?"

let remove_suffix line =
  match Str.string_match function_name line 0 with
  | false -> line
  | true -> Str.matched_group 1 line

(* Each [-dcfg] dump is introduced by a "*** <message>" header. The
   push_pop_around_calls pass emits extra dumps for debugging; skip their
   contents so they don't show up in the expected output. *)
let mentions_push_pop =
  let re = Str.regexp_string "push_pop_around_calls" in
  fun line ->
    try
      ignore (Str.search_forward re line 0);
      true
    with Not_found -> false

let () =
  let chan = open_in Sys.argv.(1) in
  let skip = ref false in
  try
    while true do
      let line = input_line chan in
      if String.starts_with ~prefix:"*** " line
      then skip := mentions_push_pop line
      else if (not !skip) && is_interesting_line line
      then print_endline (remove_suffix line)
    done
  with End_of_file -> close_in_noerr chan
