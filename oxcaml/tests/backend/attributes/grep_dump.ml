#load "str.cma"

let is_interesting_line line =
  List.exists
    (fun prefix -> String.starts_with ~prefix line)
    ["use_regalloc="; "regalloc_params="]

let function_name = Str.regexp "caml\\(.*\\)_[0-9]+_[0-9]+\\(_code\\)?"

let remove_suffix line =
  match Str.string_match function_name line 0 with
  | false -> line
  | true -> Str.matched_group 1 line

let () =
  try
    let prev_line = ref "" in
    while true do
      let line = read_line () in
      if is_interesting_line line
      then begin
        if not (is_interesting_line !prev_line)
        then print_endline (remove_suffix !prev_line);
        print_endline line
      end;
      prev_line := line
    done
  with End_of_file -> ()
