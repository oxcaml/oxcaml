let is_interesting_line line =
  List.exists
    (fun prefix -> String.starts_with ~prefix line)
    ["  use_regalloc:"; "  regalloc_params:"; "cfg for "]
  && not (String.ends_with ~suffix:"__entry" line)

let function_name =
  match Sys.argv.(1) with
  | "flat" -> Str.regexp "cfg for caml\\(.*\\)_[0-9]+_[0-9]+\\(_code\\)?"
  | "structured" -> Str.regexp "cfg for _Caml\\(.*\\)_[0-9]+_[0-9]+\\(_code\\)?"
  | unsupported ->
    failwith (Printf.sprintf "Unsupported name-mangling scheme: %S" unsupported)

let remove_suffix line =
  match Str.string_match function_name line 0 with
  | false -> line
  | true -> Str.matched_group 1 line

let () =
  let chan = open_in Sys.argv.(2) in
  try
    while true do
      let line = input_line chan in
      if is_interesting_line line then print_endline (remove_suffix line)
    done
  with End_of_file -> close_in_noerr chan
