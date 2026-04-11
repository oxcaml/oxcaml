let string_of_position { Lexing.pos_lnum; pos_fname; _ } =
  match pos_fname with
  | "" -> Printf.sprintf "Line %d" pos_lnum
  | fname -> Printf.sprintf "File %s, line %d" fname pos_lnum
