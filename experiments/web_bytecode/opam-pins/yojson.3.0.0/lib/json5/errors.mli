val string_of_position : Lexing.position -> string
(** [string_of_position pos] returns a string that contains the line and, if
  supplied, the filename of the position in a way that's appropriate to include
  in an error message *)
