type t = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
}
val none : t
val init : Lexing.lexbuf -> string -> unit
val symbol_rloc: unit -> t
val print_loc: Format.formatter -> t -> unit
