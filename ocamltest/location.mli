type t = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}
val none : t
val init : Lexing.lexbuf -> string -> unit
val symbol_rloc: unit -> t
val symbol_gloc: unit -> t
val print_loc: Format.formatter -> t -> unit
