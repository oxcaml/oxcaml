(** Provides information about the kind of the thing the cursor is on. If the cursor is
    on an expression, it returns the kind of the type of the expression. If the cursor is
    on a type, it returns the kind of the type. If the cursor is on a kind abbreviation,
    it returns the expansion of the kind abbreviation. *)

module Kind_info : sig
  type t

  val to_string : verbosity:Mconfig.Verbosity.t -> t -> string
end

val from_mbrowse :
  Mbrowse.t -> cursor:Lexing.position -> (Location.t * Kind_info.t) list
