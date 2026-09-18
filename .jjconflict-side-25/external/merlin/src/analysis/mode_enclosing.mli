(** Provides mode information for expressions around the cursor. *)

module Mode_info : sig
  type t

  val to_string : verbosity:Mconfig.Verbosity.t -> t -> string
end

val from_mbrowse : Mbrowse.t -> (Location.t * Mode_info.t) list
