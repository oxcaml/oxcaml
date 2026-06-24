(** Given a potential path to a .merlin file, check if it that path and if the
    corresponding path in the _build dir exists. Return a path to one of the two
    if either exists. *)
val find_in_src_or_build_dir : string -> string option

val exists_in_src_or_build_dir : string -> bool
