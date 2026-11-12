(** [load dot_merlin_file] reads the .merlin file at [dot_merlin_file]. It
    returns a list of directives from the file, with all paths made absolute and
    findlib packages resolved. *)
val load : string -> Merlin_dot_protocol.directive list
