(** [load dot_merlin_file] reads the .merlin file at [dot_merlin_file]. It
    returns a list of directives from the file, with all paths made absolute and
    findlib packages resolved. *)
val load : string -> Merlin_dot_protocol.directive list

(** Delete all entries in the cache that were last accessed more than
    [older_than] seconds ago. Does nothing if [older_than] is [None]. *)
val flush : ?older_than:float -> unit -> unit
