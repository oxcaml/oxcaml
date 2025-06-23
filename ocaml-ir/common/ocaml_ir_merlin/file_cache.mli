module Make :
functor (Input :
  sig type t val read : string -> t val cache_name : string end) ->
  sig
    val read : string -> Input.t
    val flush : ?older_than:float -> unit -> unit
    val clear : unit -> unit
    val get_cached_entry : string -> Input.t[@@ocaml.doc
                                              " @raise Not_found if the file is not in cache. "]
  end
