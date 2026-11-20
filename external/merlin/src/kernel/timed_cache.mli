(** A [('a, 'b) t] is a [Hashtbl]-backed cache, keyed by ['a] and having values of type ['b]. *)
type ('a, 'b) t

val create : unit -> ('a, 'b) t

(** [read_or_compute cache ~key ~f] looks looks up the value keyed by [key] in
    [cache]. If it doesn't exist, it is computed via [f] and inserted into the
    cache. *)
val read_or_compute : ('a, 'b) t -> key:'a -> f:(unit -> 'b) -> 'b

(** Delete all entries in the cache that were last accessed more than
    [older_than] seconds ago. Does nothing if [older_than] is [None]. *)
val flush : ?older_than:float -> (_, _) t -> unit
