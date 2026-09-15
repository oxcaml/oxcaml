type 'a t = { mutable v : 'a; }
val make : 'a -> 'a t
val get : 'a t -> 'a
val set : 'a t -> ('a -> unit) @ local @@ noalloc_strict
val exchange : 'a t -> 'a -> 'a
val compare_and_set : 'a t -> 'a -> 'a -> bool
val fetch_and_add : int t -> (int -> int) @ local @@ noalloc_strict
val incr : int t -> unit @@ noalloc_strict
val decr : int t -> unit @@ noalloc_strict
