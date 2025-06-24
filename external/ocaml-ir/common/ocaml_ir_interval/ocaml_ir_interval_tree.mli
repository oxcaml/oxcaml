module type Key = sig
  type t

  val compare : t -> t -> int
end

module Make : functor (Key : Key) -> sig
  type 'a t

  val of_alist : ((Key.t * Key.t) * 'a) list -> 'a t
    [@@ocaml.doc
      " [of_alist]: interval tree of all intervals in the list. Not tail-recursive. "]

  val fold_range
    :  'a t
    -> Key.t
    -> f:(interval:Key.t * Key.t -> data:'a -> 'b -> 'b)
    -> init:'b
    -> 'b
end
