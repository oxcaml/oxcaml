(** Values used by a module, but defined by a module further up the dependency
    order, to break dependency cycles. *)

(** Functions defined later. *)
module Fun : sig
  type 'f t constraint 'f = _ -> _

  (** [create ~name] is a function that is not yet defined. [name] (e.g.
      ["Ctype.upon_shape"]) is used in error messages. *)
  val create : name:string -> ('a -> 'b) t

  (** [define_exn t f] defines [t] as [f].

      Raises a fatal error if [t] is already defined. *)
  val define_exn : ('a -> 'b) t -> ('a -> 'b) -> unit

  (** [get t x] is [f x], where [t] was defined as [f].

      Raises a fatal error if [t] is not yet defined. *)
  val get : ('a -> 'b) t -> 'a -> 'b
end
