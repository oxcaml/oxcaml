(* Values in a functor's result strengthen fully: modalities and arrow modes alike,
   whether the functor body is a struct or an application. *)

module Make (Key : sig
    type t
  end) : sig
  val find : Key.t -> Key.t
  val consume : Key.t -> unit
end

module Make_applied (Key : sig
    type t
  end) : sig
  val find : Key.t -> Key.t
  val consume : Key.t -> unit
end
