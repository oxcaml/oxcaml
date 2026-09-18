module Map : sig
  module type S = sig
    include MoreLabels.Map.S

    val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

    val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
  end

  module type Key = sig
    include MoreLabels.Map.OrderedType

    val pp : Format.formatter -> t -> unit

    val show : t -> string
  end

  module Make (Key : Key) : S with type key = Key.t
end

module Option : sig
  include module type of struct
    include Stdlib.Option
  end

  module Op : sig
    val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option

    val ( let+ ) : 'a option -> ('a -> 'b) -> 'b option
  end
end

module Result : sig
  include module type of struct
    include Stdlib.Result
  end

  module Op : sig
    val ( let* ) :
      ('a, 'err) result -> ('a -> ('b, 'err) result) -> ('b, 'err) result

    val ( let+ ) : ('a, 'err) result -> ('a -> 'b) -> ('b, 'err) result
  end

  module List : sig
    val map_all :
      f:('a -> ('b, 'err) result) -> 'a list -> ('b list, 'err list) result

    val iter_all :
      f:('a -> (unit, 'err) result) -> 'a list -> (unit, 'err list) result
  end
end

module String : sig
  include module type of struct
    include StringLabels
  end

  module Map : Map.S with type key = string

  val starts_with : prefix:string -> string -> bool
end
