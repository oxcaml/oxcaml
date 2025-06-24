module Monad = struct
  module type S = sig
    type nonrec 'a t

    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
    val map : 'a t -> f:('a -> 'b) -> 'b t

    module Let_syntax : sig
      module Let_syntax : sig
        val return : 'a -> 'a t
        val bind : 'a t -> f:('a -> 'b t) -> 'b t
        val map : 'a t -> f:('a -> 'b) -> 'b t
        val both : 'a t -> 'b t -> ('a * 'b) t

        module Open_on_rhs : sig end
      end
    end
  end

  module type Arg = sig
    type nonrec 'a t

    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
    val map : 'a t -> f:('a -> 'b) -> 'b t
  end
end
