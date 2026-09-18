(* Shapes from core/map.ml: a nested module sealed by an inline ascription, and a
   same-named module shadowed at two levels. *)

module Shadowed = struct
  module Tree = struct
    type t = { leaf : unit }

    let id (x : t) = x
  end
end

module Tree : sig
  type t

  val id : t -> t
end = struct
  type t = { leaf : unit }

  let id (x : t) = x
end
