module Float_u = struct

  type t = float#

  external to_float : t -> (float[@local_opt]) @@ portable =
    "%box_float" [@@warning "-187"]

  external of_float : (float[@local_opt]) -> t @@ portable =
    "%unbox_float" [@@warning "-187"]

end

let[@inline never][@local never] permute (a : Float_u.t) (b : Float_u.t) (c : Float_u.t) (d : Float_u.t) = #(d, c, b, a)
