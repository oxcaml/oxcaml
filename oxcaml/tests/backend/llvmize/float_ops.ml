module Float_u = struct

  type t = float#

  external to_float : t -> (float[@local_opt]) @@ portable =
    "%box_float" [@@warning "-187"]

  external of_float : (float[@local_opt]) -> t @@ portable =
    "%unbox_float" [@@warning "-187"]

  let[@inline always] add x y = of_float (Float.add (to_float x) (to_float y))

  let[@inline always] sub x y = of_float (Float.sub (to_float x) (to_float y))

  let[@inline always] mul x y = of_float (Float.mul (to_float x) (to_float y))

  let[@inline always] div x y = of_float (Float.div (to_float x) (to_float y))

  let[@inline always] neg x = of_float (Float.neg (to_float x))


  let[@inline always] abs x = of_float (Float.abs (to_float x))

  let[@inline always] compare x y = (Float.compare (to_float x) (to_float y))

end
