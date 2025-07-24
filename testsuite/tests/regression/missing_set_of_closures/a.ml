module type Ret = sig
  val g : int -> int -> int
end

module F () : Ret = struct
  let n = Sys.opaque_identity 42

  let rec f = fun [@inline never] x -> x + n

  and g = fun [@inline] x -> f
end
[@@inline never]
