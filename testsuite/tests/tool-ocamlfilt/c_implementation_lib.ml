(* Library unit for c_implementation.ml. Its [@inline always] functions
   return [@inline never] closures, so that when [C_implementation_input]
   is compiled with -O3 the closures are copied into that unit and get
   structured symbols with an inline marker ([I] followed by this unit),
   and its named functions are partially applied from there, giving
   partial-application items whose callee path names this unit. *)

let[@inline always] make_adder x = fun[@inline never] y -> x + y

let[@inline always] make_scaler k =
  fun[@inline never] xs -> List.map (fun[@cold] x -> k * x) xs

let[@inline never] add a b = a + b

let[@inline never] labelled ~first ~second = first - second

module Pair = struct
  let[@inline always] both f (a, b) = (f a, f b)

  let[@inline never] swap (a, b) = (b, a)
end

module type Ordered = sig
  type t

  val compare : t -> t -> int
end

module Max (O : Ordered) = struct
  let[@inline never] max a b = if O.compare a b >= 0 then a else b
end

module Int_max = Max (struct
  type t = int

  let compare = Int.compare
end)
