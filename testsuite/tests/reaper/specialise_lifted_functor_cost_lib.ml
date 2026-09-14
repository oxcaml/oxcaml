type 'a interval = Empty | Interval of 'a * 'a

module Make (Bound : sig
  type t
  val ( < ) : t -> t -> bool
  val ( > ) : t -> t -> bool
end) : sig
  val contains : Bound.t interval -> Bound.t -> bool
  val check_pairs : Bound.t interval -> Bound.t list -> bool
end = struct
  let contains i x =
    match i with
    | Empty -> false
    | Interval (l, u) -> not (Bound.( < ) x l || Bound.( > ) x u)

  let[@inline never] rec loop i = function
    | [] | [_] -> true
    | x :: (y :: _ as xs) -> contains i x && contains i y && loop i xs

  let check_pairs i xs = loop i xs
end
