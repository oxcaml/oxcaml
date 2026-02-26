(* TEST
   expect;
*)

module M : sig
  type ('a : value mod non_float) t constraint 'a = 'b list

  val float_t : float list t
  val non_float_t : int list t
end = struct
  type 'a t = 'b constraint 'a = 'b list

  let float_t : float list t = 5.0
  let non_float_t : int list t = 5
end

type t = K : 'a M.t -> t [@@unboxed]

let disaster = [| K M.float_t; K M.non_float_t |]

[%%expect]
