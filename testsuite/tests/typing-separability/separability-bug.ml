(* TEST
   expect;
*)

module M : sig
  type 'a t constraint 'a = 'b list

  val float_t : float list t
  val non_float_t : int list t
end = struct
  type 'a t = 'b constraint 'a = 'b list

  let float_t : float list t = 5.0
  let non_float_t : int list t = 5
end

type t = K : 'a M.t -> t [@@unboxed]

let disaster = [| K M.float_t; K M.non_float_t |]

[%%expect{|
module M :
  sig
    type 'a t constraint 'a = 'b list
    val float_t : float list t
    val non_float_t : int list t
  end
Line 13, characters 0-36:
13 | type t = K : 'a M.t -> t [@@unboxed]
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of an unnamed existential variable.
       You should annotate it with "[@@ocaml.boxed]".
|}]
