(* TEST
   no-flat-float-array;
   expect;
*)

(* With the flat float array optimization disabled, this [@@unboxed]
   existential is accepted because there is no flat-float-array
   invariant to break. *)

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
type t = K : 'a list M.t -> t [@@unboxed]
val disaster : t array = [|<unknown constructor>; <unknown constructor>|]
|}]
