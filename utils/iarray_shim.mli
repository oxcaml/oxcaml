type 'a iarray

val (.:()) : 'a iarray -> int -> 'a

module Iarray : sig
  type 'a t = 'a iarray

  val unsafe_of_array : 'a array -> 'a t
  val of_array : 'a array -> 'a t
  val of_list : 'a list -> 'a t
  val to_array : 'a t -> 'a array
  val to_list : 'a t -> 'a list

  val empty : 'a t

  val make : int -> 'a -> 'a t
  val append : 'a t -> 'a t -> 'a t

  val length : _ t -> int
  val (.:()) : 'a t -> int -> 'a
  val unsafe_get : 'a t -> int -> 'a

  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val iter : ('a -> unit) -> 'a t -> unit
  val iteri : (int -> 'a -> unit) -> 'a t -> unit
  val for_all : ('a -> bool) -> 'a t -> bool
  val exists : ('a -> bool) -> 'a t -> bool
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

  val split : ('a * 'b) t -> 'a t * 'b t
end
