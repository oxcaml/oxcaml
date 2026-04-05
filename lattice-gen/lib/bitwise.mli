type var = string

type t

type binding =
  { name : string;
    expr : t
  }

val var : mask:int -> var -> t

val const : int -> t

val mask : t -> int -> t

val xor_mask : t -> int -> t

val and_ : t list -> t

val or_ : t list -> t

val shift_l : t -> int -> t

val shift_r : t -> int -> t

val mask_of : t -> int

val render : t -> string

val render_pretty : ?width:int -> t -> string

val cse : ?avoid:string list -> t -> binding list * t
