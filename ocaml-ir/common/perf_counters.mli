open! Std
module Event_type :
sig
  type t[@@deriving sexp]
  include
    sig
      [@@@ocaml.warning "-32"]
      include Sexplib0.Sexpable.S with type  t :=  t
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  module Map :
  sig
    include Map.S with type  key =  t
    val t_of_sexp : (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a t
    val sexp_of_t : ('a -> Sexplib.Sexp.t) -> 'a t -> Sexplib.Sexp.t
  end
  module Set : Set.S with type  elt =  t
  val compare : t -> t -> int
  val of_string : string -> t
  val to_string : t -> string
end
module Event :
sig
  type t[@@deriving sexp]
  include
    sig
      [@@@ocaml.warning "-32"]
      include Sexplib0.Sexpable.S with type  t :=  t
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  val line : t -> int
  val value : t -> int
  val create : line:int -> col:int -> discriminator:int -> int -> t
end
module Event_types_in_project :
sig
  type t[@@deriving sexp]
  include
    sig
      [@@@ocaml.warning "-32"]
      include Sexplib0.Sexpable.S with type  t :=  t
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  val to_list : t -> Event_type.t list
  val of_list : Event_type.t list -> t
end
module Events_for_a_file :
sig
  type t = private
    {
    values: Event.t list ;
    filename: string ;
    event: Event_type.t ;
    total_in_file: int ;
    total_in_project: int }[@@deriving sexp]
  include
    sig
      [@@@ocaml.warning "-32"]
      include Sexplib0.Sexpable.S with type  t :=  t
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  val print : Format.formatter -> t -> unit
  val create :
    values:Event.t list ->
      filename:string ->
        event:Event_type.t -> total_in_file:int -> total_in_project:int -> t
end
module Event_count_per_files :
sig
  type t[@@deriving sexp]
  include
    sig
      [@@@ocaml.warning "-32"]
      include Sexplib0.Sexpable.S with type  t :=  t
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  val to_list : t -> (string * int) list
  val of_list : (string * int) list -> t
end
