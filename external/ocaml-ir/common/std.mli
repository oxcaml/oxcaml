include module type of struct include StdLabels end
module Make_map_sexp :
functor (Map : Map.S) ->
  sig
    val map_of_sexp :
      (Sexplib.Sexp.t -> Map.key) ->
        (Sexplib.Sexp.t -> 'data) -> Sexplib.Sexp.t -> 'data Map.t
    val sexp_of_map :
      (Map.key -> Sexplib.Sexp.t) ->
        ('data -> Sexplib.Sexp.t) -> 'data Map.t -> Sexplib.Sexp.t
  end
module Error :
sig
  type t[@@deriving sexp]
  include
    sig
      [@@@ocaml.warning "-32"]
      include Sexplib0.Sexpable.S with type  t :=  t
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  val raise : t -> 'a
  val of_exn : exn -> t
  val to_string_hum : t -> string
  val of_string : string -> t
  val createf : ('a, unit, t) format -> 'a
end
module Monad :
sig
  module type S  = Std_intf.Monad.S
  module Make :
  functor (Arg : Std_intf.Monad.Arg) ->
    Std_intf.Monad.S with type 'a  t :=  'a Arg.t
end
module Option :
sig
  include module type of struct include Option end
  include Std_intf.Monad.S with type 'a  t =  'a t
  val value_exn : ?here:Lexing.position -> ?message:string -> 'a option -> 'a
end
val compare_option : ('a -> 'a -> int) -> 'a option -> 'a option -> int
module Int :
sig
  include module type of struct include Int end
  module Map :
  sig
    include Map.S with type  key =  t
    val of_alist_multi : (key * 'a) list -> 'a list t
    val to_alist : 'a t -> (key * 'a) list
    val t_of_sexp : (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a t
    val sexp_of_t : ('a -> Sexplib.Sexp.t) -> 'a t -> Sexplib.Sexp.t
  end
end
module String :
sig
  include module type of struct include String end
  module Map :
  sig
    include Map.S with type  key =  t
    val t_of_sexp : (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a t
    val sexp_of_t : ('a -> Sexplib.Sexp.t) -> 'a t -> Sexplib.Sexp.t
  end
  val split_lines : string -> string list
  val is_substring : substring:string -> string -> bool
  val split_at_indices : string -> indices:int list -> string list
  val starts_with : prefix:string -> string -> bool
end
module List :
sig
  include module type of struct include List end
  val remove_consecutive_duplicates :
    'a t -> equal:('a -> 'a -> bool) -> 'a t
  val filter_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b t
  val intersperse : 'a t -> sep:'a -> 'a t
end
module Array :
sig
  include module type of struct include Array end
  module Binary_search :
  sig
    val last_less_than_or_equal_to :
      'a array -> compare:('b -> 'a -> int) -> key:'b -> int Option.t
  end
end
