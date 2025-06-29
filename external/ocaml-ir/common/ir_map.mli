[@@@ocaml.text " This module exposes the types in [Ir_map_intf] "]

module Label : module type of struct
  include Ir_map_intf.Label
end
[@@ocaml.doc
  " Label carries additional information for a mapping between a source file and its IR "]

module Line_col : sig
  type t = int * int [@@ocaml.doc " [t] has the form [line, col] "]

  val equal : t -> t -> bool

  val of_position : Location.Position.t -> t
    [@@ocaml.doc
      " This is a one-way operation, discarding [pos_fname] and information about byte\n\
      \      position "]

  val get_better_interval : t * t -> t * t -> int
    [@@ocaml.doc
      " Given two intervals of [t], return the one that starts later, and if their start\n\
      \      positions are the same, return the one that is shorter. The semantics are \
       the same\n\
      \      as of any [compare] function. "]
end
[@@ocaml.doc
  " Represents information similar to [Position] but is filename-invariant and does not\n\
  \    contain accumulated byte counts "]

module Tree : sig
  type 'a t

  val of_alist : ((Line_col.t * Line_col.t) * 'a) list -> 'a t

  val fold_range
    :  'v t
    -> Line_col.t
    -> f:(interval:Line_col.t * Line_col.t -> data:'v -> 'b -> 'b)
    -> init:'b
    -> 'b
end
[@@ocaml.doc " Interval tree indexed on [t] "]

module Entry : sig
  type t
  [@@ocaml.doc
    " An Entry represents a single mapping between a range in two code chunks, such as in\n\
    \      a source file and one of its intermediate representations. [label] carries\n\
    \      additional information about the mapping and can be used for debugging. "]
  [@@deriving sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"] [@@merlin.hide]

  val create
    :  label:Label.t
    -> source_location:Location.Simple.t
    -> ir_location:Location.Simple.t
    -> t

  val label : t -> Label.t
  val source_location : t -> Location.Simple.t
  val ir_location : t -> Location.Simple.t
  val compare : t -> t -> int
end

type raw = Entry.t list
[@@ocaml.doc
  " [raw] mapping is a list of [entry] values, i.e. conveys mapping between multiple\n\
  \    regions in two code chunks. "]
[@@deriving sexp]

include sig
  [@@@ocaml.warning "-32"]

  val sexp_of_raw : raw -> Sexplib0.Sexp.t
  val raw_of_sexp : Sexplib0.Sexp.t -> raw
end
[@@ocaml.doc "@inline"] [@@merlin.hide]

type t = Entry.t Tree.t
[@@ocaml.doc
  " Similar to [raw], but entries are stored in an interval tree indexed on [Line_col.t]\n\
  \    positions either of the source or its IR. "]

val is_within : Line_col.t -> Location.Simple.t -> bool
  [@@ocaml.doc
    " Returns true if the given Line_col.t position lies within the Location's range "]

val create_source_to_ir : raw -> t
  [@@ocaml.doc
    " Given a [raw] map with entries if the form [label, source_location, ir_location], \
     and\n\
    \    builds an interval tree indexed on [Line_col.t] positions in the source file "]

val create_ir_to_source : raw -> t
  [@@ocaml.doc
    " Given a [raw] map with entries if the form [label, source_location, ir_location], \
     and\n\
    \    builds an interval tree indexed on [Line_col.t] positions in the IR "]

val find_best_intervals : t -> Line_col.t -> raw
  [@@ocaml.doc
    " Of all intervals in [tree] that contain the given [line_col], return the values\n\
    \    corresponding to those intervals that are best according to\n\
    \    [Line_col.get_better_interval]. Multiple values are returned if their keys in the\n\
    \    interval tree are equal [Line_col.t] pairs. Empty list is returned if no suitable\n\
    \    intervals were found. "]
