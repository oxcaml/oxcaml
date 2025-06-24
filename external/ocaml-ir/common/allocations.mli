module Allocation : sig
  type kind =
    | Ccall of string
    | Ocaml

  type context =
    | Raise
    | Direct
    | Catch

  type locality =
    | Global
    | Local

  type alloc = private
    { kind : kind
    ; context : context
    ; locality : locality
    }
  [@@deriving sexp, compare]

  include sig
    [@@@ocaml.warning "-32"]

    val sexp_of_alloc : alloc -> Sexplib0.Sexp.t
    val alloc_of_sexp : Sexplib0.Sexp.t -> alloc
    val compare_alloc : alloc -> (alloc[@merlin.hide]) -> int
  end
  [@@ocaml.doc "@inline"] [@@merlin.hide]

  type t = Location.Inferred.t option * alloc [@@deriving sexp, compare]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t

    val compare : t -> t -> int
  end
  [@@ocaml.doc "@inline"] [@@merlin.hide]

  val create
    :  kind:kind
    -> context:context
    -> locality:locality
    -> Location.Inferred.t option
    -> t

  val here_because_inlined : t -> bool
end

module Item : sig
  type t =
    | Deadcode of Location.Inferred.t
    | Allocation of Allocation.t
  [@@deriving sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"] [@@merlin.hide]

  val loc : t -> Location.Inferred.t option
  val compare : t -> t -> int
  val byte_offset_within_file : t -> (int * int) option
end

type t

val t_of_sexp : Sexplib.Sexp.t -> t
val serialize : [ `V1 | `V2 ] -> t -> Sexplib.Sexp.t
val empty : t
val open_function : function_name:string -> t -> t
val notify_deadcode_eliminated : name:string -> dbg:Location.Inferred.t -> t -> t
val filter_functions : pattern:string -> t -> t
val add : Allocation.t -> t -> t
val to_list : t -> Item.t list
val print_hum : Format.formatter -> t -> unit
val remove_duplicates : t -> t
val exclude_matching : patterns:Location.Pattern.t list -> t -> t
val infer_and_move_allocations_inlined_intra_file : t -> t

module Hide_inlined : sig
  type t =
    | All
    | Same_file
    | External
    | None

  val print : Format.formatter -> t -> unit
  val parse : string -> [> `Error of string | `Ok of t ]
end

val hide_inlined : Hide_inlined.t -> t -> t

module Locality_filter : sig
  type t =
    | Only_local
    | Only_global
    | All

  val print : Format.formatter -> t -> unit
  val parse : string -> [> `Error of string | `Ok of t ]
end

val filter_by_locality : Locality_filter.t -> t -> t
val prune_deadcode : t -> t
