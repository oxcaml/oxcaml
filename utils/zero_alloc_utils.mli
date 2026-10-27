module Argument : sig
  type t =
    | Not_a_callback
    | Callback of
        { arity : int;
          partial : bool
        }

  val equal : t -> t -> bool
end

module Arguments : sig
  type t =
    | Unconditional of { arity : int }
    | Conditional of { arguments : Argument.t list }

  val arity : t -> int

  val equal : t -> t -> bool
end

(** Bot/Safe/Top, meaning "impossible," "won't allocate," and "might allocate,"
    respectively. *)
module Component : sig
  type 'top t =
    | Top of 'top
    | Safe
    | Bot

  val map : f:('a -> 'b) -> 'a t -> 'b t

  val print :
    print_top:(Format.formatter -> 'top -> unit) ->
    Format.formatter ->
    'top t ->
    unit

  val compare : compare_top:('top -> 'top -> int) -> 'top t -> 'top t -> int

  val join : join_top:('top -> 'top -> 'top) -> 'top t -> 'top t -> 'top t

  val meet : meet_top:('top -> 'top -> 'top) -> 'top t -> 'top t -> 'top t

  val lessequal :
    lessequal_top:('top -> 'top -> bool) -> 'top t -> 'top t -> bool
end

module Tag : sig
  type t =
    | N
    | E
    | D

  val compare : t -> t -> int

  val print : t -> string
end

(** Three fields, [nor], [exn], & [div], meaning "control flow that returns
    normally," "control flow that raises," and "control flow that diverges,"
    respectively. *)
module Components : sig
  type 'component t =
    { nor : 'component;
          (** Property about all paths from this program location that may reach
              a Normal Return *)
      exn : 'component;
          (** Property about all paths from this program point that may reach a
              Return with Exception *)
      div : 'component
          (** Property about all paths from this program point that may diverge.
          *)
    }

  val create : (Tag.t -> 'a) -> 'a t

  val iter : f:(Tag.t -> 'a -> unit) -> 'a t -> unit

  val map : f:(Tag.t -> 'a -> 'b) -> 'a t -> 'b t

  val map2 : f:(Tag.t -> 'a -> 'a -> 'b) -> 'a t -> 'a t -> 'b t

  val lessequal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

  val one_hot : Tag.t -> bool t

  val print :
    (Format.formatter -> 'component -> unit) ->
    Format.formatter ->
    'component t ->
    unit
end

module Relevant_components_by_argument_index : sig
  type t = bool Components.t Numbers.Int.Map.t

  val compare : t -> t -> int

  val join : t -> t -> t

  val meet : t -> t -> t

  val lessequal : t -> t -> bool

  val print : Format.formatter -> t -> unit
end

module Allocation_sources : sig
  type 'args_only t =
    | Allocation_other_than_calling_arguments
    | No_allocation_other_than_calling_arguments of 'args_only

  val map : f:('a -> 'b) -> 'a t -> 'b t

  val bind : f:('a -> 'b t) -> 'a t -> 'b t

  val compare :
    compare_args_only:('args_only -> 'args_only -> int) ->
    'args_only t ->
    'args_only t ->
    int

  val lessequal :
    lessequal_args_only:('args_only -> 'args_only -> bool) ->
    'args_only t ->
    'args_only t ->
    bool

  val join :
    join_args_only:('args_only -> 'args_only -> 'args_only) ->
    'args_only t ->
    'args_only t ->
    'args_only t

  val meet :
    meet_args_only:('args_only -> 'args_only -> 'args_only) ->
    'args_only t ->
    'args_only t ->
    'args_only t

  val other_than_calling_arguments : 'args_only t -> bool

  val top : 'args_only t

  val print :
    (Format.formatter -> 'args_only -> unit) ->
    Format.formatter ->
    'args_only t ->
    unit
end

(** Abstract domain used in static analysis for checking [@zero_alloc]
    annotations. See [backend/zero_alloc_checker] for details of the analysis.
    See this module's .ml file for details about the translation of
    user-provided annotations to abstract values in this domain. *)
module type WS = sig
  type t

  val empty : t

  val join : t -> t -> t

  val meet : t -> t -> t

  val lessequal : t -> t -> bool

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int
end

module type Allocation_sources = sig
  type t

  val top : t

  val argument : argument_index:int -> tag:Tag.t -> t
end

module type Component = sig
  (** Abstract value for each component of the domain. *)
  type t

  type allocation_sources

  type witnesses

  (** Property may not hold on some paths. *)
  val top : allocation_sources:allocation_sources -> witnesses:witnesses -> t

  (** Property holds on all paths. *)
  val safe : t

  (** Not reachable. *)
  val bot : t

  (** Order of the abstract domain *)
  val lessequal : t -> t -> bool

  val join : t -> t -> t

  val meet : t -> t -> t

  (** Use [compare] for structural comparison of terms, for example to store
      them in a set. Use [lessequal] for checking fixed point of the abstract
      domain. *)
  val compare : t -> t -> int

  val print : witnesses:bool -> Format.formatter -> t -> unit
end

module Make_component (Witnesses : WS) : sig
  type t = Witnesses.t Component.t

  include
    Component
      with type allocation_sources := unit
       and type witnesses := Witnesses.t
       and type t := t
end

module Make_value
    (Allocation_sources : Allocation_sources)
    (Witnesses : WS)
    (V :
      Component
        with type allocation_sources := Allocation_sources.t
         and type witnesses := Witnesses.t) : sig
  (** Abstract value associated with each program location in a function. *)
  type t = V.t Components.t

  val lessequal : t -> t -> bool

  val join : t -> t -> t

  val meet : t -> t -> t

  val top :
    allocation_sources:Allocation_sources.t -> witnesses:Witnesses.t -> t

  val bot : t

  (** [normal_return] means property holds on paths to normal return,
      exceptional return is not reachable and execution will not diverge. *)
  val normal_return : t

  (** [exn_escape] means the property holds on paths to exceptional return,
      normal return is not reachable and execution will not diverge. *)
  val exn_escape : t

  (** [diverges] means the execution may diverge without violating the property,
      but normal and exceptional return are not reachable (i.e., [div] is Safe,
      `nor` and `exn` are Bot). *)
  val diverges : t

  (** [safe] means the property holds on all paths (i.e., all three components
      are set to Safe). *)
  val safe : t

  (** [relaxed] means the property holds on paths that lead to normal returns
      only (i.e., [nor] component is Safe, others are Top. *)
  val relaxed : witnesses:Witnesses.t -> t

  (** Constructs a value from a user annotation. The witness will be empty. *)
  val of_annotation :
    strict:bool -> never_returns_normally:bool -> never_raises:bool -> t

  val print : witnesses:bool -> Format.formatter -> t -> unit

  (** Use [compare] for structural comparison of terms, for example to store
      them in a set. Use [lessequal] for checking fixed point of the abstract
      domain.

      [compare] must be consistent with [lessthan] order of the abstract domain,
      i.e., if [compare t1 t2 <= 0] then [lessequal v1 v2 = true].

      [compare] may distinguish terms that are equivalent w.r.t. the
      abstraction, i.e., [lessequal v1 v2 = true] does not always imply
      [compare t1 t2 <= 0]. In particular, [compare] distinguishes between two
      "Top" values with different witnesses. *)
  val compare : t -> t -> int
end

module No_allocation_sources : Allocation_sources with type t = unit

(** The [Assume_info] module contains an instantiation of the abstract domain
    with trivial witnesses. It is used to propagate assume annotations from the
    front-end to the backend. The backend contains a different instantiation of
    the abstract domain where the witnesses contain actual information about
    allocations - see [Checkmach]. *)
module Assume_info : sig
  type t

  val none : t

  val create :
    strict:bool ->
    never_returns_normally:bool ->
    never_raises:bool ->
    inferred:bool ->
    t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val join : t -> t -> t

  val meet : t -> t -> t

  val to_string : t -> string

  val print : Format.formatter -> t -> unit

  val is_none : t -> bool

  val is_inferred : t -> bool

  module Witnesses : sig
    type t = unit

    val empty : t

    val join : t -> t -> t

    val lessequal : t -> t -> bool

    val meet : t -> t -> t

    val print : Format.formatter -> t -> unit

    val compare : t -> t -> int
  end

  module V : module type of Make_component (Witnesses)

  module Value :
      module type of Make_value (No_allocation_sources) (Witnesses) (V)

  val get_value : t -> Value.t option
end
