(** Components classifying the behavior of a region of code. *)
module Component : sig
  type 'top t =
    | Top of 'top  (** This region of code may be reached and may allocate. *)
    | Safe  (** This region of code may be reached but will not allocate. *)
    | Bot  (** This region of code will not be reached. *)

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
    | NormalReturn
    | Exception
    | Diverge

  type tag := t (* for scoping inside [Nonempty_set] *)

  val all : t list

  val compare : t -> t -> int

  val print : Format.formatter -> t -> unit

  module Nonempty_set : sig
    type t

    val compare : t -> t -> int

    val contains : tag -> t -> bool

    val intersection : t -> t -> t option

    val subset : t -> t -> bool

    val union : t -> t -> t

    val singleton : tag -> t
  end
end

(** One value per component, all of the same type. *)
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

  val get : Tag.t -> 'a t -> 'a

  val create : (Tag.t -> 'a) -> 'a t

  val iter : f:(Tag.t -> 'a -> unit) -> 'a t -> unit

  val map : f:(Tag.t -> 'a -> 'b) -> 'a t -> 'b t

  val map2 : f:(Tag.t -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val lessequal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

  val of_nonempty_set : Tag.Nonempty_set.t -> bool t

  val to_nonempty_set : bool t -> Tag.Nonempty_set.t option

  val print :
    (Format.formatter -> 'component -> unit) ->
    Format.formatter ->
    'component t ->
    unit
end

module Relevant_components_by_argument_index : sig
  (** Answers "How does this callback's behavior affect the overall function?"
      Notably, this can be done without knowing the behavior of the callback:
      this just answers which components would be relevant if they were to
      allocate, not whether they actually do.

      Concretely, this is a map from function argument indices to a yes/no
      answer for each component of that argument. Notably, these indices are the
      arguments to the function whose body we're in, not arguments passed to the
      callback. For example, with [let f g = g ()], argument 0 is [g], not [()].

      Keep in mind that each function definition holds up to *three* analyses of
      this type: one for each of its components (normal, exception, diverging).
      So, for [let f g = g ()] above, we have three separate analyses for [g]:
      - On [f]'s normal-return path, [g]'s behavior on *its* normal path
        matters, and its behavior on exceptional or diverging paths does not,
        since [g]'s exceptions are not caught in [f], so [g] cannot raise and
        then have [f] return normally.
      - On [f]'s exception path, [g]'s exception behavior is all that matters.
      - On [f]'s diverging path, [g]'s diverging behavior is all that matters.

      On the other hand, if we had [let f g = try g () with _ -> ()], then [g]'s
      exceptional behavior would matter for [f]'s *normal* path. *)
  type t = Tag.Nonempty_set.t Numbers.Int.Map.t

  val compare : t -> t -> int

  val join : t -> t -> t

  val meet : t -> t -> t

  val lessequal : t -> t -> bool

  val print : Format.formatter -> t -> unit
end

module Allocation_source : sig
  type 'args t =
    | Allocation_other_than_calling_arguments
    | No_allocation_other_than_calling_arguments of 'args

  val map : f:('a -> 'b) -> 'a t -> 'b t

  val compare :
    compare_args:('args -> 'args -> int) -> 'args t -> 'args t -> int

  val lessequal :
    lessequal_args:('args -> 'args -> bool) -> 'args t -> 'args t -> bool

  val join :
    join_args:('args -> 'args -> 'args) -> 'args t -> 'args t -> 'args t

  val meet :
    meet_args:('args -> 'args -> 'args) -> 'args t -> 'args t -> 'args t

  val allocates_other_than_calling_arguments : 'args t -> bool

  val top : 'args t

  val print :
    (Format.formatter -> 'args -> unit) -> Format.formatter -> 'args t -> unit
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

module type Allocation_source = sig
  type t

  val top : t

  val argument : argument_index:int -> tag:Tag.t -> t
end

module type Component = sig
  (** Abstract value for each component of the domain. *)
  type t

  type allocation_source

  type witnesses

  (** Property may not hold on some paths. *)
  val top : allocation_source:allocation_source -> witnesses:witnesses -> t

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
      with type allocation_source := unit
       and type witnesses := Witnesses.t
       and type t := t
end

module Make_value
    (Allocation_source : Allocation_source)
    (Witnesses : WS)
    (V :
      Component
        with type allocation_source := Allocation_source.t
         and type witnesses := Witnesses.t) : sig
  (** Abstract value associated with each program location in a function. *)
  type t = V.t Components.t

  val lessequal : t -> t -> bool

  val join : t -> t -> t

  val meet : t -> t -> t

  val top : allocation_source:Allocation_source.t -> witnesses:Witnesses.t -> t

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

module No_allocation_source : Allocation_source with type t = unit

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
      module type of Make_value (No_allocation_source) (Witnesses) (V)

  val get_value : t -> Value.t option
end
