module Artifact : sig
  type t =
    | Implementation
    | Interface

  val compare : t -> t -> int
end

module Context : sig
  type t =
    | Def of Shape.Uid.t
    | App of t * t
    | Proj of t * Shape.Uid.t
    | Body of Shape.Uid.t
    | Site of Compilation_unit.t * Artifact.t * int

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val print : Format.formatter -> t -> unit
end

module Key : sig
  type t =
    | Named of Context.t * Shape.Uid.t
    | Anon of Shape.Uid.t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val print : Format.formatter -> t -> unit

  val family : t -> Shape.Uid.t option
end

module Node : sig
  type t =
    | Uid of Shape.Uid.t
    | Location of Compilation_unit.t * Location.t

  val compare : t -> t -> int
end

module Check : sig
  module Kind : sig
    type t =
      | Ascription
      | Argument
      | Package
      | Interface
  end

  type t =
    { implementation : Node.t;
      expectation : Key.t;
      kind : Kind.t;
      site : Location.t
    }

  val compare : t -> t -> int
end

module Dependency : sig
  module Reason : sig
    type t =
      | Definition
      | Alias
      | Include
      | With_constraint
      | Destructive_substitution
      | Module_type_of
      | Strengthening
      | Functor_type
      | Instance
      | Argument_member
      | Interface
  end

  type t =
    { derived : Key.t;
      source : Key.t;
      reason : Reason.t
    }

  val compare : t -> t -> int
end

module Context_equality : sig
  type t =
    { left : Context.t;
      right : Context.t
    }

  val compare : t -> t -> int
end

module Omission : sig
  module Reason : sig
    type t =
      | Unresolved_module_type
      | Unresolved_module
      | Unsupported_path
      | Missing_parameter_expectation
  end

  type t =
    { affected : Key.t option;
      source : Shape.Uid.t option;
      reason : Reason.t
    }

  val compare : t -> t -> int
end

type t =
  { checks : Check.t list;
    dependencies : Dependency.t list;
    equalities : Context_equality.t list;
    omissions : Omission.t list
  }

val empty : t

val normalize : t -> t

val ensure_normalized : t -> t

val merge : t -> t -> t

val merge_many : t list -> t

val compare : t -> t -> int

val of_implementation :
  Compilation_unit.t ->
  module_pairs:(Shape.Uid.t * Shape.Uid.t) list ->
  modtype_pairs:(Shape.Uid.t * Shape.Uid.t) list ->
  unit_interface_check:bool ->
  argument_interface:Shape.Uid.t option ->
  Typedtree.structure ->
  t

val of_interface :
  Compilation_unit.t ->
  argument_interface:Shape.Uid.t option ->
  Typedtree.signature ->
  t
