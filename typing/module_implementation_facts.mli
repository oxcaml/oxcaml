module Artifact : sig
  type t =
    | Implementation
    | Interface

  val compare : t -> t -> int
end

module Context : sig
  type t =
    | Def of Shape.Uid.t
        (** A root module/signature instance identified directly by its UID *)
    | App of t * t  (** An applicative functor instance, e.g., [F(A)] *)
    | Proj of t * Shape.Uid.t
        (** A module member projected from another context, e.g., [M.N] *)
    | Body of Shape.Uid.t
        (** The interior associated with a named module-type or functor
            declaration *)
    | Site of Compilation_unit.t * Artifact.t * int
        (** An instance with no stable module path, e.g., anonymous module
            instances *)
  (** A path-like identity for a module/signature instance *)

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val print : Format.formatter -> t -> unit
end

module Key : sig
  type t =
    | Named of
        { context : Context.t;
          family_uid : Shape.Uid.t
        }
    | Anon of { key_uid : Shape.Uid.t }
  (** Uniquely identifies a module-type node *)

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val print : Format.formatter -> t -> unit

  val family : t -> Shape.Uid.t option
end

module Node : sig
  type t =
    | Uid of Shape.Uid.t
    | Location of Compilation_unit.t * Location.t
  (** Identifies the implemntation module of a check *)

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
  (** An [implementation] was checked against [expectation] *)

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
  (** A fact that the contexts denote the same module instance *)

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
  (** Incompleteness marker, used to identify that a missing dependency isn't
      proof that no dependency exists *)

  val compare : t -> t -> int
end

type t = private
  { checks : Check.t list;
    dependencies : Dependency.t list;
    equalities : Context_equality.t list;
    omissions : Omission.t list
  }

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
