module Artifact : sig
  type t =
    | Implementation
    | Interface

  val compare : t -> t -> int
end

module Context : sig
  module Site_id : sig
    type t

    val print : Format.formatter -> t -> unit
  end

  (** A path-like identity for a module/signature instance *)
  type t =
    | Def of Shape.Uid.t
        (** The module or signature instance introduced directly by the
            declaration. For a functor, this is the functor value itself; [Body]
            identifies the interior of its result. *)
    | App of t * t  (** An applicative functor instance, e.g., [F(A)] *)
    | Proj of t * Shape.Uid.t
        (** A module member projected from another context, e.g., [M.N] *)
    | Body of Shape.Uid.t
        (** The interior of a named module-type or functor declaration. Members
            of a named module type or functor result are projected from this
            context, rather than from the declaration instance [Def]. *)
    | Site of Compilation_unit.t * Artifact.t * Site_id.t
        (** An instance with no stable module path, e.g., anonymous module
            instances *)

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val print : Format.formatter -> t -> unit
end

(* [Key]s identify specific instances of module-type nodes. Consider:

   {[module type S = sig
       module type T
     end

     module F (_ : sig end) : S = struct
       module type T = sig end
     end

     module A = struct end
     module B = struct end

     module FA = F(A)
     module FB = F(B)]}

   In this example [FA.T] and [FB.T] are distinct module type
   instances, but they both are checked as [T], which is their family. *)
module Key : sig
  (** Uniquely identifies a module-type node *)
  type t =
    | Named of
        { context : Context.t;
              (** The location of this specific module-type occurrence *)
          family_uid : Shape.Uid.t  (** The original module-type declaration *)
        }
    | Anon of Shape.Uid.t
        (** [Anon uid] represents an anonymous module with uid [uid]. *)

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val print : Format.formatter -> t -> unit

  val family : t -> Shape.Uid.t option
end

module Node : sig
  (** Identifies the implementation module of a check *)
  type t =
    | Uid of Shape.Uid.t
    | Whole_unit of Compilation_unit.t
    | Location of Compilation_unit.t * Location.t

  val compare : t -> t -> int
end

module Check : sig
  module Kind : sig
    type t =
      | Annotation
      | Argument
      | Package
      | Interface
  end

  (** An [implementation] was checked against [expectation] *)
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
      | Interface_member
      | Interface_pair
  end

  type t =
    { derived : Key.t;
      source : Key.t;
      reason : Reason.t
    }

  val compare : t -> t -> int
end

module Context_equality : sig
  (** A fact that the contexts denote the same module instance *)
  type t

  (** [create left right] orders distinct contexts canonically. *)
  val create : Context.t -> Context.t -> t option

  val left : t -> Context.t

  val right : t -> Context.t

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

  (** Incompleteness marker, used to identify that a missing dependency isn't
      proof that no dependency exists *)
  type t =
    { affected : Key.t option;
      source : Shape.Uid.t option;
      reason : Reason.t
    }

  val compare : t -> t -> int
end

module Check_set : Set.S with type elt = Check.t

module Dependency_set : Set.S with type elt = Dependency.t

module Context_equality_set : Set.S with type elt = Context_equality.t

module Omission_set : Set.S with type elt = Omission.t

type t = private
  { checks : Check_set.t;
    dependencies : Dependency_set.t;
    equalities : Context_equality_set.t;
    omissions : Omission_set.t
  }

val map_checks : t -> f:(Check.t -> Check.t) -> t

val union : t -> t -> t

(** [of_implementation compilation_unit ~module_pairs ~modtype_pairs
     ~unit_interface_check ~argument_interface structure] extracts facts from
    [structure]. [module_pairs] and [modtype_pairs] associate implementation
    declaration UIDs with the corresponding interface declaration UIDs.
    [unit_interface_check] says whether the compilation unit was checked against
    an explicit interface. [argument_interface] identifies the parameter module
    whose interface this unit was additionally checked against, when compiling
    with [-as-argument-for]. *)
val of_implementation :
  Compilation_unit.t ->
  module_pairs:(impl:Shape.Uid.t * intf:Shape.Uid.t) list ->
  modtype_pairs:(impl:Shape.Uid.t * intf:Shape.Uid.t) list ->
  unit_interface_check:bool ->
  argument_interface:Shape.Uid.t option ->
  Typedtree.structure ->
  t

val of_interface :
  Compilation_unit.t ->
  argument_interface:Shape.Uid.t option ->
  Typedtree.signature ->
  t

(** [of_pack compilation_unit ~module_pairs ~unit_interface_check] builds the
    facts of a unit assembled with [-pack], which has no typedtree of its own:
    the interface check of the pack against its [.mli] when
    [unit_interface_check] holds, and one interface check per packed member
    paired with the [.mli] declaration it was checked against. *)
val of_pack :
  Compilation_unit.t ->
  module_pairs:(impl:Shape.Uid.t * intf:Shape.Uid.t) list ->
  unit_interface_check:bool ->
  t
