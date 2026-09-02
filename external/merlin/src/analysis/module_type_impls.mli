val impl_source_of_interface : Mconfig.t -> string -> string option
val own_file : Mconfig.t -> string

(** The modules that must {e implement} each module-type declaration of the
    buffer (or the single declaration enclosing [position]), computed from the
    compiler facts in the configured indexes.

    The contract: a module implements the target when the type it was checked
    against reaches the target purely through requirement-carrying relations
    (ascription, functor arguments, packing, aliases, includes, [with]
    constraints, destructive substitution, strengthening, [module type of],
    and functor-application instances).  A unit whose interface includes the
    target implements it too.  Modules related to the target only
    {e definitionally} are not implementers and must not be returned:
    declaring the target, being interface-paired with the target's own
    declaration, providing it as an equal member, or producing it as a
    functor result do not qualify.  The intended answers are encoded in
    [tests/test-dirs/module-type-impls.t]. *)
val query :
  pipeline:Mpipeline.t ->
  ?position:Lexing.position ->
  Mtyper.typedtree ->
  Query_protocol.Module_type_impls.response
