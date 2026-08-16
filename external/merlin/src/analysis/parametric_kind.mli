(** Restrict a type declaration's kind to its own type parameters.

    The declaration's ikind is a formula over variables standing for named
    types. Substituting top for every variable settles the formula into
    constants (a sound weakening), which rebuild directly as a jkind:
    mod-bounds from the base, one with-bound per type parameter that still
    contributes. *)

(** The strongest kind of [decl] expressible using only its type parameters,
    rendered from the declaration's ikind, or [None] when no ikind is
    available or the declaration's layout cannot be determined. *)
val restrict_to_parameters :
  env:Env.t ->
  decl:Types.type_declaration ->
  Types.type_ikind ->
  Types.jkind_l option
