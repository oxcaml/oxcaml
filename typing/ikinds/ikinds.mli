val ikind_reset : string -> Types.type_ikind

val normalize : context:Jkind.jkind_context -> Types.jkind_l -> Ikind.Ldd.node

val type_declaration_ikind :
  context:Jkind.jkind_context -> path:Path.t -> Types.constructor_ikind

val type_declaration_ikind_gated :
  context:Jkind.jkind_context -> path:Path.t -> Types.type_ikind

val type_declaration_ikind_of_jkind :
  context:Jkind.jkind_context ->
  params:Types.type_expr list ->
  Types.jkind_l ->
  Types.type_ikind

val sub_jkind_l :
  ?allow_any_crossing:bool ->
  ?origin:string ->
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  context:Jkind.jkind_context ->
  level:int ->
  Types.jkind_l ->
  Types.jkind_l ->
  (unit, Jkind.Violation.t) result

val crossing_of_jkind :
  context:Jkind.jkind_context -> ('l * 'r) Types.jkind -> Mode.Crossing.t

type sub_or_intersect = Jkind.sub_or_intersect

val sub_or_intersect :
  ?origin:string ->
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  context:Jkind.jkind_context ->
  level:int ->
  (Allowance.allowed * 'r1) Types.jkind ->
  ('l2 * Allowance.allowed) Types.jkind ->
  sub_or_intersect

val sub_or_error :
  ?origin:string ->
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  context:Jkind.jkind_context ->
  level:int ->
  (Allowance.allowed * 'r1) Types.jkind ->
  ('l2 * Allowance.allowed) Types.jkind ->
  (unit, Jkind.Violation.t) result

(** Apply a path/type-function substitution to a constructor ikind.
    - [lookup p] should describe the substitution for constructor [p]:
      [None] for identity, [`Path q] to rename to [q], or
      [`Type_fun (params, body)] to inline a type function, which is evaluated
      in an identity environment (no Env required).
*)
val substitute_decl_ikind_with_lookup :
  lookup:
    (Path.t ->
    [`Path of Path.t | `Type_fun of Types.type_expr list * Types.type_expr]
    option) ->
  Types.type_ikind ->
  Types.type_ikind
