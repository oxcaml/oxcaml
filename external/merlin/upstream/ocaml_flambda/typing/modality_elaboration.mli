(** First-class modality conversion at expression and pattern boundaries. This
    module neither unifies types nor recursively typechecks syntax. *)

type layer = private
  { wrapper : Types.type_expr;
    payload : Types.type_expr;
    modality : Mode.Modality.Const.t
  }

type plan =
  | Equal
  | Introduce of layer
  | Eliminate of layer

(** Inspect the outer wrapper, optionally checking head principality. *)
val outer_layer : ?loc:Location.t -> Env.t -> Types.type_expr -> layer option

(** Unknown variables retain the whole opposite type. Two wrapped types are
    always compared by equality, with no conversion fallback on failure.
    [Introduce] and [Eliminate] remove exactly one outer layer. *)
val plan :
  loc:Location.t ->
  Env.t ->
  actual:Types.type_expr ->
  expected:Types.type_expr ->
  plan

(** Early introduction for syntax that produces its own structural head.
    Expected payload information remains available for disambiguation.
    Transparent forms and explicit constraints are handled by their ordinary
    recursive Typecore branches, not eagerly peeled here. *)
val introduction :
  loc:Location.t ->
  Env.t ->
  expected:Types.type_expr ->
  Parsetree.expression_desc ->
  layer option

(** Structural value patterns inspect a payload. Variable, wildcard, alias,
    or-pattern and annotation nodes retain the wrapper until their ordinary
    recursion reaches a structural pattern. *)
val pattern_elimination :
  loc:Location.t ->
  Env.t ->
  expected:Types.type_expr ->
  Parsetree.pattern_desc ->
  layer option

(** Synthetic nodes own no source attributes or extras; those remain on the
    child exactly once. Types, locations, environments and pattern alias
    barriers are preserved. Runtime consumers must erase these nodes. *)
val expression :
  ty:Types.type_expr -> Typedtree.expression -> Typedtree.expression

val pattern : ty:Types.type_expr -> Typedtree.pattern -> Typedtree.pattern
