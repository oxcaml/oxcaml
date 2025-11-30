module Var : sig
  type t

  val create : unit -> t

  module Map : Container_types.Map with type key = t
end

type 'a pattern

module Pattern : sig
  type 'a t = 'a pattern

  val any : 'a t

  val var : Var.t -> 'a -> 'a t

  val untag : 'a t -> 'a t

  type 'a block_field

  val block_field :
    Target_ocaml_int.t -> Flambda_kind.t -> 'a t -> 'a block_field

  val block : ?tag:Tag.t -> 'a block_field list -> 'a t

  type 'a array_field

  val array_field :
    Target_ocaml_int.t -> Flambda_kind.t -> 'a t -> 'a array_field

  val array : 'a array_field list -> 'a t

  type 'a closure_field

  val rec_info : Function_slot.t -> 'a t -> 'a closure_field

  val value_slot : Value_slot.t -> 'a t -> 'a closure_field

  val function_slot : Function_slot.t -> 'a t -> 'a closure_field

  val closure : 'a closure_field list -> 'a t
end

type 'a expr

module Expr : sig
  type 'a t = 'a expr

  module Function_type : sig
    type 'a t

    val create : Code_id.t -> rec_info:'a -> 'a t
  end

  val var : 'a -> 'a t

  val unknown : Flambda_kind.t -> 'a t

  val bottom : Flambda_kind.t -> 'a t

  val unknown_with_subkind : Flambda_kind.With_subkind.t -> 'a t

  val tag_immediate : 'a t -> 'a t

  val immutable_block :
    is_unique:bool ->
    Tag.t ->
    shape:Flambda_kind.Block_shape.t ->
    Alloc_mode.For_types.t ->
    fields:'a t list ->
    'a t

  val exactly_this_closure :
    Function_slot.t ->
    all_function_slots_in_set:
      'a t Function_type.t Or_unknown.t Function_slot.Map.t ->
    all_closure_types_in_set:'a t Function_slot.Map.t ->
    all_value_slots_in_set:'a t Value_slot.Map.t ->
    Alloc_mode.For_types.t ->
    'a t

  val at_least_this_closure :
    Function_slot.t ->
    at_least_these_function_slots:
      'a t Function_type.t Or_unknown.t Function_slot.Map.t ->
    at_least_these_closure_types:'a t Function_slot.Map.t ->
    at_least_these_value_slots:'a t Value_slot.Map.t ->
    Alloc_mode.For_types.t ->
    'a t
end

type 'a rewrite

module Rule : sig
  type 'a t = 'a rewrite

  val identity : 'a t

  val rewrite : 'a Pattern.t -> Var.t expr -> 'a t
end

module Make (X : sig
  type t

  val print : Format.formatter -> t -> unit

  module Map : Container_types.Map with type key = t

  val rewrite : t -> Typing_env.t -> Type_grammar.t -> t rewrite

  val block_slot :
    ?tag:Tag.t -> t -> Target_ocaml_int.t -> Typing_env.t -> Type_grammar.t -> t

  val array_slot :
    t -> Target_ocaml_int.t -> Typing_env.t -> Type_grammar.t -> t

  type set_of_closures

  val set_of_closures :
    t ->
    Function_slot.t ->
    Typing_env.t ->
    Type_grammar.closures_entry ->
    set_of_closures

  val rec_info :
    Typing_env.t ->
    set_of_closures ->
    Function_slot.t ->
    Code_id.t ->
    Type_grammar.t ->
    t

  val value_slot :
    set_of_closures -> Value_slot.t -> Typing_env.t -> Type_grammar.t -> t

  val function_slot :
    set_of_closures -> Function_slot.t -> Typing_env.t -> Type_grammar.t -> t
end) : sig
  (** Rewrite the provided typing env.

        The provided name map contains all the names that must be preserved,
        along with their abstraction. It must only contain symbols and variables
        with [Name_mode.normal] that exist in the provided typing env. *)
  val rewrite :
    Typing_env.t ->
    (Symbol.t -> X.t) ->
    (X.t * Flambda_kind.t) Variable.Map.t ->
    Typing_env.t

  val rewrite_env_extension_with_extra_variables :
    Typing_env.t ->
    ((string * X.t) pattern * Flambda_kind.t) Variable.Map.t ->
    Typing_env_extension.With_extra_variables.t ->
    Var.t list ->
    Variable.t Var.Map.t * Typing_env_extension.With_extra_variables.t
end
