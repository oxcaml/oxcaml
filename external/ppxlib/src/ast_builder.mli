(** [Ast_builder] is a module to generate OCaml AST fragments. It provides a
    shorter syntax than directly using the {!Parsetree} constructors, as well as
    a better stability than the constructors. *)

open! Import

(** {1 Link to the tutorial}

    For a detailed explanation on this module, refer to the
    {{!"generating-code".ast_builder} relevant} part of the manual.

    {1 API} *)

(** Helpers taking a [~loc] argument. This module is meant to be opened or
    aliased. *)
module Default : sig
  module Located :
    Ast_builder_intf.Located
      with type 'a with_loc := 'a Ast_builder_intf.with_location

  include module type of Ast_builder_generated.M

  val pexp_apply : loc:location -> expression -> (arg_label * expression) list -> expression

  module Latest : sig
    (** This module contains updated versions of node constructors that were
        kept stable when the node changed. For every function in this module,
        there's an equally-named function outside this module. The function
        outside this module will stay stable, whereas the function inside this
        module will adapt potential upcoming new compiler features. Only use a
        function in this module, if the equally-named one outside this module is
        missing a feature you need. *)

    val ppat_construct :
      loc:location ->
      longident loc ->
      ((label loc * jkind_annotation option) list * pattern) option ->
      pattern

    val constructor_declaration :
      loc:location ->
      name:label loc ->
      vars:label loc list ->
      args:constructor_arguments ->
      res:core_type option ->
      unit ->
      constructor_declaration

    val pmty_signature : loc:location -> signature -> module_type

    val signature : loc:location -> ?modalities:modalities -> signature_item list -> signature

    val label_declaration :
      loc:location -> name:string loc ->
      mutable_:mutable_flag ->
      modalities:modalities ->
      type_:core_type ->
      label_declaration
  end

  val ppat_construct :
    loc:location -> longident loc -> pattern option -> pattern

  val constructor_declaration :
    loc:location ->
    name:label loc ->
    args:constructor_arguments ->
    res:core_type option ->
    constructor_declaration

  include
    Ast_builder_intf.Additional_helpers
      with type 'a with_loc := 'a Ast_builder_intf.with_location
end

module type Loc = Ast_builder_intf.Loc

module type S = sig
  include Ast_builder_intf.S

  module Latest : sig
    (** This module contains updated versions of node constructors that were
        kept stable when the node changed. For every function in this module,
        there's an equally-named function outside this module. The function
        outside this module will stay stable, whereas the function inside this
        module will adapt potential upcoming new compiler features. Only use a
        function in this module, if the equally-named one outside this module is
        missing a feature you need. *)

    val ppat_construct :
      longident loc -> ((label loc * jkind_annotation option) list * pattern) option -> pattern

    val constructor_declaration :
      name:label loc ->
      vars:label loc list ->
      args:constructor_arguments ->
      res:core_type option ->
      unit ->
      constructor_declaration

    val pmty_signature : signature -> module_type
    val signature : ?modalities:modalities -> signature_item list -> signature

    val label_declaration :
      name:string loc ->
      mutable_:mutable_flag ->
      modalities:modalities ->
      type_:core_type ->
      label_declaration
  end

  val ppat_construct : longident loc -> pattern option -> pattern

  val constructor_declaration :
    name:label loc ->
    args:constructor_arguments ->
    res:core_type option ->
    constructor_declaration
end

(** Build Ast helpers with the location argument factorized. *)
module Make (Loc : Loc) : S

val make : Location.t -> (module S)
(** Functional version of [Make]. *)
