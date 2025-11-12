open Astlib
open Ppxlib_ast.Asttypes
open Ppxlib_ast.Parsetree

module type S = sig
  type 'a with_loc
  type arrow_argument := Shim.arrow_argument
  type arrow_result := Shim.arrow_result
  type block_access := Shim.block_access
  type modalities := Shim.Modalities.t
  type modes := Shim.Modes.t
  type include_kind := Shim.Include_kind.t
  type index_kind = Shim.index_kind
  type jkind_annotation := Shim.jkind_annotation
  type unboxed_access := Shim.unboxed_access

  module Pcstr_tuple_arg := Shim.Pcstr_tuple_arg

  (** {2 Modes} *)

  (** Construct a [Ptyp_arrow] *)
  val ptyp_arrow : (arrow_argument -> arrow_result -> core_type) with_loc

  (** Construct a multi-argument arrow type with the provided arguments and result.

      @raise [Invalid_argument] if the input list is empty. *)
  val tarrow : (arrow_argument list -> arrow_result -> core_type) with_loc

  (** As [tarrow], but will return the result if the input list is empty rather than
      erroring; this means the result type cannot have a mode annotation. *)
  val tarrow_maybe : (arrow_argument list -> core_type -> core_type) with_loc

  (** [Pexp_let] with [let mutable] support *)
  val pexp_let
    : (mutable_flag -> rec_flag -> value_binding list -> expression -> expression)
        with_loc

  (** Construct a [Pexp_constraint] with modes *)
  val pexp_constraint : (expression -> core_type option -> modes -> expression) with_loc

  (** Construct a [Ppat_constraint] with modes *)
  val ppat_constraint : (pattern -> core_type option -> modes -> pattern) with_loc

  (** Contruct a [value_binding] with modes *)
  val value_binding
    : (pat:pattern -> expr:expression -> modes:modes -> value_binding) with_loc

  (** Construct a [Pcstr_tuple], a representation for the contents of a tupled variant
      constructor, that attaches the provided modalities to each field. *)
  val pcstr_tuple : ((modalities * core_type) list -> constructor_arguments) with_loc

  (** Construct a [Psig_include] with modalities *)
  val psig_include
    : (modalities:modalities -> include_description -> signature_item) with_loc

  (** Construct a [signature] *)
  val signature : (?modalities:modalities -> signature_item list -> signature) with_loc

  val pmty_signature : (signature -> module_type) with_loc

  (** Splits a possibly-modality-annotated field of a tupled variant constructor into a
      pair of its modality and the unannotated field. If the resulting mode is [None],
      then the field is returned unchanged. *)
  val get_tuple_field_modalities : Pcstr_tuple_arg.t -> modalities * core_type

  (** Splits a possibly-modality-annotated label declaration into a pair of its modality
      and the unannotated label declaration. If the resulting modality is [None], then the
      label declaration is returned unchanged. *)
  val get_label_declaration_modalities
    :  label_declaration
    -> modalities * label_declaration

  val label_declaration
    : (name:string Location.loc
       -> mutable_:mutable_flag
       -> modalities:modalities
       -> type_:core_type
       -> label_declaration)
        with_loc

  val get_value_description_modalities
    :  value_description
    -> modalities * value_description

  val value_description
    : (name:string Location.loc
       -> type_:core_type
       -> modalities:modalities
       -> prim:string list
       -> value_description)
        with_loc

  val pcstr_tuple_arg
    : (modalities:modalities -> type_:core_type -> Pcstr_tuple_arg.t) with_loc

  val include_infos
    : (?attrs:attributes -> kind:include_kind -> 'a -> 'a include_infos) with_loc

  val module_declaration
    : (?attrs:attributes
       -> ?modalities:modalities
       -> string option Location.loc
       -> module_type
       -> module_declaration)
        with_loc

  val pmty_functor
    : (?attrs:attributes
       -> ?modes:modes
       -> functor_parameter
       -> module_type
       -> module_type)
        with_loc

  val pmod_constraint
    : (?attrs:attributes -> module_expr -> module_type option -> modes -> module_expr)
        with_loc

  (** {2 N-ary functions} *)

  (** Many comments below make reference to the Jane Street compiler's treatment of
      function arity. These comments refer to a parsetree change made to upstream OCaml in
      https://github.com/ocaml/ocaml/pull/12236, but that Jane Street has mirrored
      internally already.

      The treatment of arity can be summarized as follows:
      - In a previous version of OCaml, a function's runtime arity was inferred at a late
        stage of the compiler, after typechecking, where it fuses together nested lambdas.
      - In the new version of OCaml (both upstream OCaml after #12236 and the internal
        Jane Street compiler), a function's runtime arity is purely a syntactic notion:
        it's the number of parameters in a [fun x1 ... xn -> body] construct, with some
        special allowances for function cases.

      Why is arity important? In native code, application sites of a function to [n]
      syntactic arguments will trigger a fast path (where arguments are passed in
      registers) only if the function's runtime arity is [n].

      As a result, ppxes must take more care than before to generate functions of the
      correct arity. Now, a nested function like [fun x -> fun y -> e] has arity 1
      (returning still another function of arity 1) instead of arity 2. All bindings below
      that construct functions are documented as to the arity of the returned function.

      Some examples of arity:
      - 2-ary function: [fun x y -> e]
      - 1-ary function returning 1-ary function: [fun x -> fun y -> e]
      - 3-ary function: [fun x y -> function P1 -> e1 | P2 -> e2]
      - 2-ary function returning 1-ary function:
        [fun x y -> (function P1 -> e1 | P2 -> e2)]
      - 2-ary function returning 1-ary function:
        [fun x -> function P1 -> function P2 -> e]

      Notably, unparenthesized [function] has a special meaning when used as a direct body
      of [fun]: the [function] becomes part of the arity of the outer [fun]. The same does
      not apply for multiple nested [function]s, even if they each have a single case; the
      nested [function]s are treated as unary. (See the last example.) *)

  type function_param = Shim.Pexp_function.function_param
  type function_constraint = Shim.Pexp_function.Function_constraint.t
  type function_body = Shim.Pexp_function.function_body

  module Latest : sig
    (** Avoid shadowing Ppxlib's AST builder. *)

    val ptyp_any : (jkind_annotation option -> core_type) with_loc
    val ptyp_var : (string -> jkind_annotation option -> core_type) with_loc

    val pexp_function
      : (?attrs:attributes
         -> function_param list
         -> function_constraint
         -> function_body
         -> expression)
          with_loc
  end

  (** Create a function with unlabeled parameters and an expression body. Like
      {!Ppxlib.Ast_builder.eapply}, but for constructing functions.

      [coalesce_fun_arity] is relevant for the Jane Street compiler. By default,
      [coalesce_fun_arity] is [true].

      Suppose there is a call [eabstract pats body ~coalesce_fun_arity]
      - If [colaesce_fun_arity] is [true], the arity of the returned function is the same
        as the arity of: [add_fun_params (List.map params ~f:(Fun.param Nolabel)) body]
      - If [coalesce_fun_arity] is [false], then the arity of the returned function is the
        length of [pats].

      In other words, [coalesce_fun_arity = true] allows you to build up the arity of an
      already-constructed function rather than necessarily creating a new function. *)
  val eabstract
    : (?coalesce_fun_arity:bool
       -> ?return_constraint:core_type
       -> pattern list
       -> expression
       -> expression)
        with_loc

  (** [unary_function cases] is [function <cases>]. When used with the Jane Street
      compiler, the function's runtime arity is 1, so the fast path for function
      application happens only when application sites of the resulting function receive 1
      argument. To create a function with multiple argument that pattern-matches on the
      last one, use [add_param] or [add_params] to add more parameters. Alternatively, use
      [pexp_function] to provide all parameters at once.

      The attributes of the resulting expression will be the [attrs] argument together
      with any attributes added by the Jane Street compiler. *)
  val unary_function : (?attrs:attributes -> case list -> expression) with_loc

  (** [fun_param lbl pat] is [Pparam_val (lbl, None, pat)]. This gives a more
      self-documenting way of constructing the usual case: value parameters without
      optional argument defaults. *)
  val fun_param : (arg_label -> pattern -> function_param) with_loc

  (** Say an expression is a "function" if it is a [Pexp_fun] or a [Pexp_function]. All
      functions have parameters and arity.

      Suppose [add_param lbl def pat e ==> e']. Then, letting
      [param = Pparam_val (lbl, def, pat)],
      - If [e] is a function with arity [n], then [e'] is a function with arity [n+1].
        [param] is added at the outermost layer. For example, if
        [e = fun <params> -> <body>], then [e' = fun <param :: params> -> body]. The
        attributes on the resulting expression will be the [attrs] argument together with
        any attributes already present on [e].
      - If [e] is not a function, then [e'] is a function with arity [1], namely:
        [fun <param> -> <e>]. The attributes of the resulting expression will be the
        [attrs] argument together with any attributes added by the Jane Street compiler. *)
  val add_fun_param
    : (?attrs:attributes
       -> ?return_constraint:core_type
       -> arg_label
       -> expression option
       -> pattern
       -> expression
       -> expression)
        with_loc

  (** [add_params params e] is [List.fold_right params ~init:e ~f:add_param]. Note the
      [fold_right]: if [e] is [fun <params'> -> <body>], then [add_params params e] is
      [fun <params @ params'> -> <body>]. *)
  val add_fun_params
    : (?attrs:attributes
       -> ?return_constraint:core_type
       -> function_param list
       -> expression
       -> expression)
        with_loc

  (** This operation is a no-op, except as interpreted by the Jane Street compiler. If [e]
      is a function with arity [n] with an expression body that itself is a function with
      arity [m], then [coalesce_fun_arity e] is a function of arity [n + m].

      You should usually call [coalesce_fun_arity] on metaquot fun expressions whose body
      may be a function, e.g.:

      [coalesce_fun_arity [%expr fun x y -> [%e possibly_function]]] *)
  val coalesce_fun_arity : expression -> expression

  (** {2 Unboxed types} *)

  val ptyp_unboxed_tuple : ((string option * core_type) list -> core_type) with_loc

  val pexp_unboxed_tuple
    : (?attrs:attributes -> (string option * expression) list -> expression) with_loc

  val pexp_record_unboxed_product
    : (?attrs:attributes
       -> (Longident.t loc * expression) list
       -> expression option
       -> expression)
        with_loc

  val ppat_record_unboxed_product
    : (?attrs:attributes -> (Longident.t loc * pattern) list -> closed_flag -> pattern)
        with_loc

  val pexp_unboxed_field
    : (?attrs:attributes -> expression -> Longident.t loc -> expression) with_loc

  val ppat_unboxed_tuple
    : (?attrs:attributes -> (string option * pattern) list -> closed_flag -> pattern)
        with_loc

  val pexp_idx
    : (?attrs:attributes -> block_access -> unboxed_access list -> expression) with_loc

  (** {3 Expression literals} *)

  (** e.g. [#42L] *)
  val eint64_u : (int64 -> expression) with_loc

  (** e.g. [#42l] *)
  val eint32_u : (int32 -> expression) with_loc

  (** e.g. [#42n] *)
  val enativeint_u : (nativeint -> expression) with_loc

  (** e.g. [#42.] *)
  val efloat_u : (string -> expression) with_loc

  (** {3 Pattern literals} *)

  (** e.g. [#42L] *)
  val pint64_u : (int64 -> pattern) with_loc

  (** e.g. [#42l] *)
  val pint32_u : (int32 -> pattern) with_loc

  (** e.g. [#42n] *)
  val pnativeint_u : (nativeint -> pattern) with_loc

  (** e.g. [#42.] *)
  val pfloat_u : (string -> pattern) with_loc

  (** {3 Layouts} *)

  val ptyp_alias
    : (?attrs:attributes
       -> core_type
       -> label loc option
       -> jkind_annotation option
       -> core_type)
        with_loc

  val ptyp_poly
    : (?attrs:attributes
       -> (string loc * jkind_annotation option) list
       -> core_type
       -> core_type)
        with_loc

  val ptyp_quote : (?attrs:attributes -> core_type -> core_type) with_loc
  val ptyp_splice : (?attrs:attributes -> core_type -> core_type) with_loc

  val pexp_newtype
    : (?attrs:attributes
       -> string loc
       -> jkind_annotation option
       -> expression
       -> expression)
        with_loc

  val type_declaration
    : (?attrs:attributes
       -> ?jkind_annotation:jkind_annotation
       -> name:string Location.loc
       -> params:(core_type * (variance * injectivity)) list
       -> cstrs:(core_type * core_type * Location.t) list
       -> kind:type_kind
       -> private_:private_flag
       -> manifest:core_type option
       -> unit
       -> type_declaration)
        with_loc

  (** {2 Labeled tuples} *)

  val ptyp_tuple
    : (?attrs:attributes -> (string option * core_type) list -> core_type) with_loc

  val pexp_tuple
    : (?attrs:attributes -> (string option * expression) list -> expression) with_loc

  val ppat_tuple
    : (?attrs:attributes -> (string option * pattern) list -> closed_flag -> pattern)
        with_loc

  (** {2 Immutable arrays} *)

  val ppat_array : (?attrs:attributes -> mutable_flag -> pattern list -> pattern) with_loc

  val pexp_array
    : (?attrs:attributes -> mutable_flag -> expression list -> expression) with_loc
end

module type S_with_implicit_loc = S with type 'a with_loc := 'a
module type S_with_explicit_loc = S with type 'a with_loc := loc:Location.t -> 'a

module type Ast_builder = sig
  (** Jane Street-internal extensions to {!Ppxlib.Ast_builder}. The bindings below
      ([Default], [Make], etc.) are parallel to bindings exported from
      [Ppxlib.Ast_builder]. *)

  module type S_with_implicit_loc = S_with_implicit_loc

  module Default : S_with_explicit_loc

  module Make (Loc : sig
      val loc : Location.t
    end) : S_with_implicit_loc

  val make : Location.t -> (module S_with_implicit_loc)
end
