module Uid = Shape.Uid

module Type_shape : sig
  module Predef : sig
    type t =
      | Array
      | Bytes
      | Char
      | Extension_constructor
      | Float
      | Floatarray
      | Int
      | Int32
      | Int64
      | Lazy_t
      | Nativeint
      | String
      | Unboxed_float
  end

  type t =
    | Ts_constr of (Uid.t * Path.t) * t list
    | Ts_tuple of t list
    | Ts_var of string option
    | Ts_predef of Predef.t * t list
    | Ts_other

  include Identifiable.S with type t := t
end

module Type_decl_shape : sig
  type 'a complex_constructor =
    { name : string;
      args : 'a complex_constructor_argument list
    }

  and 'a complex_constructor_argument =
    { field_name : string option;
      field_value : 'a
    }

  val complex_constructor_map :
    ('a -> 'b) -> 'a complex_constructor -> 'b complex_constructor

  type tds =
    | Tds_variant of
        { simple_constructors : string list;
              (** The string is the name of the constructor. The runtime
                  representation of the constructor at index [i] in this list is
                  [2 * i + 1]. See [dwarf_type.ml] for more details. *)
          complex_constructors : Type_shape.t complex_constructor list
              (** All constructors in this category are represented as blocks.
                  The index [i] in the list indicates the tag at runtime. The
                  length of the constructor argument list [args] determines the
                  size of the block. *)
        }
    | Tds_record of (string * Type_shape.t) list
    | Tds_alias of Type_shape.t
    | Tds_other

  type t =
    { path : Path.t;
      definition : tds;
      type_params : Type_shape.t list
    }

  val print : Format.formatter -> t -> unit

  val replace_tvar : t -> Type_shape.t list -> t
end

(* CR sspies: For now, we bundle the shape together with its layout. In
   subsequent PRs, the layouts will be integrated into the shape type. *)
type binder_shape =
  { type_shape : Type_shape.t;
    type_sort : Jkind_types.Sort.Const.t
  }

val all_type_decls : Type_decl_shape.t Uid.Tbl.t

val all_type_shapes : binder_shape Uid.Tbl.t

(* Passing [Path.t -> Uid.t] instead of [Env.t] to avoid a dependency cycle. *)
val add_to_type_decls :
  Path.t -> Types.type_declaration -> (Path.t -> Uid.t option) -> unit

val add_to_type_shapes :
  Uid.t ->
  Types.type_expr ->
  Jkind_types.Sort.Const.t ->
  (Path.t -> Uid.t option) ->
  unit

val find_in_type_decls : Uid.t -> Type_decl_shape.t option

val type_name : Type_shape.t -> string
