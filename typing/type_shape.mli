module Uid = Shape.Uid
module Layout = Jkind_types.Sort.Const

type base_layout = Jkind_types.Sort.base

module Type_shape : sig
  (* For several builtin types, we provide predefined type shapes with custom
     logic associated with them for emitting the DWARF debugging information. *)
  module Predef : sig
    type simd_vec_split =
      (* 128 bit *)
      | Int8x16
      | Int16x8
      | Int32x4
      | Int64x2
      | Float32x4
      | Float64x2
      (* 256 bit *)
      | Int8x32
      | Int16x16
      | Int32x8
      | Int64x4
      | Float32x8
      | Float64x4
      (* 512 bit *)
      | Int8x64
      | Int16x32
      | Int32x16
      | Int64x8
      | Float32x16
      | Float64x8

    type unboxed =
      | Unboxed_float
      | Unboxed_float32
      | Unboxed_nativeint
      | Unboxed_int64
      | Unboxed_int32
      | Unboxed_simd of simd_vec_split

    type t =
      | Array
      | Bytes
      | Char
      | Extension_constructor
      | Float
      | Float32
      | Floatarray
      | Int
      | Int32
      | Int64
      | Lazy_t
      | Nativeint
      | String
      | Simd of simd_vec_split
      | Exception
      | Unboxed of unboxed

    val to_string : t -> string

    val of_string : string -> t option

    val unboxed_type_to_layout : unboxed -> Jkind_types.Sort.base

    val predef_to_layout : t -> Layout.t
  end

  (* Type shapes are abstract representations of type expressions. We define
     them with a placeholder 'a for the layout inside. This allows one to
     first create shapes without a type by picking [without_layout] for 'a
     and then later substituting in a layout of type [Layout.t]. *)
  type without_layout = Layout_to_be_determined

  type 'a t =
    | Ts_constr of (Uid.t * Path.t * 'a) * without_layout t list
    | Ts_tuple of 'a t list
    | Ts_unboxed_tuple of 'a t list
    | Ts_var of string option * 'a
    | Ts_predef of Predef.t * without_layout t list
    | Ts_arrow of without_layout t * without_layout t
    | Ts_variant of 'a t poly_variant_constructors
    | Ts_other of 'a

  and 'a poly_variant_constructors = 'a poly_variant_constructor list

  and 'a poly_variant_constructor =
    { pv_constr_name : string;
      pv_constr_args : 'a list
    }

  val shape_layout : Layout.t t -> Layout.t

  val shape_with_layout : layout:Layout.t -> without_layout t -> Layout.t t

  val print : Format.formatter -> 'a t -> unit

  val poly_variant_constructors_map :
    ('a -> 'b) -> 'a poly_variant_constructors -> 'b poly_variant_constructors
end

module Type_decl_shape : sig
  (** For type substitution to work as expected, we store the layouts in the
      declaration alongside the shapes instead of directly going for the
      substituted version. *)
  type tds =
    | Tds_variant of
        { simple_constructors : string list;
              (** The string is the name of the constructor. The runtime
                  representation of the constructor at index [i] in this list is
                  [2 * i + 1]. See [dwarf_type.ml] for more details. *)
          complex_constructors :
            (Type_shape.without_layout Type_shape.t * Layout.t)
            complex_constructors
              (** All constructors in this category are represented as blocks.
                  The index [i] in the list indicates the tag at runtime. The
                  length of the constructor argument list [args] determines the
                  size of the block. *)
        }
    | Tds_variant_unboxed of
        { name : string;
          arg_name : string option;
              (** if this is [None], we are looking at a singleton tuple;
                otherwise, it is a singleton record. *)
          arg_shape : Type_shape.without_layout Type_shape.t;
          arg_layout : Layout.t
        }
        (** An unboxed variant corresponds to the [@@unboxed] annotation.
          It must have a single, complex constructor. *)
    | Tds_record of
        { fields :
            (string * Type_shape.without_layout Type_shape.t * Layout.t) list;
          kind : record_kind
        }
    | Tds_alias of Type_shape.without_layout Type_shape.t
    | Tds_other

  and record_kind =
    | Record_unboxed
        (** [Record_unboxed] is the case for single-field records declared with
            [@@unboxed], whose runtime representation is simply its contents
            without any indirection. *)
    | Record_unboxed_product
        (** [Record_unboxed_product] is the truly unboxed record that
             corresponds to [#{ ... }]. *)
    | Record_boxed
    | Record_mixed of mixed_product_shape
    | Record_floats
        (** Basically the same as [Record_mixed], but we don't reorder the
            fields. *)

  and 'a complex_constructors = 'a complex_constructor list

  and 'a complex_constructor =
    { name : string;
      kind : constructor_representation;
      args : 'a complex_constructor_arguments list
    }

  and 'a complex_constructor_arguments =
    { field_name : string option;
      field_value : 'a
    }

  (* Unlike in [types.ml], we use [base_layout] entries here, because we can
     represent flattened floats simply as float64 in the debugger. *)
  and constructor_representation =
    | Constructor_uniform_value
    | Constructor_mixed of mixed_product_shape

  and mixed_product_shape = base_layout array

  type t =
    { path : Path.t;
      definition : tds;
      type_params : Type_shape.without_layout Type_shape.t list
    }

  val print : Format.formatter -> t -> unit

  val replace_tvar : t -> Type_shape.without_layout Type_shape.t list -> t

  val complex_constructor_map :
    ('a -> 'b) -> 'a complex_constructor -> 'b complex_constructor

  val complex_constructors_map :
    ('a -> 'b) -> 'a complex_constructors -> 'b complex_constructors
end

val all_type_decls : Type_decl_shape.t Uid.Tbl.t

val all_type_shapes : Layout.t Type_shape.t Uid.Tbl.t

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

val type_name : 'a. 'a Type_shape.t -> string

val print_table_all_type_decls : Format.formatter -> unit

val print_table_all_type_shapes : Format.formatter -> unit
