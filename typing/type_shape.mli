module Uid = Shape.Uid
module Layout = Jkind_types.Sort.Const

type base_layout = Jkind_types.Sort.base

(* CR sspies: To prepare for moving the definitions of this file into [shapes.ml]
   the declarations for type shapes and type declaration shapes have been renamed
   in from [t] to [ts] (for type shape) and [tds] (for type declaration shape).*)

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

    val unboxed_type_to_layout : unboxed -> Jkind_types.Sort.base

    val to_layout : t -> Layout.t
  end

  (* Type shapes are abstract representations of type expressions. We define
     them with a placeholder 'a for the layout inside. This allows one to
     first create shapes without a type by picking [without_layout] for 'a
     and then later substituting in a layout of type [Layout.t]. *)
  (* CR sspies: The layout merged into the type shapes is confusing and will
     cause trouble when the type shapes are integrated into shapes. Move them
     back out again in a subsequent PR and instead recurse over layout and shape
     in the DWARF emission code. *)
  type without_layout = Layout_to_be_determined

  type 'a ts =
    | Ts_constr of (Uid.t * Path.t * 'a) * without_layout ts list
        (** [Ts_constr ((uid, p, ly), args)] is the shape of the type constructor
        [p] applied to the arguments [args], resulting in data of layout [ly].
        Unlike for the resulting data, we do not definitely know the layout
        of the arguments yet. This is only fully determined once we look at the
        declaration behind [p]. Thus, even when the type constructor application
        [Ts_constr((uid, p, ly), args)] itself carries a layout, the layout of
        the arguments is still to be determined.  *)
    | Ts_tuple of 'a ts list
    | Ts_unboxed_tuple of 'a ts list
    | Ts_var of string option * 'a
    | Ts_predef of Predef.t * without_layout ts list
        (** [Ts_predef(p, args)] is the shape for predefined type shapes.
          Analogously to [Ts_constr], its arguments do not carry a layout yet.
        *)
    | Ts_arrow of without_layout ts * without_layout ts
        (** [Ts_arrow(arg, ret)] is the shape for the function type
            [arg -> ret]. When emitting DWARF information for this type, there
            is no need to inspect the type arguments, and we never find out
            their layout. As such, these remain [without_layout] even after
            substituting in layouts.   *)
    | Ts_variant of 'a ts poly_variant_constructors
    | Ts_other of 'a

  and 'a poly_variant_constructors = 'a poly_variant_constructor list

  and 'a poly_variant_constructor =
    { pv_constr_name : string;
      pv_constr_args : 'a list
    }

  val shape_layout : Layout.t ts -> Layout.t

  val shape_with_layout : layout:Layout.t -> without_layout ts -> Layout.t ts

  val print : Format.formatter -> 'a ts -> unit

  val poly_variant_constructors_map :
    ('a -> 'b) -> 'a poly_variant_constructors -> 'b poly_variant_constructors

  val of_type_expr :
    Types.type_expr -> (Path.t -> Uid.t option) -> without_layout ts
end

module Type_decl_shape : sig
  (** For type substitution to work as expected, we store the layouts in the
      declaration alongside the shapes instead of directly going for the
      substituted version. *)
  type tds_desc =
    | Tds_variant of
        { simple_constructors : string list;
              (** The string is the name of the constructor. The runtime
                  representation of the constructor at index [i] in this list is
                  [2 * i + 1]. See [dwarf_type.ml] for more details. *)
          complex_constructors :
            (Type_shape.without_layout Type_shape.ts * Layout.t)
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
          arg_shape : Type_shape.without_layout Type_shape.ts;
          arg_layout : Layout.t
        }
        (** An unboxed variant corresponds to the [@@unboxed] annotation.
          It must have a single, complex constructor. *)
    | Tds_record of
        { fields :
            (string * Type_shape.without_layout Type_shape.ts * Layout.t) list;
          kind : record_kind
        }
    | Tds_alias of Type_shape.without_layout Type_shape.ts
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
      args : 'a complex_constructor_argument list
    }

  and 'a complex_constructor_argument =
    { field_name : string option;
      field_value : 'a
    }

  (* Unlike in [types.ml], we use [Layout.t] entries here, because we can
     represent flattened floats simply as float64 in the debugger. *)
  and constructor_representation = mixed_product_shape

  and mixed_product_shape = Layout.t array

  type tds =
    { path : Path.t;
      definition : tds_desc;
      type_params : Type_shape.without_layout Type_shape.ts list
    }

  val print : Format.formatter -> tds -> unit

  val replace_tvar : tds -> Type_shape.without_layout Type_shape.ts list -> tds

  val complex_constructor_map :
    ('a -> 'b) -> 'a complex_constructor -> 'b complex_constructor

  val complex_constructors_map :
    ('a -> 'b) -> 'a complex_constructors -> 'b complex_constructors

  val of_type_declaration :
    Path.t -> Types.type_declaration -> (Path.t -> Uid.t option) -> tds
end

type shape_with_layout =
  { type_shape : Type_shape.without_layout Type_shape.ts;
    type_layout : Layout.t
  }
(* CR sspies: There are two options here: We can fold the layout into the shape,
    or we can keep it on the outside. Currently, we keep it on the outside to
    make it easier to connect type shapes and shapes (which are agnostic about
   layouts) in subsequent PRs. *)

val all_type_decls : Type_decl_shape.tds Uid.Tbl.t

val all_type_shapes : shape_with_layout Uid.Tbl.t

(* Passing [Path.t -> Uid.t] instead of [Env.t] to avoid a dependency cycle. *)
val add_to_type_decls :
  Path.t -> Types.type_declaration -> (Path.t -> Uid.t option) -> unit

val add_to_type_shapes :
  Uid.t ->
  Types.type_expr ->
  Jkind_types.Sort.Const.t ->
  (Path.t -> Uid.t option) ->
  unit

val find_in_type_decls : Uid.t -> Type_decl_shape.tds option

val type_name : _ Type_shape.ts -> string
