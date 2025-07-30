module Uid = Shape.Uid
module Layout = Jkind_types.Sort.Const

type base_layout = Jkind_types.Sort.base

module Type_shape : sig
  val of_type_expr :
    Types.type_expr -> (Path.t -> Uid.t option) -> Shape.without_layout Shape.ts
end

module Type_decl_shape : sig
  val of_type_declaration :
    Path.t -> Types.type_declaration -> (Path.t -> Uid.t option) -> Shape.tds
end

type shape_with_layout =
  { type_shape : Shape.without_layout Shape.ts;
    type_layout : Layout.t
  }
(* CR sspies: There are two options here: We can fold the layout into the shape,
    or we can keep it on the outside. Currently, we keep it on the outside to
    make it easier to connect type shapes and shapes (which are agnostic about
   layouts) in subsequent PRs. *)
(* CR sspies: We need to revist the treatment of layouts for type shapes.
   Currently, as the declaration above indicates, we use the layout from the
   binder of the variable and propagate it through. Once type shapes are merged
   into shapes, we should compute layouts on-demand from shapes directly. The
   reason is that, in the future, the layouts stored in the constructors of type
   declarations will become unreliable as a source of information. Instead, the
   strategy for layouts should be the following:

    1. For the closed types that we obtain at binders, compute the shape. In
       this step, type declarations with arguments should become lambdas, and
       type application should become application.
    2. When emitting the DWARF information, reduce the shape to resolve all
       abstraction/application pairs. Then emit the DWARF information by
       recursion on the resulting shape.
*)

val all_type_decls : Shape.tds Uid.Tbl.t

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

val find_in_type_decls : Uid.t -> Shape.tds option

(* CR sspies: [estimate_layout_from_shape] below is only an approximation. It
   does, for example, not deal with type application and, as a result, can find
   type variables that would have been substituted. This layout computation
   needs to be revisited once type shapes have been integrated into shapes.

   If the function returns [Some], the layout is precise (regardless of the
   issues mentioned above). It returns [None] whenever estimation failed.
*)
val estimate_layout_from_type_shape :
  Shape.without_layout Shape.ts -> Layout.t option

val estimate_layout_from_type_decl_shape : Shape.tds -> Layout.t option

val type_name : _ Shape.ts -> string

val print_table_all_type_decls : Format.formatter -> unit

val print_table_all_type_shapes : Format.formatter -> unit

val print_debug_uid_tables : Format.formatter -> unit
