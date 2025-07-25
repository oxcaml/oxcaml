module Uid = Shape.Uid
module Layout = Jkind_types.Sort.Const

type base_layout = Jkind_types.Sort.base

(* CR sspies: To prepare for moving the definitions of this file into [shapes.ml]
   the declarations for type shapes and type declaration shapes have been renamed
   in from [t] to [ts] (for type shape) and [tds] (for type declaration shape).*)

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

val type_name : _ Shape.ts -> string

val print_table_all_type_decls : Format.formatter -> unit

val print_table_all_type_shapes : Format.formatter -> unit

val print_debug_uid_tables : Format.formatter -> unit
