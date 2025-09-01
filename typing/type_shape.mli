module Uid = Shape.Uid
module Layout = Jkind_types.Sort.Const

type base_layout = Jkind_types.Sort.base

type path_lookup = Path.t -> args:Shape.t list -> Shape.t option

module Type_shape : sig
  val of_type_expr : Types.type_expr -> path_lookup -> Shape.t
end

module Type_decl_shape : sig
  val of_type_declarations :
    (Ident.t * Types.type_declaration) list -> path_lookup -> Shape.t list

  val of_type_declaration :
    Ident.t -> Types.type_declaration -> path_lookup -> Shape.t

  (* CR sspies: The treatment of extension constructors for the debugger would
     need to be revisited, since the extension constructor declarations declare
     new runtime objects that we would then have to find and display differently
     in the debugger. The flow there is very different than for the regular type
     shapes. For now, we only support constructing shapes that Merlin
     understands. They should not end up in the shape reduction for DWARF
     emission at the moment. *)
  val of_extension_constructor_merlin_only :
    Types.extension_constructor -> Shape.t
end

(** When producing a type shape with [of_type_expr], the resulting shape is
      not in normal form. In particular, it can contain mutually recursive
      declarations from mutually recursive type declarations. This function
      should be applied after shape reduction. It unfoldes the mutually
      recursive types to result in one linearized form. *)
val unfold_and_evaluate : Shape.t -> Shape.t

type shape_with_layout = private
  { type_shape : Shape.t;
    type_layout : Layout.t;
    type_name : string
  }
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

val all_type_decls : Shape.t Uid.Tbl.t

val all_type_shapes : shape_with_layout Uid.Tbl.t

(* Passing [Path.t -> Uid.t] instead of [Env.t] to avoid a dependency cycle. *)
val add_to_type_decls :
  (Ident.t * Types.type_declaration) list -> path_lookup -> unit

val add_to_type_shapes :
  Uid.t -> Types.type_expr -> Layout.t -> name:string -> path_lookup -> unit

(* CR sspies: [estimate_layout_from_shape] below is only an approximation. It
   does, for example, not deal with type application and, as a result, can find
   type variables that would have been substituted. This layout computation
   needs to be revisited once type shapes have been integrated into shapes.

   If the function returns [Some], the layout is precise (regardless of the
   issues mentioned above). It returns [None] whenever estimation failed.
*)
val estimate_layout_from_type_shape : Shape.t -> Layout.t option

val print_table_all_type_decls : Format.formatter -> unit

val print_table_all_type_shapes : Format.formatter -> unit

val print_debug_uid_tables : Format.formatter -> unit
