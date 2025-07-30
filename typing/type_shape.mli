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
end

(** When producing a type shape with [of_type_expr], the resulting shape is
      not in normal form. In particular, it can contain mutually recursive
      declarations from mutually recursive type declarations. This function
      should be applied after shape reduction. It unfoldes the mutually
      recursive types to result in one linearized form. *)
val unfold_and_evaluate : Shape.t -> Shape.t

type shape_with_layout =
  { type_shape : Shape.t;
    type_layout : Layout.t
  }

val all_type_decls : Shape.t Uid.Tbl.t

val all_type_shapes : shape_with_layout Uid.Tbl.t

(* Passing [Path.t -> Uid.t] instead of [Env.t] to avoid a dependency cycle. *)
val add_to_type_decls :
  (Ident.t * Types.type_declaration) list -> path_lookup -> unit

val add_to_type_shapes :
  Uid.t -> Types.type_expr -> Layout.t -> path_lookup -> unit

val find_in_type_decls : Uid.t -> Shape.t option

val print_table_all_type_decls : Format.formatter -> unit

val print_table_all_type_shapes : Format.formatter -> unit

val print_debug_uid_tables : Format.formatter -> unit

module Recursive_binder : sig
  type t

  val mk_recursive_binder : unit -> t

  val use_recursive_binder : t -> Shape.t

  val bind_recursive_binder : ?preserve_uid:bool -> t -> Shape.t -> Shape.t
end
