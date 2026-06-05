<<<<<<< HEAD
module Type_tree : sig
  type node_data =
    | Arrow
    | Tuple
    | Unboxed_tuple
    | Object
    | Poly_variant
    | Type_ref of { path : Path.t; ty : Types.type_expr }
    | Other of Types.type_expr

  type t = { data : node_data; children : t list }
end

(** Convert a type into a simplified tree representation. *)
val create_type_tree : Types.type_expr -> Type_tree.t
||||||| c76379cdae
=======
module Type_tree : sig
  type type_ref_payload = { path : Path.t; ty : Types.type_expr }
  type t = type_ref_payload Query_protocol.Locate_types_result.Tree.t
end

(** Convert a type into a simplified tree representation. *)
val create_type_tree : Types.type_expr -> Type_tree.t option
>>>>>>> v5.6-504
