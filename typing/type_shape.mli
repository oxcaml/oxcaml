(******************************************************************************
 *                                  OxCaml                                    *
 *                 Simon Spies and Mark Shinwell, Jane Street                 *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

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

  (* CR sspies: The treatment of extension constructors for the debugger has to
     be revised if we want to support them properly. The extension constructor
     declarations allocate new runtime objects that we would then have to find
     and display in the debugger. The flow for extension constructors is very
     different from regular type shapes. For now, we only support constructing
     shapes that Merlin understands. They should not end up in the shape
     reduction for DWARF emission at the moment. *)
  val of_extension_constructor_merlin_only :
    Types.extension_constructor -> Shape.t
end

module Evaluation_diagnostics : sig
  type t

  val no_diagnostics : t

  val create_diagnostics : unit -> t

  val get_reduction_steps : t -> int
end

(** [Evaluated_shape.unfold_and_evaluate] performs call-by-value evaluation of
    shapes. It should be applied after [reduce] (from [shape_reduce.ml]) has
    already been used. More specifically, when producing a type shape with
    [of_type_expr], the resulting shape is not a normal form. A normal form---at
    least for the emission into DWARF debug information---should no longer
    contain structures (from modules), lambdas (from functors and parametric
    types), application (from functor instantiation and type application),
    mutually recursive declarations and their projections ([Constr] and
    [Proj_decl]). (Due to missing information, it might still contain
    compilation units [Comp_unit] and projections [Proj] from them.)

    We reach the normal form of a type shape (e.g., during the emission of DWARF
    information) in two steps:

    + We execute [shape_reduce], which will substitute compilation units with
      their shapes loaded from .cms files and then reduce away projections
      [Proj] and functor applications. It will also, partially, deal with
      instantiation of type variables. That is, [of_type_declaration] inserts
      lambdas for non-recursive types with type arguments. These are reduced
      away via [shape_reduce]. (For (mutually-)recursive types, the next step
      handles the instantiation.)

    + After shape reduction, the type declaration can still contain recursive or
      mutually-recursive declarations, which have not been unfolded. Since we
      cannot directly emit these into DWARF, we then unfold recursive
      declarations applied to type shape arguments into recursive types with
      [Evaluated_shape.unfold_and_evaluate]. For example,
      [type 'a list = [] | :: of 'a * 'a list] applied to [int] becomes the type
      shape [Mu (Variant [] | :: of int * #0)]. *)
module Evaluated_shape : sig
  type t

  (** Evaluate a shape that has already been reduced via [shape_reduce]. This
      unfolds recursive declarations into recursive types using [Mu]. *)
  val unfold_and_evaluate :
    ?diagnostics:Evaluation_diagnostics.t -> Shape.t -> t

  (** Access the underlying shape. *)
  val shape : t -> Shape.t
end

type shape_with_layout = private
  { type_shape : Shape.t;
    type_layout : Layout.t;
    type_name : string
  }

val all_type_decls : Shape.t Uid.Tbl.t

val all_type_shapes : shape_with_layout Uid.Tbl.t

(* Passing [Path.t -> Uid.t] instead of [Env.t] to avoid a dependency cycle. *)
val add_to_type_decls :
  (Ident.t * Types.type_declaration) list -> path_lookup -> unit

val add_to_type_shapes :
  Uid.t -> Types.type_expr -> Layout.t -> name:string -> path_lookup -> unit

val print_table_all_type_decls : Format.formatter -> unit

val print_table_all_type_shapes : Format.formatter -> unit

val print_debug_uid_tables : Format.formatter -> unit
