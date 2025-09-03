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
end

type shape_with_layout = private
  { type_shape : Shape.t;
    type_layout : Layout.t;
    type_name : string
  }
(* CR sspies: We need to revisit the treatment of layouts for type shapes.
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
