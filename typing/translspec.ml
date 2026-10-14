(******************************************************************************
 *                                  OxCaml                                    *
 *                          Simon Spies, Jane Street                          *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
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

open Asttypes
open Typedtree
open Spec

type unsupported =
  | Arrays
  | Atomic_fields
  | Binding_operators
  | Block_indices
  | Borrow
  | Coercions
  | Comprehensions
  | Exclave
  | First_class_modules
  | Layout_polymorphism
  | Local_exceptions
  | Locally_abstract_types
  | Loops
  | Mutable_variables
  | Mutation
  | Objects
  | Overwrite
  | Polymorphic_annotations
  | Position_arguments
  | Probes
  | Quotations
  | Refutation_cases
  | Source_locations
  | Source_positions
  | Stack_allocation
  | Values_through_applications
  | Not_yet_supported of string

type error =
  | Unsupported of unsupported

exception Error of Location.t * error

let unsupported loc what = raise (Error (loc, Unsupported what))

(* Sorts and modes *)

let sort s = Jkind.Sort.default_for_transl_and_get s

let return_mode m = return_mode_zap_to_floor_exn m

(* Constants and partiality *)

let constant loc (c : Typedtree.constant) : Spec.constant =
  match c with
  | Const_int n -> Const_int n
  | _ -> unsupported loc (Not_yet_supported "constants other than integers")

(* Labels of arguments *)

let arg_label loc (lbl : Types.arg_label) : Asttypes.arg_label =
  match lbl with
  | Nolabel -> Nolabel
  | Labelled s -> Labelled s
  | Optional s -> Optional s
  | Position _ -> unsupported loc Position_arguments

(* Constructors and labels *)

(* A functor application names no particular instance of its values. *)
let check_global loc (path : Path.t) =
  if Path.contains_apply path then unsupported loc Values_through_applications

let global loc path = check_global loc path; path

(* Expressions. [bound] contains the local variables in scope. *)

let check_extra loc extra =
  match extra with
  | Texp_constraint _ | Texp_mode _ | Texp_inspected_type _ -> ()
  | Texp_poly _ -> unsupported loc Polymorphic_annotations
  | Texp_coerce _ -> unsupported loc Coercions
  | Texp_newtype _ -> unsupported loc Locally_abstract_types
  | Texp_stack -> unsupported loc Stack_allocation
  | Texp_borrowed -> unsupported loc Borrow
  | Texp_ghost_region -> unsupported loc Borrow

let rec expression bound (e : Typedtree.expression) =
  List.iter (fun (extra, loc, _) -> check_extra loc extra) e.exp_extra;
  { sexp_desc = expression_desc bound e.exp_loc e.exp_desc;
    sexp_type = e.exp_type;
    sexp_loc = e.exp_loc }

and expression_desc bound loc desc =
  match desc with
  | Texp_ident { path = Pident id; _ } when Ident.Set.mem id bound ->
      Sexp_var id
  | Texp_ident { desc = { val_kind = Val_prim prim; _ }; _ }
    when String.starts_with ~prefix:"%loc_" prim.prim_name ->
      (* [__LINE__] and the like depend on the location of their
         occurrence (see [Translprim]). *)
      unsupported loc Source_locations
  | Texp_ident { path; _ } -> Sexp_global (global loc path)
  | Texp_apply_layout _ -> unsupported loc Layout_polymorphism
  | Texp_constant c -> Sexp_constant (constant loc c)
  | Texp_let _ -> unsupported loc (Not_yet_supported "let-bindings")
  | Texp_letmutable _ -> unsupported loc Mutable_variables
  | Texp_function _ -> unsupported loc (Not_yet_supported "functions")
  | Texp_apply (funct, args, _, ret_mode, _, _) ->
      let args =
        List.map
          (fun (lbl, (arg : apply_arg)) ->
             let lbl = arg_label loc lbl in
             match arg with
             | Omitted _ -> (lbl, Omitted)
             | Arg (e, s) -> (lbl, Arg (expression bound e, sort s)))
          args
      in
      Sexp_apply
        { funct = expression bound funct;
          args;
          ret_mode = return_mode ret_mode }
  | Texp_match _ -> unsupported loc (Not_yet_supported "pattern matching")
  | Texp_try _ -> unsupported loc (Not_yet_supported "exception handlers")
  | Texp_unboxed_unit | Texp_unboxed_bool _ | Texp_tuple _
  | Texp_unboxed_tuple _ | Texp_construct _ | Texp_variant _ ->
      unsupported loc (Not_yet_supported "tuples, constructors and variants")
  | Texp_record _ | Texp_record_unboxed_product _ ->
      unsupported loc (Not_yet_supported "records")
  | Texp_atomic_loc _ -> unsupported loc Atomic_fields
  | Texp_field _ | Texp_unboxed_field _ ->
      unsupported loc (Not_yet_supported "field accesses")
  | Texp_setfield _ -> unsupported loc Mutation
  | Texp_array _ -> unsupported loc Arrays
  | Texp_idx _ -> unsupported loc Block_indices
  | Texp_list_comprehension _ | Texp_array_comprehension _ ->
      unsupported loc Comprehensions
  | Texp_ifthenelse _ -> unsupported loc (Not_yet_supported "conditionals")
  | Texp_sequence _ -> unsupported loc (Not_yet_supported "sequences")
  | Texp_while _ | Texp_for _ -> unsupported loc Loops
  | Texp_send _ | Texp_new _ | Texp_instvar _ | Texp_setinstvar _
  | Texp_override _ | Texp_object _ ->
      unsupported loc Objects
  | Texp_mutvar _ | Texp_setmutvar _ -> unsupported loc Mutable_variables
  | Texp_letmodule _ -> unsupported loc (Not_yet_supported "local modules")
  | Texp_letexception _ -> unsupported loc Local_exceptions
  | Texp_assert _ -> unsupported loc (Not_yet_supported "assertions")
  | Texp_lazy _ -> unsupported loc (Not_yet_supported "lazy values")
  | Texp_pack _ -> unsupported loc First_class_modules
  | Texp_letop _ -> unsupported loc Binding_operators
  | Texp_unreachable -> unsupported loc Refutation_cases
  | Texp_extension_constructor _ ->
      unsupported loc (Not_yet_supported "extension constructors")
  | Texp_open _ -> unsupported loc (Not_yet_supported "local opens")
  | Texp_probe _ | Texp_probe_is_enabled _ -> unsupported loc Probes
  | Texp_exclave _ -> unsupported loc Exclave
  | Texp_src_pos -> unsupported loc Source_positions
  | Texp_overwrite _ | Texp_hole _ -> unsupported loc Overwrite
  | Texp_quote _ | Texp_splice _ -> unsupported loc Quotations

let expression ~bound e = expression bound e

(* Errors *)

let describe = function
  | Arrays -> "arrays"
  | Atomic_fields -> "atomic fields"
  | Binding_operators -> "binding operators"
  | Block_indices -> "block indices"
  | Borrow -> "borrow_"
  | Coercions -> "coercions"
  | Comprehensions -> "comprehensions"
  | Exclave -> "exclave_"
  | First_class_modules -> "first-class modules"
  | Layout_polymorphism -> "layout-polymorphic bindings"
  | Local_exceptions -> "local exceptions"
  | Locally_abstract_types -> "locally abstract types"
  | Loops -> "loops"
  | Mutable_variables -> "mutable variables"
  | Mutation -> "mutation"
  | Objects -> "objects"
  | Overwrite -> "overwrite_"
  | Polymorphic_annotations -> "polymorphic type annotations"
  | Position_arguments -> "source position arguments"
  | Probes -> "probes"
  | Quotations -> "quotations"
  | Refutation_cases -> "refutation cases"
  | Source_locations -> "source locations"
  | Source_positions -> "source positions"
  | Stack_allocation -> "stack_"
  | Values_through_applications -> "values through functor applications"
  | Not_yet_supported what -> what

let report_error ppf = function
  | Unsupported what ->
      Format_doc.fprintf ppf "Laws do not support %s." (describe what)

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer ~loc report_error err)
    | _ -> None)
