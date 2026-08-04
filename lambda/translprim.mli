(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Insertion of debugging events *)

val event_before : Lambda.scoped_location -> Typedtree.expression
                   -> Lambda.lambda -> Lambda.lambda

val event_after : Lambda.scoped_location -> Typedtree.expression
                  -> Lambda.lambda -> Lambda.lambda

(* Translation of primitives *)

val add_exception_ident : Ident.t -> unit
val remove_exception_ident : Ident.t -> unit

val clear_used_primitives : unit -> unit
val get_units_with_used_primitives: unit -> Compilation_unit.t list

val check_primitive_arity :
  Location.t -> Primitive.description -> unit

val transl_primitive :
  Lambda.scoped_location -> Primitive.description -> Env.t ->
  Types.type_expr ->
  poly_mode:Mode.Locality.l option ->
  poly_sort:Jkind.Sort.t option ->
  Path.t option ->
  Lambda.lambda

val transl_primitive_application :
  Lambda.scoped_location -> Primitive.description -> Env.t ->
  Types.type_expr ->
  poly_mode:Mode.Locality.l option -> stack:bool ->
  poly_sort:Jkind.Sort.t option -> Path.t ->
  Typedtree.expression option ->
  Lambda.lambda list -> Typedtree.expression list ->
  Lambda.region_close -> Lambda.lambda

(** [sort_of_native_repr] returns the sort expected after typechecking (which
    may be different than the sort used in the external interface).

    [poly_sort] must be [Some sort] when [Repr_poly] is given. It will produce
    fatal error if it's [None].  *)
val sort_of_native_repr :
  poly_sort:Jkind.Sort.t option -> Primitive.native_repr -> Jkind.Sort.Const.t

(** Whether an application of a primitive compiles to a direct primitive
    application, or whether [Translcore] first has to eta-expand the primitive
    into a closure.

    Set [check_poly_mode] to [true] to check zap mode and make precise decision
    for Prim_poly. *)
val can_apply_primitive :
  Primitive.description ->
  Mode.Locality.lr option ->
  Typedtree.apply_position ->
  (Typedtree.arg_label * Typedtree.apply_arg) list ->
  check_poly_mode:bool ->
  bool

(** The allocation an occurrence of a primitive makes, as the registration the
    type checker should perform. *)
type allocation_registration =
  | No_allocation_to_register
  | Register_heap
  | Register_at_locality of Mode.Locality.lr
      (** Register at the primitive's own result locality. *)

(** The whole allocation decision for an application of a primitive: whether
    [Translcore] will eta-expand it into a closure, whether the primitive
    itself allocates, and at which locality.

    This is total, and it leaves no trace on type, mode or sort inference
    variables: it answers conservatively rather than resolving anything or
    raising. A primitive-related error must not be reported during type
    checking -- translating the same primitive raises it again later, in the
    right phase and with the right location.

    [ty] is the instantiated type of the occurrence and [args] the typed
    arguments, both needed by the primitives whose behaviour depends on the
    types involved, such as [%compare] or [%array_unsafe_get]. [poly_mode] is
    used only to report the locality to register at. *)
val application_allocation :
  Env.t ->
  Location.t ->
  Primitive.description ->
  Typedtree.apply_position ->
  (Typedtree.arg_label * Typedtree.apply_arg) list ->
  poly_mode:Mode.Locality.lr option ->
  poly_sort:Jkind.Sort.t option ->
  ty:Types.type_expr ->
  allocation_registration

(** The allocation a primitive's own result makes. *)
val result_allocation :
  Primitive.description ->
  poly_mode:Mode.Locality.lr option ->
  allocation_registration

(* Errors *)

type invalid_stack_primitive =
  | Not_primitive
  | Not_allocating
  | Allocating_on_heap

type error =
  | Unknown_builtin_primitive of string
  | Wrong_arity_builtin_primitive of string
  | Wrong_layout_for_peek_or_poke of string
  | Invalid_floatarray_glb
  | Invalid_array_kind_for_uninitialized_makearray_dynamic
  | Invalid_stack_primitive of invalid_stack_primitive
  | Unable_to_specialize_array_idx_primitive of Types.type_expr
  | Element_would_be_reordered_in_record

exception Error of Location.t * error

val report_error :  error Format_doc.format_printer
val report_error_doc:  error Format_doc.printer
