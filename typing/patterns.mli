(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Gabriel Scherer, projet Partout, INRIA Paris-Saclay           *)
(*          Thomas Refis, Jane Street Europe                              *)
(*                                                                        *)
(*   Copyright 2019 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Typedtree
open Types
open Data_types

val omega : pattern
(** aka. "Tpat_any" or "_"  *)

val omegas : int -> pattern list
(** [List.init (fun _ -> omega)] *)

val omega_list : 'a list -> pattern list
(** [List.map (fun _ -> omega)] *)

module Non_empty_row : sig
  type 'a t = 'a * Typedtree.pattern list

  val of_initial : Typedtree.pattern list -> Typedtree.pattern t
  (** 'assert false' on empty rows *)

  val map_first : ('a -> 'b) -> 'a t -> 'b t
end

(** Slice patterns either match slices of an exact length
    ([[&| p1; ...; pn |]]) or slices of a minimum length
    ([[&| p1; ...; pk; ..; q1; ...; qm |]]). *)
module Slice : sig
  (** The lengths a slice pattern matches *)
  type kind =
    | Closed of int  (** exactly this length *)
    | Open of { prefix : int; suffix : int }
        (** at least [prefix + suffix], with a rest between them *)

  (** The element patterns of a slice pattern, without the variable bound by
      its rest *)
  type elements =
    | Elements_closed of pattern list
    | Elements_open of { prefix : pattern list; suffix : pattern list }

  (** Ignores the variable bound by the rest *)
  val of_typed : slice_elements -> elements

  val to_typed : elements -> slice_elements

  val kind : elements -> kind

  (** All element patterns, in order *)
  val patterns : elements -> pattern list

  (** The number of element patterns of this kind *)
  val arity : kind -> int

  (** Inverse of [patterns] *)
  val of_patterns : kind -> pattern list -> elements

  (** Variable-length patterns are not alone in their signature, so the
      lengths of a column of slice patterns are split into finitely many
      classes that behave like constructors, as rustc does.

      Let [F] be the largest fixed length and [P] and [S] the largest prefix
      and suffix of the variable-length patterns in the column. If
      [F + 1 >= P + S], increase [P] to [F + 1 - S], and let [L = P + S]. Then
      all slices of length at least [L] are matched by the same patterns in
      the column: by none of the fixed-length patterns, and by all of the
      variable-length patterns, without their prefixes and suffixes
      overlapping. Hence [split kinds] is [Closed 0], ..., [Closed (L - 1)]
      and [Open { prefix = P; suffix = S }]. *)
  val split : kind list -> kind list

  (** [covers d h]: whether a pattern of kind [h] matches all slices of the
      constructor [d] of a split *)
  val covers : kind -> kind -> bool

  (** [args d h ps]: the arguments of a pattern of kind [h] with element
      patterns [ps] for a constructor [d] that it covers: its prefix, then
      wildcards for the elements between its prefix and suffix, then its
      suffix. *)
  val args : kind -> kind -> pattern list -> pattern list

  (** [common_args e1 e2]: the constructors of the split of the lengths of
      two slice patterns that both cover, with the arguments of both *)
  val common_args :
    elements -> elements -> (kind * pattern list * pattern list) list

  (** [missing kinds]: given the constructors of a split that a column
      contains, a kind of slices that none of them matches, if there is
      one *)
  val missing : kind list -> kind option
end

module Simple : sig
  type view = [
    | `Any
    | `Constant of constant
    | `Unboxed_unit
    | `Unboxed_bool of bool
    | `Tuple of (string option * pattern * Jkind.sort) list
    | `Unboxed_tuple of (string option * pattern * Jkind.sort) list
    | `Construct of
        Longident.t loc * constructor_description * constructor_representation
        * (Jkind.sort * pattern) list
    | `Variant of label * pattern option * row_desc ref
    | `Record of
        (Longident.t loc * label_description * pattern) list
        * record_representation * closed_flag
    | `Record_unboxed_product of
        (Longident.t loc * unboxed_label_description * pattern) list
        * record_unboxed_product_representation * closed_flag
    | `Array of mutability * Jkind.sort * pattern list
    | `Addr of mutability * Jkind.sort * pattern
    | `Slice of mutability * Jkind.sort * Slice.elements
    | `Lazy of pattern
  ]
  type pattern = view pattern_data

  val omega : [> view ] pattern_data
end

module Half_simple : sig
  type view = [
    | Simple.view
    | `Or of pattern * pattern * row_desc option
  ]
  type pattern = view pattern_data
end

module General : sig
  type view = [
    | Half_simple.view
    | `Var of
        Ident.t * string loc * Uid.t * Jkind.Sort.t * Mode.With_regionality.l
    | `Fun_layout of Ident.t * string loc * Uid.t
                   * Jkind.Sort.t * Mode.With_regionality.l * Types.Lpoly.t
                   * locality_mode_r
    | `Alias of
        pattern
        * Ident.t
        * string loc
        * Uid.t
        * Jkind.Sort.t
        * Mode.With_regionality.l
        * Types.type_expr
    | `Deref of
        Ident.t
        * string loc
        * Uid.t
        * Jkind.Sort.t
        * Mode.With_regionality.l
        * Types.type_expr
    | `Alias_deref of
        pattern
        * Ident.t
        * string loc
        * Uid.t
        * Jkind.Sort.t
        * Mode.With_regionality.l
        * Types.type_expr
    | `Slice_rest of
        mutability * Jkind.sort * pattern list * slice_rest_var * pattern list
        (** A slice pattern whose rest binds a variable, with its prefix and
            suffix *)
  ]
  type pattern = view pattern_data

  val view : Typedtree.pattern -> pattern
  val erase : [< view ] pattern_data -> Typedtree.pattern

  val strip_vars : pattern -> Half_simple.pattern
end

module Head : sig
  type desc =
    | Any
    | Construct of
        constructor_description * constructor_representation * Jkind.sort list
    | Constant of constant
    | Unboxed_unit
    | Unboxed_bool of bool
    | Tuple of (string option * Jkind.sort) list
    | Unboxed_tuple of (string option * Jkind.sort) list
    | Record of
        label_description list * record_representation
    | Record_unboxed_product of
        unboxed_label_description list *
        record_unboxed_product_representation
    | Variant of
        { tag: label; has_arg: bool;
          cstr_row: row_desc ref;
          type_row : unit -> row_desc; }
          (* the row of the type may evolve if [close_variant] is called,
             hence the (unit -> ...) delay *)
    | Array of mutability * Jkind.sort * int
    | Addr of mutability * Jkind.sort
    | Slice of { mutability : mutability; sort : Jkind.sort;
                 kind : Slice.kind }
    | Lazy

  type t = desc pattern_data

  val arity : t -> int

  (** [deconstruct p] returns the head of [p] and the list of sub patterns.

      @raise [Invalid_arg _] if [p] is an or- or an exception-pattern.  *)
  val deconstruct : Simple.pattern -> t * pattern list

  (** reconstructs a pattern, putting wildcards as sub-patterns. *)
  val to_omega_pattern : t -> pattern

  val omega : t

end
