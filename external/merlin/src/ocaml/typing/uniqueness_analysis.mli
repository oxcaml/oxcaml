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

open Typedtree

(* CR-soon zqian: rename the following so others can call
[Uniqueness_analysis.xxx] instead of
[Uniqueness_analysis.check_uniqueness_xxx] *)

(* Check that idents which are used more than once, are not used with mode
   unique. *)
val check_uniqueness_exp : expression -> unit

(* Check that idents which are used more than once, are not used with mode
   unique. *)
val check_uniqueness_value_bindings : value_binding list -> unit

(* These definitions are just to allow printing in the debugger *)
module type P := sig
  type t

  val print : Format.formatter -> t -> unit
end

module Occurrence : sig
  type t = { loc : Location.t }
end

module Maybe_unique : sig
  include P

  type axis =
    | Uniqueness
    | Linearity

  type cannot_force =
    { occ : Occurrence.t;
      axis : axis
    }
end

module Maybe_aliased : P

module Aliased : P

type usage_order =
  | Seq_before
  | Seq_after
  | Par

module Usage : sig
  include P

  type cannot_force_error =
    { cannot_force : Maybe_unique.cannot_force;
      there : t;
      order : usage_order
    }

  val extract_occurrence : t -> Occurrence.t option

  type action =
    | Use
    | Borrow
    | Read
    | Write

  type pattern_kind =
    | Lazy
    | Array
    | Constant

  type context =
    | Direct
    | In_pattern of pattern_kind
    | In_closure_that_might_be_called_later
    | While_being_borrowed

  type view =
    { action : action;
      context : context
    }

  val view : t -> view

  type unique_use_during_borrowing_error =
    { region_loc : Location.t;
      borrow_occ : Occurrence.t;
      cannot_force : Maybe_unique.cannot_force
    }

  exception Unique_use_during_borrowing of unique_use_during_borrowing_error
end

module Tag : sig
  type t =
    { tag : Types.tag;
      name_for_error : Longident.t Location.loc
    }

  val print : Format.formatter -> t -> unit
end

module Projection : sig
  type t =
    | Tuple_field of int
    | Record_field of string
    | Record_unboxed_product_field of string
    | Construct_field of string * int
    | Variant_field of Asttypes.label
    | Array_index of int
    | Memory_address

  val print : Format.formatter -> t -> unit
end

module Overwrites : sig
  type old_tag =
    | Old_tag_unknown
    | Old_tag_was of Tag.t
    | Old_tag_mutated of usage_order

  type error =
    | Changed_tag of
        { old_tag : old_tag;
          new_tag : Tag.t
        }
end

module Usage_tree : P

module Usage_forest : P

module Paths : P

module Value : P

module Ienv : sig
  include P

  module Extension : P
end

type boundary_reason =
  | Paths_from_mod_class
  | Free_var_of_mod_class
  | Out_of_mod_class

type relation =
  | Self
  | Ancestor of Projection.t list
  | Descendant of Projection.t list

type error =
  | Cannot_force of
      { inner : Usage.cannot_force_error;
        first_is_of_second : relation
      }
  | Boundary of
      { cannot_force : Maybe_unique.cannot_force;
        reason : boundary_reason
      }
  | Overwrite_changed_tag of Overwrites.error
  | Borrowed_out_of_context of Location.t
  | Borrowed_value_used_uniquely of Maybe_unique.cannot_force

exception Error of error
