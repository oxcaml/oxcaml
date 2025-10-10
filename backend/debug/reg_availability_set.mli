(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Register availability sets. *)

type t =
  | Ok of Reg_with_debug_info.Set_distinguishing_names_and_locations.t
  | Unreachable

(** See comments in the .ml file about these functions. *)

(* CR gyorsh/mshinwell: The treatment of Unreachable in the implementation of
   union,interf and diff seems inconsistent. What is the intended meaning of
   these operations on Unreachable? I guess my higher-level point is that we use
   Reg_availability_set.t for two different things: the domain in
   Cfg_availability and the keys in Compute_ranges for Available_ranges_vars.
   The latter only uses a canonicalized form of this set. I think this should be
   a separate type that is a simple set (not a variant type). Then, canonicalize
   will return None for Unreachable, matching the behavior expected in
   Compute_ranges. Also, we won't need to define any set operations on
   Unreachable, because the set operations on Reg_availability_set.t are only
   used in Compute_ranges. We can then expose join and less_equal directly from
   Available_ranges_vars. *)

val of_list : Reg_with_debug_info.t list -> t

val union : t -> t -> t

val inter : t -> t -> t

val inter_removing_conflicting_debug_info : t -> t -> t

val diff : t -> t -> t

(** This returns the initial value in the [Unreachable] case *)
val fold : (Reg_with_debug_info.t -> 'a -> 'a) -> t -> 'a -> 'a

(** Return a subset of the given availability set which contains no registers
    that are not associated with debug info (and holding values of
    non-persistent identifiers); and where no two registers share the same
    location. *)
val canonicalise : t -> t

val equal : t -> t -> bool

val subset : t -> t -> bool

(** For debugging purposes only. *)
val print :
  print_reg:(Format.formatter -> Reg.t -> unit) -> Format.formatter -> t -> unit
