(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*            Mark Shinwell and Xavier Clerc, Jane Street Europe          *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   Copyright 2017--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** {1 Validity of expressions in Flambda}

    Some operations/primitives (and therefore by extension expressions) can be
    valid only after certain points in the control flow of a program. For
    instance, the most obvisous is probably an array_load of an **immutable**
    array: the load is only valid after check on the length, or rather the
    behaviour of the generated code will change depending on whether the load is
    before or after the array length check, even though since this is for
    **immutable** arrays, neither the load nor the length access have any
    effects or coeffects. This shows instances where the regular notion of
    effects and co-effects is not enough to completely define which ordering of
    operations are equivalent.

    Therefore, we add a new notion to the effects and coeffects (and placement)
    bundle that is used by Flambda2 and more importantly to_cmm to decide which
    re-ordering of expressions are valid. *)

(** Validity of operations/expressions *)
type t =
  | Can_move_anywhere  (** Operations that are always valid *)
  | Can't_move_before_any_branch
      (** Operations whose evaluation may change after a certain control flow
          branch. Examples of these include immutable array operations, but also
          primitives that operate on values, but expect a particular shape for
          these values, since GADT matches can refine that shape.

          For instance addition of tagged integers technically accept any ocaml
          value, but expects its arguments to be tagged integers, which may be
          true only after a given GADT pattern match. Moving such an operation
          before the match would not only produce garbage (e.g. add two
          pointers), but also and most importantly produce garbage **that we
          think is a legal ocaml value** (since we wrongly assume that it is a
          tagged integer), that we may scan later, resulting in a segfault. *)
  | Control_flow_point
      (** A control flow point that branches and can refine types/shapes (e.g. a
          GADT pattern match), or more basically check some pre-conditions (e.g.
          an `if idx < Array.length arr - 1 then ...`).

          Note that in general any function calls can hide a control flow point:
          a function may perform any check/pattern match and raise an exception
          in some cases. So absent any further analysis, any function call
          should be considered as being an (implicit) control flow point. *)

(** Print function. *)
val print : Format.formatter -> t -> unit

(** Comaprison *)
val compare : t -> t -> int

(** Join *)
val join : t -> t -> t
