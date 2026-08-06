(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                    Nathanaëlle Courant, OCamlPro                       *)
(*                                                                        *)
(*   Copyright 2026 OCamlPro SAS                                          *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** "Check actions" are ghost code to check some properties of the program. They
    are always associated with an [Apply_cont] node; the check action is
    executed before the application of the continuation.

    For now, check actions are isomorphic to the [Close_alloc_region] primitive
    (see [Flambda_primitive.of_check_action]). *)

(** The exits of an allocation region at which it can be explicitly closed
    ([Alloc_checks.per_exit] minus divergence, which no explicit closing can
    observe). *)
type close_alloc_region_type =
  | Normal
  | Exn
  | Notrace

val compare_close_alloc_region_type :
  close_alloc_region_type -> close_alloc_region_type -> int

(** The Forward/Close flag corresponding to the given region exit kind. *)
val alloc_check_for_close_alloc_region_type :
  Alloc_checks.t -> close_alloc_region_type -> Alloc_checks.check

type t =
  | Close_alloc_region of
      { exit : close_alloc_region_type;
        region : Variable.t
      }

include Expr_std.S with type t := t

val compare : t -> t -> int

val equal : t -> t -> bool

val ids_for_export : t -> Ids_for_export.t

val apply_renaming : t -> Renaming.t -> t
