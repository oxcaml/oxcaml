(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                  *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type ('a : any) t = 'a ptr

(** [of_idx base index] retains [base] and points to the field selected by
    [index]. The result cannot outlive [base]. *)
external of_idx
  : ('a : any) ('b : any).
  ('a box[@local_opt]) -> ('a, 'b) idx_mut -> ('b t[@local_opt])
  = "%ptr_of_idx"

(** Read the pointed-to contents. *)
external unsafe_get
  : ('a : any). ('a t[@local_opt]) -> ('a[@local_opt])
  = "%unsafe_get_ptr"
[@@layout_poly]

(** Update the pointed-to contents. The caller must ensure that the stored
    value outlives the containing block. *)
external unsafe_set
  : ('a : any). ('a t[@local_opt]) -> ('a[@local_opt]) -> unit
  = "%unsafe_set_ptr"
[@@layout_poly]
