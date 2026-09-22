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

type (+'a : any) t = 'a ptr_imm

(** [of_idx base index] retains [base] and points to the field selected by
    [index]. The result cannot outlive [base]. *)
external of_idx
  : ('a : any) ('b : any).
  ('a box[@local_opt]) -> ('a, 'b) idx_imm -> ('b t[@local_opt])
  = "%ptr_of_idx"

(** Read the pointed-to contents. *)
external unsafe_get
  : ('a : any). ('a t[@local_opt]) -> ('a[@local_opt])
  = "%unsafe_get_ptr_imm"
[@@layout_poly]
