(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Joe Kerrigan, Jane Street, New York                    *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Atomic indices into blocks. *)

(** An alias for the type of atomic indices into blocks. *)
type ('a : value_or_null, 'b : any) t : bits64 mod everything =
  ('a, 'b) idx_atomic

(** [get a i] uses the index [i] to access [a]. *)
external get
  : ('a : value_or_null) ('b : any).
  ('a[@local_opt]) -> ('a, 'b) idx_atomic -> ('b[@local_opt])
  = "%get_idx_atomic"
[@@layout_poly]

(** [set a i v] uses the index [i] to set [a] to [v].

    It can take [a] locally and [v] globally because mutable indices (e.g. to
    array elements or mutable record fields) can only be created to elements
    with the [global] modality. *)
external set
  : ('a : value_or_null) ('b : any).
  ('a[@local_opt]) -> ('a, 'b) idx_atomic -> 'b -> unit
  = "%set_idx_atomic"
[@@layout_poly]
