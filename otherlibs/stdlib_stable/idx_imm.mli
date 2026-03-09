(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                    Ryan Tjoa, Jane Street, New York                    *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Immutable indices into blocks. *)

(** An alias for the type of immutable indices into blocks. *)
type ('a : value_or_null, 'b : any) t : bits64 mod everything = ('a, 'b) idx_imm

(** [get a i] uses the index [i] to access [a]. *)
external get
  : ('a : value_or_null) ('b : any).
  ('a[@local_opt]) -> ('a, 'b) idx_imm -> ('b[@local_opt])
  = "%get_idx_imm"
[@@layout_poly]

(** [unsafe_create_into_iarray i] creates an index into the [i]th element of an
    immutable array.

    This is unsafe because it cannot check array bounds, so calling [get] with
    the index later could perform an unchecked out-of-bounds access. *)
external unsafe_create_into_iarray
  : ('a : any mod non_float). int -> ('a iarray, 'a) idx_imm
  = "%unsafe_array_idx"
[@@layout_poly]

external unsafe_create_into_iarray_indexed_by_int8
  : ('a : any mod non_float). int8# -> ('a iarray, 'a) idx_imm
  = "%unsafe_array_idx_indexed_by_int8#"
[@@layout_poly]

external unsafe_create_into_iarray_indexed_by_int16
  : ('a : any mod non_float). int16# -> ('a iarray, 'a) idx_imm
  = "%unsafe_array_idx_indexed_by_int16#"
[@@layout_poly]

external unsafe_create_into_iarray_indexed_by_int32
  : ('a : any mod non_float). int32# -> ('a iarray, 'a) idx_imm
  = "%unsafe_array_idx_indexed_by_int32#"
[@@layout_poly]

external unsafe_create_into_iarray_indexed_by_int64
  : ('a : any mod non_float). int64# -> ('a iarray, 'a) idx_imm
  = "%unsafe_array_idx_indexed_by_int64#"
[@@layout_poly]

external unsafe_create_into_iarray_indexed_by_nativeint
  : ('a : any mod non_float). nativeint# -> ('a iarray, 'a) idx_imm
  = "%unsafe_array_idx_indexed_by_nativeint#"
[@@layout_poly]
