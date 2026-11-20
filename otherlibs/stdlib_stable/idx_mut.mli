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

(** Mutable indices into blocks. *)

(** An alias for the type of mutable indices into blocks. *)
type ('a : value_or_null, 'b : any) t : bits64 mod everything = ('a, 'b) idx_mut

(** [get a i] uses the index [i] to access [a]. *)
external get
  : ('a : value_or_null) ('b : any).
  ('a[@local_opt]) -> ('a, 'b) idx_mut -> ('b[@local_opt])
  = "%get_idx"
[@@layout_poly]

(** [set a i v] uses the index [i] to set [a] to [v].

    It can take [a] locally and [v] globally because mutable indices (e.g. to
    array elements or mutable record fields) can only be created to elements
    with the [global] modality. *)
external set
  : ('a : value_or_null) ('b : any).
  ('a[@local_opt]) -> ('a, 'b) idx_mut -> 'b -> unit
  = "%set_idx"
[@@layout_poly]

(** [unsafe_create_into_array i] creates an index into the [i]th element of an
    array.

    This is unsafe because it cannot check array bounds, so calling [get]/[set]
    with the index later could perform an unchecked out-of-bounds access. *)
external unsafe_create_into_array
  : ('a : any mod non_float). int -> ('a array, 'a) idx_mut
  = "%unsafe_array_idx"
[@@layout_poly]

external unsafe_create_into_array_indexed_by_int8
  : ('a : any mod non_float). int8# -> ('a array, 'a) idx_mut
  = "%unsafe_array_idx_indexed_by_int8#"
[@@layout_poly]

external unsafe_create_into_array_indexed_by_int16
  : ('a : any mod non_float). int16# -> ('a array, 'a) idx_mut
  = "%unsafe_array_idx_indexed_by_int16#"
[@@layout_poly]

external unsafe_create_into_array_indexed_by_int32
  : ('a : any mod non_float). int32_u -> ('a array, 'a) idx_mut
  = "%unsafe_array_idx_indexed_by_int32#"
[@@layout_poly]

external unsafe_create_into_array_indexed_by_int64
  : ('a : any mod non_float). int64_u -> ('a array, 'a) idx_mut
  = "%unsafe_array_idx_indexed_by_int64#"
[@@layout_poly]

external unsafe_create_into_array_indexed_by_nativeint
  : ('a : any mod non_float). nativeint_u -> ('a array, 'a) idx_mut
  = "%unsafe_array_idx_indexed_by_nativeint#"
[@@layout_poly]

(** [unsafe_advance i ~by] is the index of the array element [by] positions
    after the one that the index [i] points to.

    This is unsafe because it cannot check that [i] points to an element of an
    array, or array bounds, so calling [get]/[set] with the index later could
    perform an unchecked out-of-bounds access. The result is also unspecified
    if [i] was deepened into a field of the element, since only the position of
    the last step of an index is advanced on bytecode. *)
external unsafe_advance
  : ('a : value_or_null) ('b : any mod separable).
  ('a, 'b) idx_mut -> by:int -> ('a, 'b) idx_mut
  = "%unsafe_idx_array_advance"
[@@layout_poly]

external unsafe_advance_indexed_by_int8
  : ('a : value_or_null) ('b : any mod separable).
  ('a, 'b) idx_mut -> by:int8# -> ('a, 'b) idx_mut
  = "%unsafe_idx_array_advance_indexed_by_int8#"
[@@layout_poly]

external unsafe_advance_indexed_by_int16
  : ('a : value_or_null) ('b : any mod separable).
  ('a, 'b) idx_mut -> by:int16# -> ('a, 'b) idx_mut
  = "%unsafe_idx_array_advance_indexed_by_int16#"
[@@layout_poly]

external unsafe_advance_indexed_by_int32
  : ('a : value_or_null) ('b : any mod separable).
  ('a, 'b) idx_mut -> by:int32_u -> ('a, 'b) idx_mut
  = "%unsafe_idx_array_advance_indexed_by_int32#"
[@@layout_poly]

external unsafe_advance_indexed_by_int64
  : ('a : value_or_null) ('b : any mod separable).
  ('a, 'b) idx_mut -> by:int64_u -> ('a, 'b) idx_mut
  = "%unsafe_idx_array_advance_indexed_by_int64#"
[@@layout_poly]

external unsafe_advance_indexed_by_nativeint
  : ('a : value_or_null) ('b : any mod separable).
  ('a, 'b) idx_mut -> by:nativeint_u -> ('a, 'b) idx_mut
  = "%unsafe_idx_array_advance_indexed_by_nativeint#"
[@@layout_poly]
