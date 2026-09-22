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
type ('a : any, 'b : any) t : bits64 mod everything = ('a, 'b) idx_mut

(** [get a i] uses the index [i] to access [a]. *)
external get
  : ('a : any) ('b : any).
  ('a box[@local_opt]) -> ('a, 'b) idx_mut -> ('b[@local_opt])
  = "%get_idx"
[@@layout_poly]

(** [set a i v] uses the index [i] to set [a] to [v].

    It can take [a] locally and [v] globally because mutable indices (e.g. to
    array elements or mutable record fields) can only be created to elements
    with the [global] modality. *)
external set
  : ('a : any) ('b : any).
  ('a box[@local_opt]) -> ('a, 'b) idx_mut -> 'b -> unit
  = "%set_idx"
[@@layout_poly]

(** [compose outer inner] indexes a mutable part of the unboxed contents
    indexed by [outer]. *)
external compose
  : ('a : any) ('b : any) ('c : any).
  ('a, 'b) idx_mut -> ('b, 'c) idx_mut -> ('a, 'c) idx_mut
  = "%idx_compose"

(** [compose_imm outer inner] indexes an immutable part of the unboxed contents
    indexed by [outer]. The composed index remains mutable because [outer]
    is mutable. *)
external compose_imm
  : ('a : any) ('b : any) ('c : any).
  ('a, 'b) idx_mut -> ('b, 'c) idx_imm -> ('a, 'c) idx_mut
  = "%idx_compose"

(** [unsafe_create_into_array i] creates an index into the [i]th element of an
    array.

    This is unsafe because it cannot check array bounds, so calling [get]/[set]
    with the index later could perform an unchecked out-of-bounds access. *)
external unsafe_create_into_array
  : ('a : any mod non_float). int -> ('a array#, 'a) idx_mut
  = "%unsafe_array_idx"
[@@layout_poly]

external unsafe_create_into_array_indexed_by_int8
  : ('a : any mod non_float). int8# -> ('a array#, 'a) idx_mut
  = "%unsafe_array_idx_indexed_by_int8#"
[@@layout_poly]

external unsafe_create_into_array_indexed_by_int16
  : ('a : any mod non_float). int16# -> ('a array#, 'a) idx_mut
  = "%unsafe_array_idx_indexed_by_int16#"
[@@layout_poly]

external unsafe_create_into_array_indexed_by_int32
  : ('a : any mod non_float). int32_u -> ('a array#, 'a) idx_mut
  = "%unsafe_array_idx_indexed_by_int32#"
[@@layout_poly]

external unsafe_create_into_array_indexed_by_int64
  : ('a : any mod non_float). int64_u -> ('a array#, 'a) idx_mut
  = "%unsafe_array_idx_indexed_by_int64#"
[@@layout_poly]

external unsafe_create_into_array_indexed_by_nativeint
  : ('a : any mod non_float). nativeint_u -> ('a array#, 'a) idx_mut
  = "%unsafe_array_idx_indexed_by_nativeint#"
[@@layout_poly]
