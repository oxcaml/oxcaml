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

[@@@ocaml.flambda_o3]

type ('a : value_or_null, 'b : any) t : bits64 mod everything = ('a, 'b) idx_mut

external get
  : ('a : value_or_null) ('b : any).
  ('a[@local_opt]) -> ('a, 'b) idx_mut -> ('b[@local_opt])
  = "%get_idx"
[@@layout_poly]

external set
  : ('a : value_or_null) ('b : any).
  ('a[@local_opt]) -> ('a, 'b) idx_mut -> 'b -> unit
  = "%set_idx"
[@@layout_poly]

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
  : ('a : any mod non_float). int32# -> ('a array, 'a) idx_mut
  = "%unsafe_array_idx_indexed_by_int32#"
[@@layout_poly]

external unsafe_create_into_array_indexed_by_int64
  : ('a : any mod non_float). int64# -> ('a array, 'a) idx_mut
  = "%unsafe_array_idx_indexed_by_int64#"
[@@layout_poly]

external unsafe_create_into_array_indexed_by_nativeint
  : ('a : any mod non_float). nativeint# -> ('a array, 'a) idx_mut
  = "%unsafe_array_idx_indexed_by_nativeint#"
[@@layout_poly]
