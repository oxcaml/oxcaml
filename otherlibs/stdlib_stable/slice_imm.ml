(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Benjamin Peters, Jane Street, New York                *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type (+'a : any) t : (value_or_null & bits64) & word = 'a slice_imm

external of_parts
  : ('a : any).
  (#('a Addr_imm.t * nativeint_u)[@local_opt]) -> ('a t[@local_opt])
  = "%identity"

let[@zero_alloc] unsafe_of_addr addr ~len = of_parts #(addr, len)

external unbox_nativeint : nativeint -> nativeint_u = "%unbox_nativeint"

let[@zero_alloc] of_iarray (type a : value mod non_float) (arr : a iarray)
    : a t =
  unsafe_of_addr
    (Addr_imm.of_idx arr (Idx_imm.unsafe_create_into_iarray 0))
    ~len:(unbox_nativeint (Nativeint.of_int (Iarray.length arr)))
