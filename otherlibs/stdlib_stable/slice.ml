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

type ('a : any) t : (value_or_null & bits64) & word = 'a slice

external of_parts
  : ('a : any). (#('a Addr.t * nativeint_u)[@local_opt]) -> ('a t[@local_opt])
  = "%identity"

let[@zero_alloc] unsafe_of_addr addr ~len = of_parts #(addr, len)

external unbox_nativeint : nativeint -> nativeint_u = "%unbox_nativeint"

let[@zero_alloc] of_array (type a : value mod non_float) (arr : a array) : a t =
  unsafe_of_addr
    (Addr.of_idx arr (Idx_mut.unsafe_create_into_array 0))
    ~len:(unbox_nativeint (Nativeint.of_int (Array.length arr)))
