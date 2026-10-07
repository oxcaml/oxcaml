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

type ('a : any) t : value_or_null & bits64 = 'a addr

val of_idx
  : ('a : value) ('b : any).
  'a -> ('a, 'b) idx_mut -> 'b t
[@@zero_alloc]
val of_idx_local
  : ('a : value) ('b : any).
  'a @ local -> ('a, 'b) idx_mut -> 'b t @ local
[@@zero_alloc]
val of_idx_read
  : ('a : value) ('b : any).
  'a @ read -> ('a, 'b) idx_mut -> 'b t @ read
[@@zero_alloc]
val of_idx_read_local
  : ('a : value) ('b : any).
  'a @ local read -> ('a, 'b) idx_mut -> 'b t @ local read
[@@zero_alloc]
val of_idx_write
  : ('a : value) ('b : any).
  'a @ write -> ('a, 'b) idx_mut -> 'b t @ write
[@@zero_alloc]
val of_idx_write_local
  : ('a : value) ('b : any).
  'a @ local write -> ('a, 'b) idx_mut -> 'b t @ local write
[@@zero_alloc]

external of_imm
  : ('a : any).
  'a Addr_imm.t @ read -> 'a t @ read
  = "%identity"

external get
  : ('a : any).
  ('a t[@local_opt]) -> ('a[@local_opt])
  = "%unsafe_get_ptr"
[@@layout_poly]
external get_read
  : ('a : any).
  ('a t[@local_opt]) @ read -> ('a[@local_opt]) @ read
  = "%unsafe_get_ptr"
[@@layout_poly]

external set
  : ('a : any).
  ('a t[@local_opt]) @ write -> 'a -> unit
  = "%unsafe_set_ptr"
[@@layout_poly]
