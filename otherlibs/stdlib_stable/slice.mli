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

(** Slices: contiguous sequences of elements in memory, as matched by slice
    patterns [[&| ... |]]. *)

type ('a : any) t :
  (value_or_null & bits64) & word mod forkable unyielding many stateless
  with 'a
  = 'a slice

(** [unsafe_of_addr addr ~len] is the slice of the [len] elements starting at
    [addr]. This is unsafe because it cannot check that these elements exist.
    The elements are as far apart as in an array of them. *)
val unsafe_of_addr : ('a : any). 'a Addr.t -> len:nativeint_u -> 'a t
[@@zero_alloc]

(** [of_array a] is the slice of all the elements of [a]. *)
val of_array : ('a : value mod non_float). 'a array -> 'a t
[@@zero_alloc]
