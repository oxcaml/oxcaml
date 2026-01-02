(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                 Benedikt Meurer, University of Siegen                  *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2012 Benedikt Meurer.                                      *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Recognition and encoding of logical immediate arguments for ARM64 *)

(** [is_logical_immediate x] returns [true] if [x] is a valid logical immediate
    for ARM64 instructions. A valid logical immediate is:
    - neither [0] nor [-1];
    - composed of a repetition [BBBBB] of a bit-pattern [B] of length [e]
    - the low [e] bits of the number, that is, [B], match [0+1+0*] or [1+0+1*].
*)
val is_logical_immediate : nativeint -> bool

(** [encode_logical_immediate_fields x] encodes a logical immediate into
    (N, immr, imms) fields for ARM64 instructions.
    @raise Invalid_argument if [x] is not a valid logical immediate. *)
val encode_logical_immediate_fields : nativeint -> int * int * int
