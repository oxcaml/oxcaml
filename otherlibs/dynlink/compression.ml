(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*        Xavier Leroy, Collège de France and Inria project Cambium       *)
(*                                                                        *)
(*   Copyright 2023 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* This is a copy of [utils/compression.ml] without the reference to the
   primitive, so that dynlink.cmxa doesn't need to link with -lcomprmarsh.
   This disappears when dynlink_compilerlibs is removed, which is a prereq
   for having actual marshalled compression. *)

let compression_supported = false

let output_value = Stdlib.output_value

let input_value = Stdlib.input_value
