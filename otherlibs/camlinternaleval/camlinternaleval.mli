(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Implementation of [%eval]                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type compiler_settings = {
  debug : bool;
  unsafe : bool;
  noassert : bool;
  native_code : bool;
}
(** Compiler settings passed to eval *)

val eval : CamlinternalQuote.Code.t -> 'b
(** Evaluate a quoted OCaml expression at runtime. *)
