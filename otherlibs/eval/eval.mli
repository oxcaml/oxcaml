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

(** Compiler settings passed to eval_quotation *)
type compiler_settings = {
  debug: bool;
  unsafe: bool;
  noassert: bool;
  native_code: bool;
}

(** Evaluate a quoted OCaml expression at runtime
    @param settings Compiler settings to use for compilation
    @param code The quoted code to compile and evaluate
    @param unit Unit parameter
    @return The result of evaluating the quoted code *)
val eval : compiler_settings -> CamlinternalQuote.Code.t -> 'b
