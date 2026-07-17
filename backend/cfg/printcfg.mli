(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

(** Terse rendering of a basic instruction description, with no register
    interleaving and no colour wrapping (suitable for error messages). *)
val basic_desc : Format.formatter -> Cfg.basic -> unit

(** Terse rendering of a terminator description, with no register interleaving
    and no colour wrapping (suitable for error messages). *)
val terminator_desc : ?sep:string -> Format.formatter -> Cfg.terminator -> unit

(** Renders the body of a basic instruction (no result, no instruction id, no
    surrounding colour wrapping). Register arguments are interleaved into the
    operation syntax for the [Op] case, and printed via [print_reg]. *)
val basic_body :
  ?print_reg:(Format.formatter -> Reg.t -> unit) ->
  Format.formatter ->
  Cfg.basic Cfg.instruction ->
  unit

(** Renders the body of a terminator instruction (no result, no instruction id,
    no surrounding colour wrapping). Register arguments are interleaved into the
    terminator syntax, and printed via [print_reg]. *)
val terminator_body :
  ?print_reg:(Format.formatter -> Reg.t -> unit) ->
  ?sep:string ->
  Format.formatter ->
  Cfg.terminator Cfg.instruction ->
  unit

(** Full rendering of a basic instruction (result, body) with colour wrapping.
*)
val basic : Format.formatter -> Cfg.basic Cfg.instruction -> unit

(** Full rendering of a terminator instruction (result, body) with colour
    wrapping. *)
val terminator : Format.formatter -> Cfg.terminator Cfg.instruction -> unit

(** Full rendering of an instruction (basic or terminator) with colour wrapping.
*)
val instruction :
  Format.formatter ->
  [ `Basic of Cfg.basic Cfg.instruction
  | `Terminator of Cfg.terminator Cfg.instruction ] ->
  unit

(** Variant of {!basic} taking a custom register printer. *)
val basic_with_print_reg :
  print_reg:(Format.formatter -> Reg.t -> unit) ->
  Format.formatter ->
  Cfg.basic Cfg.instruction ->
  unit

(** Variant of {!terminator} taking a custom register printer. *)
val terminator_with_print_reg :
  print_reg:(Format.formatter -> Reg.t -> unit) ->
  Format.formatter ->
  Cfg.terminator Cfg.instruction ->
  unit

(** Variant of {!instruction} taking a custom register printer. *)
val instruction_with_print_reg :
  print_reg:(Format.formatter -> Reg.t -> unit) ->
  Format.formatter ->
  [ `Basic of Cfg.basic Cfg.instruction
  | `Terminator of Cfg.terminator Cfg.instruction ] ->
  unit

(** Full rendering of a CFG (blocks, instructions, edges) with colour wrapping.
    Blocks are visited in [Label.compare] order. *)
val cfg : Format.formatter -> Cfg.t -> unit

(** Like {!cfg}, but interleaves liveness information at each instruction. *)
val cfg_with_liveness :
  Format.formatter ->
  Cfg.t ->
  Cfg_liveness.Liveness.domain InstructionId.Tbl.t ->
  unit

(** Like {!cfg}, but visits blocks in the layout's order. *)
val cfg_with_layout : Format.formatter -> Cfg_with_layout.t -> unit
