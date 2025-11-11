(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
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

(** Collection of section states for all sections in an assembly unit. *)

module Asm_label = Asm_targets.Asm_label
module Asm_section = Asm_targets.Asm_section
module Asm_symbol = Asm_targets.Asm_symbol
module Symbol = Arm64_ast.Ast.Symbol

type t

val create : for_jit:bool -> t

val for_jit : t -> bool

val get_or_create : t -> Asm_section.t -> Section_state.t

val find : t -> Asm_section.t -> Section_state.t option

val find_exn : t -> Asm_section.t -> Section_state.t

val iter : t -> f:(Asm_section.t -> Section_state.t -> unit) -> unit

val fold :
  t -> init:'a -> f:(Asm_section.t -> Section_state.t -> 'a -> 'a) -> 'a

(** Individual sections: for sections tracked by their exact name string rather
    than the [Asm_section.t] enum. This is needed for function sections where
    each function gets its own [.text.caml.<funcname>] section. These sections
    are tracked separately so we can compare them individually against the
    assembler output during verification. *)

val get_or_create_individual : t -> string -> Section_state.t

val find_individual : t -> string -> Section_state.t option

val iter_individual : t -> f:(string -> Section_state.t -> unit) -> unit

val fold_individual :
  t -> init:'a -> f:(string -> Section_state.t -> 'a -> 'a) -> 'a

val find_in_any_individual_section :
  t -> Symbol.target -> (int * Asm_section.t) option

(** Search individual sections for a label or symbol. Returns (offset,
    section, state) if found. *)
val find_in_any_individual_section_with_state :
  t -> Symbol.target -> (int * Asm_section.t * Section_state.t) option

(** Search all sections for a label or symbol. Returns (offset, section,
    section_state) if found. This is needed when the caller needs to access
    the actual state where the label was found. *)
val find_in_any_section_with_state :
  t -> Symbol.target -> (int * Asm_section.t * Section_state.t) option

val find_in_any_section : t -> Symbol.target -> (int * Asm_section.t) option

val reset_offsets : t -> unit

module D = Asm_targets.Asm_directives

val add_direct_assignment : t -> string -> D.Directive.Constant.t -> unit

val find_direct_assignment : t -> string -> D.Directive.Constant.t option
