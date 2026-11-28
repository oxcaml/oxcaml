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

(** Formatting of the code of a function for [%%expect_asm] tests. The output is
    the same whatever the target system: symbols have no platform prefix,
    relocations use the syntax of the GNU assembler and local labels are
    numbered from 0 in order of definition. *)

module Line : sig
  type t =
    | Instruction of Ast.Instruction.t
    | Directive of Asm_targets.Asm_directives.Directive.t
end

(** Format [body] as the code of the function [name]. As on x86, references to
    labels defined in [hidden_gc_jump_pads] but not in [body] are shown as GC
    jump pads, and references to other labels not defined in [body], as well as
    address computations for symbols of the toplevel ([camlTOP*]), are hidden.
*)
val format :
  name:string -> body:Line.t list -> hidden_gc_jump_pads:Line.t list -> string
