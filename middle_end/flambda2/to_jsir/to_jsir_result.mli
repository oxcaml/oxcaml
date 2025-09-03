(******************************************************************************
 *                                  OxCaml                                    *
 *                           Leo Lee, Jane Street                             *
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

open! Jsoo_imports.Import

(** Result structure used during Flambda to Js_of_ocaml IR translation. *)

(** An accumulator for the JSIR blocks and instructions.

    Values of type [t] store complete/archived blocks, as well as a stack of
    "current" blocks that is still being worked on. *)

type t

(** Create a new result structure. It is not initialised with any blocks. *)
val create : unit -> t

(** Add a [Jsir.instr] to the top of the stack of current blocks and return the address of
    the new block. This function raises if there are no blocks being worked on. *)
val add_instr_exn : t -> Jsir.instr -> t

(** Add debugging information as an [Event] if it is not [_none_]. Raises if there are no
    blocks being worked on. [pos] controls whether we log the start of the region or the
    end. *)
val maybe_add_debuginfo_exn : t -> Debuginfo.t -> pos:[`Start | `End] -> t

(** Wrap any result changes between [`Start] and [`End] debuginfos. Raises if there are no
    blocks being worked on. *)
val with_debuginfo_exn : t -> Debuginfo.t -> f:(t -> 'a * t) -> 'a * t

(** Push a new block to the stack of current blocks. *)
val new_block : t -> params:Jsir.Var.t list -> t * Jsir.Addr.t

(** Return a new address without actually creating a block corresponding to that address.
    This is used for recursive continuations, where we need to use the address of a
    continuation before actually translating the body. The caller should promise that they
    call [new_block_with_addr] with the returned address at some point before
    [to_program_exn] is called. *)
val reserve_address : t -> t * Jsir.Addr.t

(** Make a new block, but using [addr]. Raises if [addr] is not a reserved address. *)
val new_block_with_addr_exn :
  t -> params:Jsir.Var.t list -> addr:Jsir.Addr.t -> t

(** End the block at the top of the current stack, setting [last] to the given argument.
    This function raises if there are no blocks being worked on. *)
val end_block_with_last_exn : t -> Jsir.last -> t

(** Returns the address for a special block for invalid switches, creating one if it
    doesn't exist already. *)
val invalid_switch_block : t -> t * Jsir.Addr.t

(** Get a public method and return the function. Increments the method cache ID. *)
val get_public_method :
  t -> obj:Jsir.Var.t -> field:Jsir.Var.t -> t * Jsir.Var.t

(** Register the fact that we import the (toplevel module of the) given compilation unit
    from the JSOO global data table. This is used to inform Js_of_ocaml that it needs to
    add this compilation unit to the global data table. *)
val import_compilation_unit : t -> Compilation_unit.t -> t
(* CR selee: Eventually this should also be used for symbols too, so that we
   don't put unused symbols in the symbol table. *)

val global_data_var : t -> t * Jsir.Var.t

type program =
  { program : Jsir.program;
    imported_compilation_units : Compilation_unit.Set.t
  }

(** Create a [program] with the blocks in the result, including the
    current block. This function raises if there are still blocks being
    worked on, or there is a reserved address that has not been used yet. *)
val to_program_exn : t -> program
