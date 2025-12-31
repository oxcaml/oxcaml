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

(** Build linker arguments from dissector results.

    After the dissector runs, OCaml object files have been partitioned and
    partially linked into partition object files. Passthrough files (C stubs
    and runtime libraries) are passed directly without partial linking because
    they may have sections like .gcc_except_table that don't work well with
    ld -r.

    The original linker invocation combines:
    - startup_obj (the startup code)
    - ml_objfiles (OCaml .o/.a files derived from .cmx/.cmxa)
    - ccobjs (C object files from -cclib, including lib_ccobjs from .cmxa)
    - runtime_libs (runtime library files)

    After dissector processing:
    - startup_obj and ml_objfiles are baked into partitions
    - ccobjs and runtime_libs are passed through directly (not partially linked)

    The caller must NOT add ccobjs or runtime_libs again when using dissector
    output - they are already included in object_files. *)

(** The linker arguments produced by the dissector. *)
type t

(** Returns the object files for the final linker. This includes:
    - Rewritten partition .o files (containing startup_obj and ml_objfiles
      that were partially linked)
    - Passthrough files (ccobjs and runtime_libs that bypass partial linking)

    The caller should pass these files to the linker and NOT add ccobjs or
    runtime_libs separately. *)
val object_files : t -> string list

(** Returns the path to the generated linker script. This should be passed to
    the linker via -Wl,-T,<path> added to Clflags.all_ccopts. *)
val linker_script : t -> string

(** [build result] constructs linker arguments from a dissector result.

    Returns the list of rewritten partition object files (with .rewritten
    suffix) and the linker script path. *)
val build : Dissector.Result.t -> t

(** [linker_script_flag t] returns the linker flag for the linker script,
    suitable for adding to Clflags.all_ccopts. Returns "-Wl,-T,<path>". *)
val linker_script_flag : t -> string
