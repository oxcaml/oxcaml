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

(** Generate EH frame registration code for manual runtime registration.

    When using the dissector with LLD that lacks 64-bit EH frame support, we
    suppress LLD's 32-bit .eh_frame_hdr generation and instead register the
    .eh_frame section manually at runtime using libgcc's frame registration
    functions. *)

(** Linker script sections that define __EH_FRAME_BEGIN__ and provide an empty
    .eh_frame_hdr table. This should be included in the SECTIONS block of the
    linker script when manual EH frame registration is used. *)
val linker_script_sections : string

(** [generate ~temp_dir] creates a C file with .init/.fini constructors that
    call __register_frame_info_bases and __deregister_frame_info_bases, compiles
    it, and returns the path to the resulting object file.

    @param temp_dir Directory where the C and object files will be created
    @return Path to the compiled object file

    @raise Misc.Fatal_error if compilation fails *)
val generate : temp_dir:string -> string
