(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2026 Jane Street Group LLC                                       *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)

(** [-functorize] driver: bundle parameterised modules into a single functor. *)

val validate_inputs : string list -> Compilation_unit.Name.Set.t

val functorize :
  Compilation_unit.Name.Set.t ->
  Misc.filepath ->
  with_info:
    (dump_ext:string -> Unit_info.t -> (Compile_common.info -> unit) -> unit) ->
  impl_ext:string ->
  read_format:
    (Misc.filepath -> Lambda.main_module_block_format * Lambda.arg_descr option) ->
  compile_program:(Compile_common.info -> Lambda.program -> unit) ->
  unit
(** Dispatch on the target extension: [.cmi] writes just the bundle interface;
    anything else emits both cmi and cmo/cmx via [compile_program]. [impl_ext]
    is the compiled-unit extension used for loading bundled deps ([.cmo] or
    [.cmx]) and doubles as the dump-file suffix. [with_info] is
    [Compile_common.with_info] with the caller's backend and tool name
    pre-applied. *)
