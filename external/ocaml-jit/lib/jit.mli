(* Copyright (c) 2021 Nathan Rebours <nathan.p.rebours@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Import

type evaluation_outcome = Result of Obj.t | Exception of exn

val jit_load :
  (module Binary_emitter_intf.S
     with type Assembled_section.t = 'a
      and type Relocation.t = 'r) ->
  phrase_name:string ->
  outcome_ref:evaluation_outcome option ref ->
  'a String.Map.t ->
  unit
(** Load and run assembled binary sections. This is the generic primitive
    under [jit_load_program], for any architecture, and it checks nothing
    about how the sections were compiled: they carry no record of it. Code
    compiled with other frame-pointer, stack-check or AddressSanitizer
    settings than the calling program fails here all the same, on the symbol
    that ties generated code to its runtime ("Symbol not found:
    caml_runtime_codegen_variant_is_..."), but code compiled without poll
    insertion is loaded whatever the program needs. Callers that compile the
    sections themselves must make the checks of [jit_load_program]. *)

val jit_load_program :
  phrase_name:string -> Format.formatter -> Lambda.program -> evaluation_outcome
(** Load and run a Lambda program. Automatically selects backend based on
    architecture (x86 or arm64).

    The program is compiled with the current flags and runs in the image of
    the calling program: this function fails (with [Failure]) when the flags
    select other frame-pointer, stack-check or AddressSanitizer settings than
    the calling program was built with, or when the target is not the native
    one, which the caller has to set
    ([Opttoploop.check_codegen_variant_for_in_process_load]), or code without
    poll insertion in a fully polled program that may run more than one
    domain ([Opttoploop.check_poll_insertion_for_in_process_load]). It also
    fails whenever the flags select the LLVM backend, whose output it cannot
    load: the native target is not enough, the code must come from the native
    backend. *)

val jit_lookup_symbol : string -> Obj.t option

val set_debug : unit -> unit
(** Enables debugging if the OCAML_JIT_DEBUG env var is set. *)

val init_top : unit -> unit
(** Initializes the JIT for use via the native toplevel code. *)
