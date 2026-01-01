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

type evaluation_outcome =
  | Result of Obj.t
  | Exception of exn

(** Load and run assembled binary sections.
    This is the main generic JIT entry point that works with any architecture. *)
val jit_load :
  (module Binary_emitter_intf.S
     with type Assembled_section.t = 'a
      and type Relocation.t = 'r) ->
  phrase_name:string ->
  outcome_ref:evaluation_outcome option ref ->
  'a String.Map.t ->
  unit

(** Load and run a Lambda program. Automatically selects backend based on
    architecture (x86 or arm64). *)
val jit_load_program :
  phrase_name:string ->
  Format.formatter ->
  Lambda.program ->
  evaluation_outcome

val jit_lookup_symbol : string -> Obj.t option

val set_debug : unit -> unit
(** Enables debugging if the OCAML_JIT_DEBUG env var is set. *)
