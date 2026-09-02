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

open! Import

val symbols : Symbols.t ref

val debug : bool ref

(** Keeps the most recently activated unloadable unit's module block alive
    from the moment the unit's static blocks are donated to the major heap
    until the caller has obtained its own reference to the module block
    (see [Jit.clear_unloadable_pin]). Being a global of this AOT library,
    this ref is scanned by the GC every cycle. *)
val unloadable_pin : Obj.t option ref
