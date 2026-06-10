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

external memalign : int -> (Address.t, string) result = "jit_memalign"

external supports_unloading : unit -> bool = "jit_supports_unloading"

external load_section : Address.t -> string -> int -> unit = "jit_load_section"

external mprotect_ro : Address.t -> int -> (unit, int) result
  = "jit_mprotect_ro"

external mprotect_rx : Address.t -> int -> (unit, int) result
  = "jit_mprotect_rx"

external run_toplevel : Jit_unit.Entry_points.t -> Toplevel_res.t
  = "jit_run_toplevel"

external get_page_size : unit -> int = "jit_get_page_size"

external dlsym : string -> Address.t option = "jit_dlsym"

external register_unloadable_unit :
  nativeint
  (* Address of the unit's code-blocks sentinel array:
     [count; entry_1; code_block_1; ...; entry_count; code_block_count]. *) ->
  nativeint
  (* Address of the unit's data-blocks sentinel array:
     [count; addr_1; ...; addr_count]. 0n if absent. *) ->
  nativeint (* code_end *) ->
  nativeint (* frametable, or 0n if absent *) ->
  nativeint (* gc_roots, or 0n if absent *) ->
  nativeint (* JIT buffer base address *) ->
  int (* JIT buffer size in bytes *) ->
  unit = "jit_register_unloadable_unit_bytecode"
        "jit_register_unloadable_unit_native"
