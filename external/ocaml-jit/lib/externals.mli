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

val memalign : int -> (Address.t, string) result

(** Whether the build's allocator backend supports unloading JIT'd CUs. False
    when the JIT buffer is allocated via [sbrk] (Linux ASan / TCMalloc) or out
    of a static [.bss] arena (musl), neither of which can be passed to [free].
    Drives [Eval.eval]'s choice between unloadable and forever-leaked CUs. *)
val supports_unloading : unit -> bool

val load_section : Address.t -> string -> int -> unit

val mprotect_ro : Address.t -> int -> (unit, int) result

val mprotect_rx : Address.t -> int -> (unit, int) result

val run_toplevel : Jit_unit.Entry_points.t -> Toplevel_res.t

val get_page_size : unit -> int

val dlsym : string -> Address.t option

val register_unloadable_unit :
  nativeint
  (* Address of the unit's code-blocks sentinel array:
     [count; entry_1; code_block_1; ...; entry_count; code_block_count]. *) ->
  nativeint (* unloadable_blocks_start *) ->
  nativeint (* unloadable_blocks_end *) ->
  nativeint (* code_end *) ->
  nativeint (* frametable, or 0n if absent *) ->
  nativeint (* gc_roots, or 0n if absent *) ->
  nativeint (* JIT buffer base address *) ->
  int (* JIT buffer size in bytes *) ->
  nativeint (* unit handle for [activate_unloadable_unit] *)

(** Activate a registered unloadable unit: unregister its gc_roots
    dyn-globals and donate its static-block region to the major heap as a
    heap extent. Must be called exactly once, after the unit's initialiser
    has finished (normally or with an exception). *)
val activate_unloadable_unit : nativeint -> unit
