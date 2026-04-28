(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Mark Shinwell, Jane Street                       *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Emit a [Code_block] static-data item for one function in an unloadable
    compilation unit. A [Code_block] is a heap-shaped scannable block containing
    pointers to the [Code_block]s of unloadable code dependencies and to data
    dependencies, used by the major-GC mark phase to keep an unloadable
    function's transitive code/data alive while it is reachable.

    Called from the same path that emits fundecls for the function body (see
    [To_cmm_static.add_functions]) so every emitted function gets a matching
    [Code_block], regardless of whether the [Code_id] is recorded in [all_code]
    as [Code_present] or [Metadata_only]. (Simplification can rebuild a function
    body and store only its metadata while the actual body lives in the IR's
    [Static_const_or_code] list.)

    No-op for non-unloadable compilation units or non-unloadable functions. *)
val emit_code_block_for :
  all_code:Exported_code.t -> Code.t -> To_cmm_result.t -> To_cmm_result.t

(** [linkage_name code_id] is the linkage name of the [Code_block] static symbol
    associated with [code_id]. The function entry's linkage name is derived from
    [Code_id.linkage_name]; the [Code_block] symbol appends a fixed suffix so it
    can be referenced by both the runtime registration path and the per-function
    back-pointer (section D). *)
val linkage_name : Code_id.t -> string

(** Emit a [Code_block] for the module-initializer ("entry") function of an
    unloadable compilation unit. The entry has no Flambda 2 [Code_id] (it's
    emitted directly from the to_cmm driver), so its [Code_block] is constructed
    without going through the Flambda 2 dependency analysis — it has zero
    scannable fields. The Code_block exists purely so the back-pointer at
    [entry - 1] resolves; while the entry is on the stack the GC darkens this
    Code_block via the F.2 stack-RA scan, and after entry returns the Code_block
    becomes unmarkable so the unit can be unloaded if nothing else holds it.

    No-op for non-unloadable compilation units. *)
val emit_entry_code_block :
  entry_sym:Cmm.symbol -> To_cmm_result.t -> To_cmm_result.t
