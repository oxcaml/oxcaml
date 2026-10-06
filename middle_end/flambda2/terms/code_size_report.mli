(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Mark Shinwell, Jane Street Europe                *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Measurement of the estimated code size of every function in a compilation
    unit, with both code size models, for comparison against the machine code
    actually emitted. [dump] writes [<prefixname>.code_sizes.csv] with one line
    per function (plus the module initialiser) giving the linkage name of its
    code, its debuginfo, its v1 estimate and its v2 estimates for x86-64 and
    arm64. The estimates cover the function's own code only: the bodies of
    closures it defines are measured separately. See
    [tools/code_size_histogram.py] for how to use the output. *)
val dump :
  prefixname:string ->
  machine_width:Target_system.Machine_width.t ->
  Flambda_unit.t ->
  unit

(** A traversal of Flambda terms that visits each construct once; code bound in
    [Static_consts] is not entered (see [collect_code]). *)
type visitor =
  { named : Flambda.Named.t -> unit;
    let_cont : Continuation.t -> Flambda.Continuation_handler.t -> unit;
    apply : Apply_expr.t -> unit;
    apply_cont : Apply_cont_expr.t -> unit;
    switch : Switch_expr.t -> unit;
    invalid : unit -> unit
  }

val iter_expr : visitor -> Flambda.Expr.t -> unit

val iter_function_body :
  Code.t ->
  f:
    (return_continuation:Continuation.t ->
    exn_continuation:Continuation.t ->
    Flambda.Expr.t ->
    'a) ->
  'a

(** All pieces of code in the unit, including any bound inside function bodies.
*)
val collect_code : Flambda_unit.t -> Code.t Code_id.Map.t

(** The size of the callee of a direct call, when the call is sure to be inlined
    (a small function, a stub, or an [@inlined] call) and its body will thus
    replace the call. *)
val size_if_inlined :
  code_metadata:Code_metadata.t ->
  inlined:Inlined_attribute.t ->
  Code_size.t option

(** The size of a function body (or of the module initialiser) in the v1 and v2
    models; see the comment in the implementation. A direct call for which
    [inlined_callee_size] returns a size counts as that size, see
    [size_if_inlined]. *)
val measure :
  machine_width:Target_system.Machine_width.t ->
  function_slot_size:(Code_id.t -> int) ->
  inlined_callee_size:
    (Code_id.t -> inlined:Inlined_attribute.t -> Code_size.t option) ->
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  Flambda.Expr.t ->
  int * Code_size_v2.t
