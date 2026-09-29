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
