(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                    Greta Yorsh, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Format of .cmir-cfg files *)

(* Compiler can optionally save Cfg representation of a compilation unit,
   along with other information required to emit assembly. *)
type cfg_item_info =
  | Cfg of Cfg_with_layout.t
  | Data of Cmm.data_item list
  | Cfg_before_regalloc of {
    cfg_with_layout_and_relocatable_regs : Cfg_with_layout.t * (Reg.t list);
    cmm_label: Label.t;
    reg_stamp: int;
  }

type cfg_unit_info =
  {
    mutable unit : Compilation_unit.t;
    mutable items : cfg_item_info list;
  }

type error =
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string
  | Marshal_failed of string
  | Configuration_mismatch of Ir_config_fingerprint.configuration_mismatch

exception Error of error

(* Marshal and unmarshal a compilation unit in Cfg format.
   It includes saving and restoring global state required for Emit,
   that currently consists of Cmm.label_counter.
   [restore] raises [Error (Configuration_mismatch _)] for a file that was
   saved under another configuration than that of the current process (see
   [Ir_config_fingerprint]).
*)
val save : string -> cfg_unit_info -> unit
val restore : string -> cfg_unit_info * Digest.t
