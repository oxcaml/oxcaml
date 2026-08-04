(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Max Slater, Jane Street                         *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Types shared between the CSV-driven instruction generator ([simdgen.ml]) and
   the intrinsics generator ([simdgen_intrins.ml]). *)

open Amd64_simd_defs

type evex_rnd =
  | Rnd_none
  | Rnd_er
  | Rnd_sae

type evex_bcst =
  | Bcst_none
  | Bcst_32
  | Bcst_64

type evex_flags =
  { mutable z : bool; (* Supports zeroing *)
    mutable b : evex_bcst; (* Supports broadcasting *)
    mutable r : evex_rnd; (* Supports rounding *)
    mutable k : bool (* Expects write mask *)
  }

type instr_emit =
  { ext : ext array (* Multiple extensions may be required. *);
    args : arg array;
    res : res;
    imm : imm;
    mnemonic : string;
    enc : enc;
    flags : evex_flags
  }

exception Unsupported
