(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Max Slater, Jane Street                         *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The selection data for the AVX512 [caml_<intel-name>] builtins, which
   [Simd_selection] interprets. The table itself is generated into
   [Amd64_simd_intrins] by tools/simdgen/simdgen_intrins.ml. Both modules are
   compiled on every architecture, so they only refer to the arch-independent
   instruction descriptors. *)

[@@@ocaml.warning "+a-42"]

type instr = Amd64_simd_instrs.instr

type rounding = Amd64_simd_defs.evex_rounding

(* An instruction descriptor, possibly parameterized by EVEX zeroing. *)
type bind =
  | Instr of instr
  | Zeroing of (z:bool -> instr) * bool

type rnd_bind =
  | Rnd of (rnd:rounding -> instr)
  | Rnd_zeroing of (rnd:rounding -> z:bool -> instr) * bool

type sae_bind =
  | Sae of (sae:unit -> instr)
  | Sae_zeroing of (sae:unit -> z:bool -> instr) * bool

(* The instruction's immediate, taken from the leading constant arguments. *)
type imm =
  | No_imm
  | Imm of int (* one constant in [0, max] *)
  | Getmant of int * int
    (* two constants (interval, sign), packed as [sign lsl 2 lor interval] *)
  | Fixed of int (* no constant argument: implied by the intrinsic's name *)

(* The flag a KORTEST/KTEST flag reader tests. *)
type flag =
  | Zf
  | Cf

(* An instruction operand: one of the (non-constant) arguments, or a constant
   synthesized for the unmasked gathers and scatters. *)
type operand =
  | Value of int
  | All_ones_mask
  | Zero_vec128
  | Zero_vec256
  | Zero_vec512

(* [arity] is the number of non-constant arguments; [args.(n)] is the
   instruction's [n]th operand. *)
type t =
  | Register of
      { arity : int;
        imm : imm;
        bind : bind;
        args : operand array
      }
  | Embedded_rounding of
      { arity : int;
        bind : rnd_bind;
        cur : bind option;
        args : operand array
      }
    (* A leading rounding-control constant selects [bind] with a static rounding
       mode (8-11) or [cur] (4, _MM_FROUND_CUR_DIRECTION). *)
  | Suppress_all_exceptions of
      { arity : int;
        imm : imm;
        bind : sae_bind;
        cur : bind option;
        args : operand array
      }
    (* After [imm], a constant selects [bind] (8 or 12, _MM_FROUND_NO_EXC) or
       [cur] (4, _MM_FROUND_CUR_DIRECTION). *)
  | Flag_reader of
      { flag : flag;
        instr : instr
      }
  | Load of
      { arity : int;
        bind : bind;
        args : operand array
      }
  | Store of
      { arity : int;
        bind : bind;
        args : operand array
      }
  | Gather of
      { arity : int;
        instr : instr;
        args : operand array
      }
    (* A leading scale constant selects the addressing mode. *)
  | Scatter of
      { arity : int;
        instr : instr;
        args : operand array
      }
