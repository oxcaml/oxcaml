(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Max Slater, Jane Street                           *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
[@@@ocaml.warning "+a-40-42"]

(* SIMD register behavior for ARM64 *)

open! Int_replace_polymorphic_compare [@@warning "-66"]

(* Convert Simd.Cond.t to Arm64_ast.Ast.Cond.t *)
let simd_cond_to_ast_cond (c : Simd.Cond.t) : Arm64_ast.Ast.Cond.t =
  match c with EQ -> EQ | GE -> GE | GT -> GT | LE -> LE | LT -> LT

(* Convert Simd.Rounding_mode.t to Arm64_ast.Ast.Rounding_mode.t *)
let simd_rounding_to_ast_rounding (rm : Simd.Rounding_mode.t) :
    Arm64_ast.Ast.Rounding_mode.t =
  match rm with
  | Current -> X (* Use current FPCR mode *)
  | Neg_inf -> M
  | Pos_inf -> P
  | Zero -> Z
  | Nearest -> N
