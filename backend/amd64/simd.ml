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

open! Int_replace_polymorphic_compare [@@warning "-66"]
open Format
include Amd64_simd_defs

module Amd64_simd_instrs = struct
  include Amd64_simd_instrs

  let equal = Stdlib.( == )
end

type instr = Amd64_simd_instrs.instr

module Pcompare_string = struct
  type t =
    | Pcmpestra
    | Pcmpestrc
    | Pcmpestro
    | Pcmpestrs
    | Pcmpestrz
    | Pcmpistra
    | Pcmpistrc
    | Pcmpistro
    | Pcmpistrs
    | Pcmpistrz

  let equal t1 t2 =
    match t1, t2 with
    | Pcmpestra, Pcmpestra
    | Pcmpestrc, Pcmpestrc
    | Pcmpestro, Pcmpestro
    | Pcmpestrs, Pcmpestrs
    | Pcmpestrz, Pcmpestrz
    | Pcmpistra, Pcmpistra
    | Pcmpistrc, Pcmpistrc
    | Pcmpistro, Pcmpistro
    | Pcmpistrs, Pcmpistrs
    | Pcmpistrz, Pcmpistrz ->
      true
    | ( ( Pcmpestra | Pcmpestrc | Pcmpestro | Pcmpestrs | Pcmpestrz | Pcmpistra
        | Pcmpistrc | Pcmpistro | Pcmpistrs | Pcmpistrz ),
        _ ) ->
      false

  let mnemonic t =
    match t with
    | Pcmpestra -> "pcmpestra"
    | Pcmpestrc -> "pcmpestrc"
    | Pcmpestro -> "pcmpestro"
    | Pcmpestrs -> "pcmpestrs"
    | Pcmpestrz -> "pcmpestrz"
    | Pcmpistra -> "pcmpistra"
    | Pcmpistrc -> "pcmpistrc"
    | Pcmpistro -> "pcmpistro"
    | Pcmpistrs -> "pcmpistrs"
    | Pcmpistrz -> "pcmpistrz"
end

module Seq = struct
  type id =
    | Sqrtss
    | Sqrtsd
    | Roundss
    | Roundsd
    | Pcompare_string of Pcompare_string.t
    | Vpcompare_string of Pcompare_string.t
    | Ptestz
    | Ptestc
    | Ptestnzc
    | Vptestz
    | Vptestc
    | Vptestnzc

  type nonrec t =
    { id : id;
      instr : Amd64_simd_instrs.instr
    }

  let sqrtss = { id = Sqrtss; instr = Amd64_simd_instrs.sqrtss }

  let sqrtsd = { id = Sqrtsd; instr = Amd64_simd_instrs.sqrtsd }

  let roundss = { id = Roundss; instr = Amd64_simd_instrs.roundss }

  let roundsd = { id = Roundsd; instr = Amd64_simd_instrs.roundsd }

  let pcmpestra =
    { id = Pcompare_string Pcmpestra; instr = Amd64_simd_instrs.pcmpestri }

  let pcmpestrc =
    { id = Pcompare_string Pcmpestrc; instr = Amd64_simd_instrs.pcmpestri }

  let pcmpestro =
    { id = Pcompare_string Pcmpestro; instr = Amd64_simd_instrs.pcmpestri }

  let pcmpestrs =
    { id = Pcompare_string Pcmpestrs; instr = Amd64_simd_instrs.pcmpestri }

  let pcmpestrz =
    { id = Pcompare_string Pcmpestrz; instr = Amd64_simd_instrs.pcmpestri }

  let pcmpistra =
    { id = Pcompare_string Pcmpistra; instr = Amd64_simd_instrs.pcmpistri }

  let pcmpistrc =
    { id = Pcompare_string Pcmpistrc; instr = Amd64_simd_instrs.pcmpistri }

  let pcmpistro =
    { id = Pcompare_string Pcmpistro; instr = Amd64_simd_instrs.pcmpistri }

  let pcmpistrs =
    { id = Pcompare_string Pcmpistrs; instr = Amd64_simd_instrs.pcmpistri }

  let pcmpistrz =
    { id = Pcompare_string Pcmpistrz; instr = Amd64_simd_instrs.pcmpistri }

  let vpcmpestra =
    { id = Vpcompare_string Pcmpestra; instr = Amd64_simd_instrs.vpcmpestri }

  let vpcmpestrc =
    { id = Vpcompare_string Pcmpestrc; instr = Amd64_simd_instrs.vpcmpestri }

  let vpcmpestro =
    { id = Vpcompare_string Pcmpestro; instr = Amd64_simd_instrs.vpcmpestri }

  let vpcmpestrs =
    { id = Vpcompare_string Pcmpestrs; instr = Amd64_simd_instrs.vpcmpestri }

  let vpcmpestrz =
    { id = Vpcompare_string Pcmpestrz; instr = Amd64_simd_instrs.vpcmpestri }

  let vpcmpistra =
    { id = Vpcompare_string Pcmpistra; instr = Amd64_simd_instrs.vpcmpistri }

  let vpcmpistrc =
    { id = Vpcompare_string Pcmpistrc; instr = Amd64_simd_instrs.vpcmpistri }

  let vpcmpistro =
    { id = Vpcompare_string Pcmpistro; instr = Amd64_simd_instrs.vpcmpistri }

  let vpcmpistrs =
    { id = Vpcompare_string Pcmpistrs; instr = Amd64_simd_instrs.vpcmpistri }

  let vpcmpistrz =
    { id = Vpcompare_string Pcmpistrz; instr = Amd64_simd_instrs.vpcmpistri }

  let ptestz = { id = Ptestz; instr = Amd64_simd_instrs.ptest }

  let ptestc = { id = Ptestc; instr = Amd64_simd_instrs.ptest }

  let ptestnzc = { id = Ptestnzc; instr = Amd64_simd_instrs.ptest }

  let vptestz = { id = Vptestz; instr = Amd64_simd_instrs.vptest_r64_X_Xm128 }

  let vptestc = { id = Vptestc; instr = Amd64_simd_instrs.vptest_r64_X_Xm128 }

  let vptestnzc =
    { id = Vptestnzc; instr = Amd64_simd_instrs.vptest_r64_X_Xm128 }

  let mnemonic ({ id; _ } : t) =
    match id with
    | Sqrtss -> "sqrtss"
    | Sqrtsd -> "sqrtsd"
    | Roundss -> "roundss"
    | Roundsd -> "roundsd"
    | Pcompare_string p -> Pcompare_string.mnemonic p
    | Vpcompare_string p -> "v" ^ Pcompare_string.mnemonic p
    | Ptestz -> "ptestz"
    | Ptestc -> "ptestc"
    | Ptestnzc -> "ptestnzc"
    | Vptestz -> "vptestz"
    | Vptestc -> "vptestc"
    | Vptestnzc -> "vptestnzc"

  let equal { id = id0; instr = instr0 } { id = id1; instr = instr1 } =
    let return_true () =
      assert (Amd64_simd_instrs.equal instr0 instr1);
      true
    in
    match id0, id1 with
    | Sqrtss, Sqrtss
    | Sqrtsd, Sqrtsd
    | Roundss, Roundss
    | Roundsd, Roundsd
    | Ptestz, Ptestz
    | Ptestc, Ptestc
    | Ptestnzc, Ptestnzc
    | Vptestz, Vptestz
    | Vptestc, Vptestc
    | Vptestnzc, Vptestnzc ->
      return_true ()
    | Pcompare_string p1, Pcompare_string p2
    | Vpcompare_string p1, Vpcompare_string p2 ->
      if Pcompare_string.equal p1 p2 then return_true () else false
    | ( ( Sqrtss | Sqrtsd | Roundss | Roundsd | Pcompare_string _
        | Vpcompare_string _ | Ptestz | Ptestc | Ptestnzc | Vptestz | Vptestc
        | Vptestnzc ),
        _ ) ->
      false
end

module Pseudo_instr = struct
  type t =
    | Instruction of Amd64_simd_instrs.instr
    | Sequence of Seq.t

  let equal t1 t2 =
    match t1, t2 with
    | Instruction i0, Instruction i1 -> Amd64_simd_instrs.equal i0 i1
    | Sequence s0, Sequence s1 -> Seq.equal s0 s1
    | (Instruction _ | Sequence _), _ -> false

  let print ppf t =
    match t with
    | Instruction instr -> fprintf ppf "%s" instr.mnemonic
    | Sequence seq -> fprintf ppf "[seq] %s" (Seq.mnemonic seq)
end

type operation =
  { instr : Pseudo_instr.t;
    imm : int option
  }

let instruction instr imm = { instr = Pseudo_instr.Instruction instr; imm }

let sequence instr imm = { instr = Pseudo_instr.Sequence instr; imm }

type operation_class =
  | Pure
  | Load of { is_mutable : bool }

let is_pure_operation _op = true

let class_of_operation _op = Pure

let equal_operation { instr = instr0; imm = imm0 }
    { instr = instr1; imm = imm1 } =
  Pseudo_instr.equal instr0 instr1 && Option.equal Int.equal imm0 imm1

let print_operation printreg (op : operation) ppf regs =
  Pseudo_instr.print ppf op.instr;
  Option.iter (fun imm -> fprintf ppf " %d" imm) op.imm;
  Array.iter (fun reg -> fprintf ppf " %a" printreg reg) regs

module Mem = struct
  (** Initial support for some operations with memory arguments.
      Requires 16-byte aligned memory. *)

  type operation =
    | Add_f32
    | Sub_f32
    | Mul_f32
    | Div_f32
    | Add_f64
    | Sub_f64
    | Mul_f64
    | Div_f64

  let class_of_operation (op : operation) =
    match op with
    | Add_f32 | Sub_f32 | Mul_f32 | Div_f32 | Add_f64 | Sub_f64 | Mul_f64
    | Div_f64 ->
      Load { is_mutable = true }

  let op_name (op : operation) =
    match op with
    | Add_f32 -> "add_f32"
    | Sub_f32 -> "sub_f32"
    | Mul_f32 -> "mul_f32"
    | Div_f32 -> "div_f32"
    | Add_f64 -> "add_f64"
    | Sub_f64 -> "sub_f64"
    | Mul_f64 -> "mul_f64"
    | Div_f64 -> "div_f64"

  let print_operation printreg printaddr (op : operation) ppf arg =
    let addr_args = Array.sub arg 1 (Array.length arg - 1) in
    fprintf ppf "%s %a [%a]" (op_name op) printreg arg.(0) printaddr addr_args

  let is_pure_operation op =
    match class_of_operation op with Pure -> true | Load _ -> true

  let equal_operation (l : operation) (r : operation) =
    match l, r with
    | Add_f64, Add_f64
    | Sub_f64, Sub_f64
    | Mul_f64, Mul_f64
    | Div_f64, Div_f64
    | Add_f32, Add_f32
    | Sub_f32, Sub_f32
    | Mul_f32, Mul_f32
    | Div_f32, Div_f32 ->
      true
    | ( ( Add_f64 | Sub_f64 | Mul_f64 | Div_f64 | Add_f32 | Sub_f32 | Mul_f32
        | Div_f32 ),
        _ ) ->
      false
end
