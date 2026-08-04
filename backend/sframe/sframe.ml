(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(** Emission of SFrame v3 (Simple Frame format) stack trace information.

    See
    {{:https://sourceware.org/binutils/docs/sframe-spec.html} the SFrame
     specification}. *)

open Asm_targets
module D = Asm_directives
module L = Asm_label

type cfa_base_reg =
  | SP
  | FP

type fre =
  { fre_label : Asm_label.t;
    cfa_base : cfa_base_reg;
    cfa_offset : int;
    ra_offset : int option;
    fp_offset : int option
  }

(* SFrame v3 constants, pre-converted to the types needed for emission. See
   https://sourceware.org/binutils/docs/sframe-spec.html *)

let sframe_magic = Numbers.Uint16.of_nonnegative_int_exn 0xdee2

let sframe_version_3 = Numbers.Uint8.of_nonnegative_int_exn 3

(* Preamble flags *)
let sframe_f_fde_sorted = 0x1

let sframe_f_fde_func_start_pcrel = 0x4

(* ABI/arch identifiers *)
let sframe_abi_aarch64_endian_little = Numbers.Uint8.of_nonnegative_int_exn 2

let sframe_abi_amd64_endian_little = Numbers.Uint8.of_nonnegative_int_exn 3

(* FDE PC types (bit 4 of sfda_func_info) *)
let sframe_fde_pctype_inc = 0

(* FRE types (address size, bits 0-3 of sfda_func_info) *)
let sframe_fre_type_addr4 = 2

(* FRE dataword size (bits 5-6 of sfre_info) *)
let sframe_fre_dataword_2b = 1

(* FDE type (bits 0-4 of sfda_func_info2) *)
let sframe_fde_type_default = Numbers.Uint8.of_nonnegative_int_exn 0

(* v3 FDE: index (16 bytes) + attribute (5 bytes) = 21 bytes *)
let sframe_fde_size = 21

let zero_uint8 = Numbers.Uint8.of_nonnegative_int_exn 0

let zero_int8 = Numbers.Int8.of_int_exn 0

(* --- Internal types for collected data --- *)

type fde =
  { fun_symbol : Asm_symbol.t;
    fun_start_label : Asm_label.t;
    fun_end_label : Asm_label.t;
    fres : fre list;
    _contains_calls : bool
  }

(* --- Environment --- *)

type current_function =
  { cf_symbol : Asm_symbol.t;
    cf_start_label : Asm_label.t;
    cf_contains_calls : bool;
    mutable cf_fres : fre list
  }

type t =
  { mutable fdes : fde list;
    mutable current : current_function option
  }

let create () = { fdes = []; current = None }

let new_function t ~fun_symbol ~fun_start_label ~contains_calls =
  t.current
    <- Some
         { cf_symbol = fun_symbol;
           cf_start_label = fun_start_label;
           cf_contains_calls = contains_calls;
           cf_fres = []
         }

let add_fre t fre =
  match t.current with
  | None -> Misc.fatal_error "Sframe.add_fre: no current function"
  | Some cf -> cf.cf_fres <- fre :: cf.cf_fres

let end_function t ~fun_end_label =
  match t.current with
  | None -> Misc.fatal_error "Sframe.end_function: no current function"
  | Some cf ->
    let fde =
      { fun_symbol = cf.cf_symbol;
        fun_start_label = cf.cf_start_label;
        fun_end_label;
        fres = List.rev cf.cf_fres;
        _contains_calls = cf.cf_contains_calls
      }
    in
    t.fdes <- fde :: t.fdes;
    t.current <- None

(* --- FRE info byte encoding --- *)

(* Bit 0: fre_cfa_base_reg_id (0=SP, 1=FP) Bits 1-4: fre_dataword_count Bits
   5-6: fre_dataword_size Bit 7: fre_mangled_ra_p *)

let encode_fre_info ~cfa_base ~dataword_count =
  let base_reg_bit = match cfa_base with SP -> 0 | FP -> 1 in
  base_reg_bit lor (dataword_count lsl 1) lor (sframe_fre_dataword_2b lsl 5)
  |> Numbers.Uint8.of_nonnegative_int_exn

(* --- FDE info bytes encoding --- *)

(* sfda_func_info (byte 1): Bits 0-3: fre_type Bit 4: fde_pctype (0=INC, 1=MASK)
   Bit 5: pauth_key (AArch64 only) Bit 6: unused Bit 7: signal_p *)
let fde_func_info =
  sframe_fre_type_addr4 lor (sframe_fde_pctype_inc lsl 4)
  |> Numbers.Uint8.of_nonnegative_int_exn

(* sfda_func_info2 (byte 2): Bits 0-4: fde_type (0=DEFAULT, 1=FLEX) *)
let fde_func_info2 = sframe_fde_type_default

(* --- Compute FRE byte size --- *)

let fre_dataword_count (fre : fre) =
  1
  + (if Option.is_some fre.ra_offset then 1 else 0)
  + if Option.is_some fre.fp_offset then 1 else 0

let fre_size_in_bytes (fre : fre) =
  (* ADDR4 start address (4) + info byte (1) + dataword_count * 2
     (DATAWORD_2B) *)
  4 + 1 + (fre_dataword_count fre * 2)

(* --- Emit helpers --- *)

let sframe_section =
  Asm_section.Custom
    { names = [".sframe"];
      flags = Some "a";
      args = ["@progbits"];
      is_delayed = false
    }

let emit_int16 n = D.int16 (Numbers.Int16.of_int_exn n)

let emit_header ~abi_arch ~cfa_fixed_ra_offset ~flags ~num_fdes ~num_fres
    ~fre_total_len =
  (* Preamble: magic (2), version (1), flags (1) *)
  D.uint16 sframe_magic;
  D.uint8 sframe_version_3;
  D.uint8 flags;
  (* abi_arch (1), cfa_fixed_fp_offset (1), cfa_fixed_ra_offset (1), auxhdr_len
     (1) *)
  D.uint8 abi_arch;
  D.int8 zero_int8;
  D.int8 cfa_fixed_ra_offset;
  D.uint8 zero_uint8;
  (* num_fdes (4), num_fres (4) *)
  D.int32 (Int32.of_int num_fdes);
  D.int32 (Int32.of_int num_fres);
  (* fre_len (4) *)
  D.int32 (Int32.of_int fre_total_len);
  (* fdeoff (4): FDEs immediately follow the header. *)
  D.int32 0l;
  (* freoff (4): FREs follow FDEs. *)
  D.int32 (Int32.of_int (num_fdes * sframe_fde_size))

let emit_fde fre_offset (fde : fde) =
  (* --- FDE Index (16 bytes) --- *)
  (* sfdi_func_start_offset: 64-bit PC-relative from this field. We
     create a label associated with the sframe section that has the
     same name as the function's .text label so that
     [between_this_and_label_offset_64bit_expr] passes its section
     check while GAS resolves the cross-section reference via a
     relocation. *)
  let func_label_in_sframe =
    L.create_label_for_local_symbol sframe_section fde.fun_symbol
  in
  D.between_this_and_label_offset_64bit_expr ~upper:func_label_in_sframe
    ~offset_upper:Targetint.zero;
  (* sfdi_func_size (4) *)
  D.between_labels_32_bit ~upper:fde.fun_end_label ~lower:fde.fun_start_label ();
  (* sfdi_func_start_fre_off (4) *)
  D.int32 (Int32.of_int !fre_offset);
  (* --- FDE Attribute (5 bytes) --- *)
  (* sfda_func_num_fres (2) *)
  D.uint16 (Numbers.Uint16.of_nonnegative_int_exn (List.length fde.fres));
  (* sfda_func_info (1) *)
  D.uint8 fde_func_info;
  (* sfda_func_info2 (1) *)
  D.uint8 fde_func_info2;
  (* sfda_func_rep_size (1; 0 for PCTYPE_INC) *)
  D.uint8 zero_uint8;
  List.iter
    (fun fre -> fre_offset := !fre_offset + fre_size_in_bytes fre)
    fde.fres

let emit_fre ~fun_start_label (fre : fre) =
  (* sfre_start_address: offset from function start (4 bytes, ADDR4). Both
     labels are in .text. *)
  D.between_labels_32_bit ~upper:fre.fre_label ~lower:fun_start_label ();
  (* sfre_info *)
  D.uint8
    (encode_fre_info ~cfa_base:fre.cfa_base
       ~dataword_count:(fre_dataword_count fre));
  (* Data words (2 bytes each, DATAWORD_2B) *)
  emit_int16 fre.cfa_offset;
  Option.iter emit_int16 fre.ra_offset;
  Option.iter emit_int16 fre.fp_offset

(* --- Main emission entry point --- *)

let arch_info () =
  match Target_system.architecture () with
  | X86_64 ->
    Some (sframe_abi_amd64_endian_little, Some (Numbers.Int8.of_int_exn (-8)))
  | AArch64 -> Some (sframe_abi_aarch64_endian_little, None)
  | IA32 | ARM | POWER | Riscv ->
    (* SFrame is not defined for these architectures. *)
    None
  | Z ->
    (* SFrame is defined for Z but OxCaml does not support Z. *)
    None

let emit t =
  let fdes = List.rev t.fdes in
  match fdes, arch_info () with
  | [], _ | _, None -> ()
  | _ :: _, Some (abi_arch, cfa_fixed_ra_offset) ->
    let cfa_fixed_ra_offset =
      Option.value cfa_fixed_ra_offset ~default:zero_int8
    in
    let num_fdes = List.length fdes in
    let num_fres =
      List.fold_left (fun acc fde -> acc + List.length fde.fres) 0 fdes
    in
    let fre_total_len =
      List.fold_left
        (fun acc fde ->
          List.fold_left
            (fun acc fre -> acc + fre_size_in_bytes fre)
            acc fde.fres)
        0 fdes
    in
    let flags =
      sframe_f_fde_sorted lor sframe_f_fde_func_start_pcrel
      |> Numbers.Uint8.of_nonnegative_int_exn
    in
    D.switch_to_section sframe_section;
    emit_header ~abi_arch ~cfa_fixed_ra_offset ~flags ~num_fdes ~num_fres
      ~fre_total_len;
    let fre_offset = ref 0 in
    List.iter (emit_fde fre_offset) fdes;
    List.iter
      (fun (fde : fde) ->
        List.iter (emit_fre ~fun_start_label:fde.fun_start_label) fde.fres)
      fdes
