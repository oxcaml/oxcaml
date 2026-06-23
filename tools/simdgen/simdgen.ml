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

(* Parses [amd64.csv] and outputs [Amd64_simd_defs.instr] definitions.

   [amd64.csv] was retrieved from https://github.com/GregoryComer/x86-csv
   (commit c638bbbaa17f0c81abaa7e84a968335c985542fa) and manually modified.
   Instruction data was originally derived from the Intel SDM Vol 2. *)

open Amd64_simd_defs
open Printf

type id = Dummy

exception Unsupported

let all_mnemonics = Hashtbl.create 1024

let all_instructions = Hashtbl.create 1024

let is_evex instr =
  match instr.enc.prefix with Evex _ -> true | Legacy _ | Vex _ -> false

let register instr =
  (match Hashtbl.find_opt all_mnemonics instr.mnemonic with
  | Some i -> Hashtbl.replace all_mnemonics instr.mnemonic (i + 1)
  | None -> Hashtbl.add all_mnemonics instr.mnemonic 1);
  Hashtbl.add all_instructions instr ()

let first_word str =
  match String.split_on_char ' ' str with
  | [fst] -> fst, ""
  | fst :: rest -> fst, String.concat " " rest
  | _ -> failwith str

let fail name part = failwith (name ^ " (" ^ part ^ ")")

let parse in_ =
  let rec one quote acc =
    match In_channel.input_char in_ with
    | None -> `Eof acc
    | Some '\n' -> `Eol acc
    | Some '"' when quote -> (
      match In_channel.input_char in_ with
      | None -> `Eof acc
      | Some ',' -> `One acc
      | Some '\n' -> `Eol acc
      | _ -> assert false)
    | Some ',' when not quote -> `One acc
    | Some '"' -> one true acc
    | Some c -> one quote (acc ^ String.make 1 c)
  in
  let rec line acc =
    match one false "" with
    | `Eof one -> `Eof (List.rev (one :: acc))
    | `Eol one -> `Line (List.rev (one :: acc))
    | `One one -> line (one :: acc)
  in
  let rec csv acc =
    match line [] with
    | `Line line -> csv (line :: acc)
    | `Eof line -> List.rev (line :: acc)
  in
  csv []

let strip_modifiers arg =
  let no_braces =
    let b = Buffer.create (String.length arg) in
    let depth = ref 0 in
    String.iter
      (fun c ->
        match c with
        | '{' -> incr depth
        | '}' -> if !depth > 0 then decr depth
        | c -> if !depth = 0 then Buffer.add_char b c)
      arg;
    Buffer.contents b
  in
  String.split_on_char '/' no_braces
  |> List.map String.trim
  |> List.filter (fun seg ->
      not (String.equal seg "" || Filename.check_suffix seg "bcst"))
  |> String.concat "/"

let rec parse_args mnemonic acc encs args imm res =
  let set_imm i =
    if !imm <> Imm_none then failwith mnemonic;
    imm := i
  in
  let set_res_arg () =
    match !res with
    | Res _ -> raise Unsupported
    | Res_none -> res := Arg [| List.length acc |]
    | Arg rr -> res := Arg (Array.append rr [| List.length acc |])
  in
  let set_res loc enc =
    match !res with
    | Arg _ -> raise Unsupported
    | Res_none -> res := Res [| { loc; enc } |]
    | Res rr -> res := Res (Array.append rr [| { loc; enc } |])
  in
  match args, encs with
  | [], _ -> List.rev acc
  | "" :: args, encs -> parse_args mnemonic acc encs args imm res
  | arg :: args, enc :: encs -> (
    let loc : loc option =
      match strip_modifiers arg with
      | "" -> raise Unsupported
      | "<RAX>" -> Some (Pin RAX)
      | "<RDI>" -> Some (Pin RDI)
      | "<RCX>" -> Some (Pin RCX)
      | "<RDX>" -> Some (Pin RDX)
      | "<XMM0>" -> Some (Pin XMM0)
      | "imm8" ->
        set_imm Imm_spec;
        None
      | "r8/m8" | "r/m8" -> Some (Temp [| R8; M8 |])
      | "r16/m16" | "r/m16" -> Some (Temp [| R16; M16 |])
      | "r32/m32" | "r/m32" -> Some (Temp [| R32; M32 |])
      | "r64/m64" | "r/m64" -> Some (Temp [| R64; M64 |])
      | "r32/m8" -> Some (Temp [| R32; M8 |])
      | "r32/m16" -> Some (Temp [| R32; M16 |])
      | "r8" -> Some (Temp [| R8 |])
      | "r16" -> Some (Temp [| R16 |])
      | "r32" | "r32a" | "r32b" -> Some (Temp [| R32 |])
      | "r64" | "r64a" | "r64b" | "reg" -> Some (Temp [| R64 |])
      | "reg/m8" -> Some (Temp [| R64; M8 |])
      | "reg/m16" -> Some (Temp [| R64; M16 |])
      | "reg/m32" -> Some (Temp [| R64; M32 |])
      | "reg/m64" -> Some (Temp [| R64; M64 |])
      | "m8" -> Some (Temp [| M8 |])
      | "m16" -> Some (Temp [| M16 |])
      | "m32" -> Some (Temp [| M32 |])
      | "m64" -> Some (Temp [| M64 |])
      | "m128" -> Some (Temp [| M128 |])
      | "m256" -> Some (Temp [| M256 |])
      | "m512" -> Some (Temp [| M512 |])
      | "mm" | "mm0" | "mm1" | "mm2" | "mm3" -> Some (Temp [| MM |])
      | "mm0/m8" | "mm1/m8" | "mm2/m8" | "mm3/m8" -> Some (Temp [| MM; M8 |])
      | "mm0/m16" | "mm1/m16" | "mm2/m16" | "mm3/m16" ->
        Some (Temp [| MM; M16 |])
      | "mm0/m32" | "mm1/m32" | "mm2/m32" | "mm3/m32" ->
        Some (Temp [| MM; M32 |])
      | "mm0/m64" | "mm1/m64" | "mm2/m64" | "mm3/m64" ->
        Some (Temp [| MM; M64 |])
      | "xmm" | "xmm0" | "xmm1" | "xmm2" | "xmm3" | "xmm4" ->
        Some (Temp [| XMM |])
      | "xmm0/m8" | "xmm1/m8" | "xmm2/m8" | "xmm3/m8" ->
        Some (Temp [| XMM; M8 |])
      | "xmm0/m16" | "xmm1/m16" | "xmm2/m16" | "xmm3/m16" ->
        Some (Temp [| XMM; M16 |])
      | "xmm0/m32" | "xmm1/m32" | "xmm2/m32" | "xmm3/m32" ->
        Some (Temp [| XMM; M32 |])
      | "xmm0/m64" | "xmm1/m64" | "xmm2/m64" | "xmm3/m64" ->
        Some (Temp [| XMM; M64 |])
      | "xmm0/m128" | "xmm1/m128" | "xmm2/m128" | "xmm3/m128" ->
        Some (Temp [| XMM; M128 |])
      | "vm32x" -> Some (Temp [| VM32X |])
      | "vm64x" -> Some (Temp [| VM64X |])
      | "vm32y" -> Some (Temp [| VM32Y |])
      | "vm64y" -> Some (Temp [| VM64Y |])
      | "vm32z" -> Some (Temp [| VM32Z |])
      | "vm64z" -> Some (Temp [| VM64Z |])
      | "ymm" | "ymm0" | "ymm1" | "ymm2" | "ymm3" | "ymm4" ->
        Some (Temp [| YMM |])
      | "ymm0/m256" | "ymm1/m256" | "ymm2/m256" | "ymm3/m256" ->
        Some (Temp [| YMM; M256 |])
      | "zmm" | "zmm0" | "zmm1" | "zmm2" | "zmm3" | "zmm4" ->
        Some (Temp [| ZMM |])
      | "zmm0/m512" | "zmm1/m512" | "zmm2/m512" | "zmm3/m512" ->
        Some (Temp [| ZMM; M512 |])
      | "k0" | "k1" | "k2" | "k3" | "k4" | "k5" | "k6" | "k7" ->
        Some (Temp [| K |])
      | "k0/m8" | "k1/m8" | "k2/m8" | "k3/m8" | "k4/m8" | "k5/m8" | "k6/m8"
      | "k7/m8" ->
        Some (Temp [| K; M8 |])
      | "k0/m16" | "k1/m16" | "k2/m16" | "k3/m16" | "k4/m16" | "k5/m16"
      | "k6/m16" | "k7/m16" ->
        Some (Temp [| K; M16 |])
      | "k0/m32" | "k1/m32" | "k2/m32" | "k3/m32" | "k4/m32" | "k5/m32"
      | "k6/m32" | "k7/m32" ->
        Some (Temp [| K; M32 |])
      | "k0/m64" | "k1/m64" | "k2/m64" | "k3/m64" | "k4/m64" | "k5/m64"
      | "k6/m64" | "k7/m64" ->
        Some (Temp [| K; M64 |])
      | arg -> fail mnemonic arg
    in
    let enc, rw = first_word enc in
    let enc =
      match String.trim enc with
      | "" -> raise Unsupported
      | "ModRM:reg" -> RM_r
      | "ModRM:r/m" -> RM_rm
      | "BaseReg" ->
        (* Vector address: always r/m, and only used for gathers. We set this
           operand as an output to assure it gets a distinct register. *)
        set_res_arg ();
        RM_rm
      | "VEX.vvvv" | "EVEX.vvvv" -> Vex_v
      | "NA" | "<XMM0>" | "<RAX>" | "<RDI>" | "<RCX>" | "<RDX>" | "implicit" ->
        Implicit
      | "Imm8" | "imm8" | "imm8[3:0]" | "imm8[7:4]" ->
        if Option.is_some loc
        then (
          set_imm Imm_reg;
          Immediate)
        else Implicit
      | enc -> fail mnemonic enc
    in
    match loc with
    | None -> parse_args mnemonic acc encs args imm res
    | Some loc ->
      let rw = String.trim rw in
      let starts p = String.starts_with rw ~prefix:p in
      if starts "(r,w)" || starts "(r, w)"
      then (
        set_res_arg ();
        parse_args mnemonic ({ loc; enc } :: acc) encs args imm res)
      else if starts "(w"
      then (
        set_res loc enc;
        parse_args mnemonic acc encs args imm res)
      else parse_args mnemonic ({ loc; enc } :: acc) encs args imm res)
  | _ -> failwith mnemonic

let contains s sub =
  let n = String.length sub and m = String.length s in
  let rec go i =
    i + n <= m && (String.equal (String.sub s i n) sub || go (i + 1))
  in
  go 0

(* EVEX decorators detected on the operand strings, used to expand a single CSV
   line into separate definitions per modifier / zeroing. *)
type evex_info =
  { broadcast : temp option; (* broadcast element width, if any *)
    has_sae : bool;
    has_er : bool;
    evex_z : bool (* whether zeroing is permitted *)
  }

let parse_args mnemonic enc args =
  let imm = ref Imm_none in
  let res = ref Res_none in
  let parsed = parse_args mnemonic [] enc args imm res in
  let has_mask = List.exists (fun a -> contains a "{k") args in
  let parsed =
    if has_mask then parsed @ [{ loc = Temp [| K |]; enc = Mask }] else parsed
  in
  let any sub = List.exists (fun a -> contains a sub) args in
  let broadcast =
    if any "m32bcst" then Some M32 else if any "m64bcst" then Some M64 else None
  in
  let info =
    { broadcast;
      has_sae = any "{sae}";
      has_er = any "{er}";
      evex_z = any "{z}"
    }
  in
  Array.of_list parsed, !imm, !res, info

let parse_enc mnemonic enc ~operand_size_override =
  let enc = String.uppercase_ascii enc in
  let parse_opcode_rm_reg enc =
    let opcode, rest =
      let opcode, rest = first_word enc in
      match Int64.of_string_opt ("0x" ^ opcode) with
      | Some i -> Int64.to_int i, rest
      | None -> fail mnemonic enc
    in
    let rm_reg =
      let rm_reg, _ = first_word rest in
      match rm_reg with
      | "/0" -> Spec 0
      | "/1" -> Spec 1
      | "/2" -> Spec 2
      | "/3" -> Spec 3
      | "/4" -> Spec 4
      | "/5" -> Spec 5
      | "/6" -> Spec 6
      | "/7" -> Spec 7
      | "/R" | "/VSIB" | "" -> Reg
      | _ -> fail mnemonic enc
    in
    opcode, rm_reg
  in
  let parse_legacy () =
    let prefix, rest =
      let prefix, rest = first_word enc in
      match prefix with
      | "NP" -> Prx_none, rest
      | "66" -> Prx_66, rest
      | "F2" -> Prx_F2, rest
      | "F3" -> Prx_F3, rest
      | prefix -> fail mnemonic prefix
    in
    let rex, rest =
      let rex, rest' = first_word rest in
      match rex with
      | "REX" -> Rex, rest'
      | "REX.W" -> Rex_w, rest'
      | _ -> Rex_none, rest
    in
    let escape, rest =
      let escape, rest' = first_word rest in
      match escape with
      | "0F" -> (
        let escape, rest'' = first_word rest' in
        match escape with
        | "38" -> Esc_0F38, rest''
        | "3A" -> Esc_0F3A, rest''
        | _ -> Esc_0F, rest')
      | _ -> Esc_none, rest
    in
    let opcode, rm_reg = parse_opcode_rm_reg rest in
    { prefix = Legacy { prefix; escape; rex; operand_size_override };
      rm_reg;
      opcode
    }
  in
  let parse_vex () =
    let prefix, rest = first_word enc in
    let prefix =
      let comps = String.split_on_char '.' prefix |> List.tl in
      let comps =
        match comps with
        | ("NDS" | "NDD" | "DDS") :: comps -> comps
        | _ -> comps
      in
      let vex_l, comps =
        match comps with
        | ("LZ" | "LIG" | "128" | "L0") :: comps -> false, comps
        | ("256" | "L1") :: comps -> true, comps
        | _ -> fail mnemonic enc
      in
      let vex_p, comps =
        match comps with
        | "66" :: comps -> Prx_66, comps
        | "F2" :: comps -> Prx_F2, comps
        | "F3" :: comps -> Prx_F3, comps
        | comps -> Prx_none, comps
      in
      let vex_m, comps =
        match comps with
        | "0F" :: comps -> Vexm_0F, comps
        | "0F38" :: comps -> Vexm_0F38, comps
        | "0F3A" :: comps -> Vexm_0F3A, comps
        | _ -> fail mnemonic enc
      in
      let vex_w =
        match comps with
        | [("W0" | "WIG")] | [] -> false
        | ["W1"] -> true
        | _ -> fail mnemonic enc
      in
      Vex { vex_m; vex_w; vex_l; vex_p }
    in
    let opcode, rm_reg = parse_opcode_rm_reg rest in
    { prefix; rm_reg; opcode }
  in
  let parse_evex () =
    let prefix, rest = first_word enc in
    let prefix =
      let comps = String.split_on_char '.' prefix |> List.tl in
      let comps =
        match comps with
        | ("NDS" | "NDD" | "DDS") :: comps -> comps
        | _ -> comps
      in
      let evex_l, comps =
        match comps with
        | ("LIG" | "LLIG" | "LZ" | "128" | "L0") :: comps -> L128, comps
        | ("256" | "L1") :: comps -> L256, comps
        | "512" :: comps -> L512, comps
        | _ -> fail mnemonic enc
      in
      let evex_p, comps =
        match comps with
        | "66" :: comps -> Prx_66, comps
        | "F2" :: comps -> Prx_F2, comps
        | "F3" :: comps -> Prx_F3, comps
        | comps -> Prx_none, comps
      in
      let evex_m, comps =
        match comps with
        | "0F" :: comps -> Vexm_0F, comps
        | "0F38" :: comps -> Vexm_0F38, comps
        | "0F3A" :: comps -> Vexm_0F3A, comps
        | _ -> fail mnemonic enc
      in
      let evex_w =
        match comps with
        | [("W0" | "WIG")] | [] -> false
        | ["W1"] -> true
        | _ -> fail mnemonic enc
      in
      Evex
        { evex_m; evex_w; evex_bll = Bll_length evex_l; evex_p; evex_z = false }
    in
    let opcode, rm_reg = parse_opcode_rm_reg rest in
    { prefix; rm_reg; opcode }
  in
  if String.starts_with enc ~prefix:"EVEX"
  then parse_evex ()
  else if String.starts_with enc ~prefix:"VEX"
  then parse_vex ()
  else parse_legacy ()

let mangle_loc (loc : loc) =
  let width : temp -> int option = function
    | R8 | M8 -> Some 8
    | R16 | M16 -> Some 16
    | R32 | M32 -> Some 32
    | R64 | M64 -> Some 64
    | M128 -> Some 128
    | M256 -> Some 256
    | M512 -> Some 512
    | MM | XMM | YMM | ZMM | K | VM32X | VM32Y | VM32Z | VM64X | VM64Y | VM64Z
      ->
      None
  in
  let short : temp -> string = function
    | R8 | R16 | R32 | R64 -> "r"
    | M8 | M16 | M32 | M64 | M128 | M256 | M512 -> "m"
    | MM -> "M"
    | XMM -> "X"
    | YMM -> "Y"
    | ZMM -> "Z"
    | K -> "K"
    | VM32X -> "M32X"
    | VM32Y -> "M32Y"
    | VM32Z -> "M32Z"
    | VM64X -> "M64X"
    | VM64Y -> "M64Y"
    | VM64Z -> "M64Z"
  in
  match loc with
  | Pin RAX -> "rax"
  | Pin RDI -> "rdi"
  | Pin RCX -> "rcx"
  | Pin RDX -> "rdx"
  | Pin XMM0 -> "xmm0"
  | Temp temps ->
    Array.map
      (fun temp ->
        match width temp with
        | Some width -> short temp ^ Int.to_string width
        | None -> short temp)
      temps
    |> Array.to_list |> String.concat ""

let evex_suffix instr =
  match instr.enc.prefix with
  | Legacy _ | Vex _ -> ""
  | Evex { evex_bll; evex_z; _ } ->
    let m =
      match evex_bll with
      | Bll_length _ -> ""
      | Bll_broadcast _ -> "_bcst"
      | Bll_sae -> "_sae"
      | Bll_round Rnd_near -> "_rne"
      | Bll_round Rnd_down -> "_rd"
      | Bll_round Rnd_up -> "_ru"
      | Bll_round Rnd_zero -> "_rz"
    in
    m ^ if evex_z then "_z" else ""

let binding instr =
  let base = instr.mnemonic in
  let mangled () =
    let args =
      Array.map (fun (arg : arg) -> mangle_loc arg.loc) instr.args
      |> Array.to_list |> String.concat "_"
    in
    let res =
      match instr.res with
      | Res_none | Arg _ -> ""
      | Res rr ->
        Array.fold_left (fun acc { loc; _ } -> acc ^ mangle_loc loc ^ "_") "" rr
    in
    base ^ "_" ^ res ^ args ^ evex_suffix instr
  in
  let variants = Hashtbl.find all_mnemonics base in
  if variants > 1 then mangled () else base

let print_one bind instr =
  let print_ext : ext -> string = function
    | SSE -> "SSE"
    | SSE2 -> "SSE2"
    | SSE3 -> "SSE3"
    | SSSE3 -> "SSSE3"
    | SSE4_1 -> "SSE4_1"
    | SSE4_2 -> "SSE4_2"
    | POPCNT -> "POPCNT"
    | LZCNT -> "LZCNT"
    | PCLMULQDQ -> "PCLMULQDQ"
    | BMI -> "BMI"
    | BMI2 -> "BMI2"
    | AVX -> "AVX"
    | AVX2 -> "AVX2"
    | F16C -> "F16C"
    | FMA -> "FMA"
    | AVX512F -> "AVX512F"
    | AVX512DQ -> "AVX512DQ"
    | AVX512CD -> "AVX512CD"
    | AVX512BW -> "AVX512BW"
    | AVX512VL -> "AVX512VL"
  in
  let print_temp : temp -> string = function
    | R8 -> "R8"
    | R16 -> "R16"
    | R32 -> "R32"
    | R64 -> "R64"
    | M8 -> "M8"
    | M16 -> "M16"
    | M32 -> "M32"
    | M64 -> "M64"
    | M128 -> "M128"
    | M256 -> "M256"
    | M512 -> "M512"
    | MM -> "MM"
    | XMM -> "XMM"
    | YMM -> "YMM"
    | ZMM -> "ZMM"
    | K -> "K"
    | VM32X -> "VM32X"
    | VM32Y -> "VM32Y"
    | VM32Z -> "VM32Z"
    | VM64X -> "VM64X"
    | VM64Y -> "VM64Y"
    | VM64Z -> "VM64Z"
  in
  let print_loc : loc -> string = function
    | Pin RAX -> "Pin RAX"
    | Pin RDI -> "Pin RDI"
    | Pin RCX -> "Pin RCX"
    | Pin RDX -> "Pin RDX"
    | Pin XMM0 -> "Pin XMM0"
    | Temp temps ->
      let temps =
        Array.map print_temp temps |> Array.to_list |> String.concat ";"
      in
      "Temp [|" ^ temps ^ "|]"
  in
  let print_arg_enc = function
    | RM_r -> "RM_r"
    | RM_rm -> "RM_rm"
    | Vex_v -> "Vex_v"
    | Mask -> "Mask"
    | Implicit -> "Implicit"
    | Immediate -> "Immediate"
  in
  let print_imm = function
    | Imm_none -> "Imm_none"
    | Imm_reg -> "Imm_reg"
    | Imm_spec -> "Imm_spec"
  in
  let print_args args =
    Array.map
      (fun (arg : arg) ->
        sprintf "{ loc = %s; enc = %s }" (print_loc arg.loc)
          (print_arg_enc arg.enc))
      args
    |> Array.to_list |> String.concat ";"
  in
  let print_idxs idxs =
    Array.map Int.to_string idxs |> Array.to_list |> String.concat ";"
  in
  let print_res : res -> string = function
    | Res_none -> "Res_none"
    | Arg rr -> sprintf "Arg [|%s|]" (print_idxs rr)
    | Res rr -> sprintf "Res [|%s|]" (print_args rr)
  in
  let print_legacy_prefix : legacy_prefix -> string = function
    | Prx_none -> "Prx_none"
    | Prx_66 -> "Prx_66"
    | Prx_F2 -> "Prx_F2"
    | Prx_F3 -> "Prx_F3"
  in
  let print_legacy_rex : legacy_rex -> string = function
    | Rex_none -> "Rex_none"
    | Rex -> "Rex"
    | Rex_w -> "Rex_w"
  in
  let print_legacy_escape : legacy_escape -> string = function
    | Esc_none -> "Esc_none"
    | Esc_0F -> "Esc_0F"
    | Esc_0F38 -> "Esc_0F38"
    | Esc_0F3A -> "Esc_0F3A"
  in
  let print_vex_map : vex_map -> string = function
    | Vexm_0F -> "Vexm_0F"
    | Vexm_0F38 -> "Vexm_0F38"
    | Vexm_0F3A -> "Vexm_0F3A"
  in
  let print_evex_length : evex_length -> string = function
    | L128 -> "L128"
    | L256 -> "L256"
    | L512 -> "L512"
  in
  let print_evex_bll : evex_bll -> string = function
    | Bll_length l -> sprintf "Bll_length %s" (print_evex_length l)
    | Bll_broadcast l -> sprintf "Bll_broadcast %s" (print_evex_length l)
    | Bll_sae -> "Bll_sae"
    | Bll_round Rnd_near -> "Bll_round Rnd_near"
    | Bll_round Rnd_down -> "Bll_round Rnd_down"
    | Bll_round Rnd_up -> "Bll_round Rnd_up"
    | Bll_round Rnd_zero -> "Bll_round Rnd_zero"
  in
  let print_prefix : prefix -> string = function
    | Legacy { prefix; rex; escape; operand_size_override } ->
      sprintf
        "Legacy { prefix = %s; rex = %s; escape = %s; operand_size_override = \
         %b }"
        (print_legacy_prefix prefix)
        (print_legacy_rex rex)
        (print_legacy_escape escape)
        operand_size_override
    | Vex { vex_m; vex_w; vex_l; vex_p } ->
      sprintf "Vex { vex_m = %s; vex_w = %b; vex_l = %b; vex_p = %s }"
        (print_vex_map vex_m) vex_w vex_l
        (print_legacy_prefix vex_p)
    | Evex { evex_m; evex_w; evex_bll; evex_p; evex_z } ->
      sprintf
        "Evex { evex_m = %s; evex_w = %b; evex_bll = %s; evex_p = %s; evex_z = \
         %b }"
        (print_vex_map evex_m) evex_w (print_evex_bll evex_bll)
        (print_legacy_prefix evex_p)
        evex_z
  in
  let print_rm_reg : rm_reg -> string = function
    | Reg -> "Reg"
    | Spec s -> "Spec " ^ Int.to_string s
  in
  let print_enc { prefix = p; rm_reg = r; opcode } =
    sprintf "{ prefix = %s; rm_reg = %s; opcode = %d }" (print_prefix p)
      (print_rm_reg r) opcode
  in
  let constructor = String.capitalize_ascii bind in
  let ext =
    Array.map print_ext instr.ext |> Array.to_list |> String.concat ";"
  in
  let args = print_args instr.args in
  let res = print_res instr.res in
  let imm = print_imm instr.imm in
  let enc = print_enc instr.enc in
  printf
    {|
let %s = {
    id = %s
  ; ext = [|%s|]
  ; args = [|%s|]
  ; res = %s
  ; imm = %s
  ; mnemonic = "%s"
  ; enc = %s
}|}
    bind constructor ext args res imm instr.mnemonic enc

let print_all () =
  let module Map = Map.Make (String) in
  (* A plain unmasked EVEX form can share its binding with a VEX/legacy form
     that does the same thing (e.g. [vmovlhps]); keep the non-EVEX one, which
     has the shorter encoding. *)
  let all =
    Hashtbl.to_seq_keys all_instructions
    |> Seq.fold_left
         (fun acc instr ->
           Map.update (binding instr)
             (function
               | Some other when is_evex instr && not (is_evex other) ->
                 Some other
               | Some _ | None -> Some instr)
             acc)
         Map.empty
  in
  print_endline "type id = ";
  Map.iter (fun bind _ -> printf "  | %s\n" (String.capitalize_ascii bind)) all;
  print_endline "\ntype nonrec instr = id instr";
  Map.iter (fun bind instr -> print_one bind instr) all

let parse_ext_token = function
  | "SSE" -> Some SSE
  | "SSE2" -> Some SSE2
  | "SSE3" -> Some SSE3
  | "SSSE3" -> Some SSSE3
  | "SSE4_1" -> Some SSE4_1
  | "SSE4_2" -> Some SSE4_2
  | "POPCNT" -> Some POPCNT
  | "LZCNT" -> Some LZCNT
  | "PCLMULQDQ" -> Some PCLMULQDQ
  | "BMI1" -> Some BMI
  | "BMI2" -> Some BMI2
  | "AVX" -> Some AVX
  | "AVX2" -> Some AVX2
  | "F16C" -> Some F16C
  | "FMA" -> Some FMA
  | "AVX512F" -> Some AVX512F
  | "AVX512DQ" -> Some AVX512DQ
  | "AVX512CD" -> Some AVX512CD
  | "AVX512BW" -> Some AVX512BW
  | "AVX512VL" -> Some AVX512VL
  | _ -> None

let parse_ext ext =
  let toks =
    String.split_on_char ' ' ext
    |> List.filter (fun t -> not (String.equal t ""))
  in
  match toks with
  | [] -> None
  | _ ->
    let parsed = List.map parse_ext_token toks in
    if List.for_all Option.is_some parsed
    then Some (Array.of_list (List.map Option.get parsed))
    else None

let arg_has_int16 arg =
  match arg.loc with
  | Pin _ -> false
  | Temp temps ->
    Array.exists
      (function
        | R16 -> true
        | R8 | R32 | R64 | M8 | M16 | M32 | M64 | M128 | M256 | M512 | MM | XMM
        | YMM | ZMM | K | VM32X | VM32Y | VM32Z | VM64X | VM64Y | VM64Z ->
          false)
      temps

let reg_only = function
  | Pin _ as loc -> loc
  | Temp temps ->
    Temp (Array.of_list (List.filter temp_is_reg (Array.to_list temps)))

let evex_variant base evex_bll source evex_z =
  let prefix =
    match base.enc.prefix with
    | Evex e -> Evex { e with evex_bll; evex_z }
    | (Legacy _ | Vex _) as p -> p
  in
  let args =
    Array.map
      (fun (arg : arg) ->
        match arg.enc, source with
        | RM_rm, `Broadcast elt -> { arg with loc = Temp [| elt |] }
        | RM_rm, `Reg -> { arg with loc = reg_only arg.loc }
        | _ -> arg)
      base.args
  in
  { base with args; enc = { base.enc with prefix } }

(* Expand an instruction into one definition per EVEX modifier / zeroing combo.
   Non-EVEX instructions are left as-is. *)
let expand_evex base info =
  match base.enc.prefix with
  | Legacy _ | Vex _ -> [base]
  | Evex { evex_bll; _ } ->
    let length =
      match evex_bll with
      | Bll_length l -> l
      | Bll_broadcast _ | Bll_sae | Bll_round _ -> L128
    in
    let variants =
      (Bll_length length, `Keep)
      ::
      (match info.broadcast with
      | Some elt -> [Bll_broadcast length, `Broadcast elt]
      | None -> [])
      @ (if info.has_sae then [Bll_sae, `Reg] else [])
      @
      if info.has_er
      then
        [ Bll_round Rnd_near, `Reg;
          Bll_round Rnd_down, `Reg;
          Bll_round Rnd_up, `Reg;
          Bll_round Rnd_zero, `Reg ]
      else []
    in
    let zeroings = if info.evex_z then [false; true] else [false] in
    List.concat_map
      (fun (evex_bll, source) ->
        List.map
          (fun evex_z -> evex_variant base evex_bll source evex_z)
          zeroings)
      variants

let amd64 () =
  let csv = In_channel.with_open_text "amd64/amd64.csv" parse in
  let lines =
    csv
    |> List.concat_map (function
      | mnemonic :: enc :: ext :: encs -> (
        try
          match parse_ext ext with
          | Some ext ->
            let mnemonic, args = first_word mnemonic in
            let mnemonic = String.lowercase_ascii mnemonic in
            let args, imm, res, info =
              String.split_on_char ',' args |> parse_args mnemonic encs
            in
            let enc =
              parse_enc mnemonic enc
                ~operand_size_override:(Array.exists arg_has_int16 args)
            in
            expand_evex { id = Dummy; ext; args; res; imm; mnemonic; enc } info
          | None -> []
        with Unsupported -> [])
      | _ -> [])
  in
  print_endline "(* Generated by tools/simdgen/simdgen.ml *)\n";
  print_endline "open Amd64_simd_defs\n";
  List.iter register lines;
  print_all ()

let arm64 () = print_endline "(* Generated by tools/simdgen/simdgen.ml *)\n"

let () =
  match Sys.argv.(1) with
  | "amd64" -> amd64 ()
  | "arm64" -> arm64 ()
  | _ -> assert false
