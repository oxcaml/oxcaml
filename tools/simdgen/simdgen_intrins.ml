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

(* Generates the selection table for the AVX512 [caml_<intel-name>] builtins,
   and C-oracle tests for them, by matching the Intel Intrinsics Guide data
   ([x86-intel.xml]) to the instruction descriptors parsed by [simdgen.ml]. *)

open Amd64_simd_defs
open Simdgen_types
open Printf

let required = function Some x -> x | None -> raise Unsupported

let substring_index ~needle s =
  let n = String.length needle in
  let rec go i =
    if i + n > String.length s
    then None
    else if String.equal (String.sub s i n) needle
    then Some i
    else go (i + 1)
  in
  go 0

let contains ~needle s = Option.is_some (substring_index ~needle s)

(* XML *)

type param =
  { etype : string;
    ctype : string;
    immwidth : string option;
    immtype : string option;
    varname : string
  }

type instruction =
  { mnemonic : string;
    form : string
  }

type intrinsic =
  { name : string;
    tech : string;
    sequence : bool;
    cpuid : string list;
    ret : param;
    params : param list;
    instrs : instruction list
  }

let attr node name = Option.value ~default:"" (Simdgen_xml.attr_opt node name)

let param_of_node node =
  { etype = attr node "etype";
    ctype = Simdgen_xml.attr node "type";
    immwidth = Simdgen_xml.attr_opt node "immwidth";
    immtype = Simdgen_xml.attr_opt node "immtype";
    varname = attr node "varname"
  }

let intrinsic_of_node node =
  let children name f = List.map f (Simdgen_xml.children_named node name) in
  { name = Simdgen_xml.attr node "name";
    tech = Simdgen_xml.attr node "tech";
    sequence = Simdgen_xml.attr_opt node "sequence" = Some "TRUE";
    cpuid = children "CPUID" (fun n -> n.Simdgen_xml.text);
    ret = param_of_node (Simdgen_xml.child_named node "return");
    params =
      children "parameter" param_of_node
      |> List.filter (fun p -> not (String.equal p.ctype "void"));
    instrs =
      children "instruction" (fun n ->
          { mnemonic = String.lowercase_ascii (Simdgen_xml.attr n "name");
            form = attr n "form"
          })
  }

let parse_intrinsics path =
  Simdgen_xml.children_named (Simdgen_xml.parse_file path) "intrinsic"
  |> List.map intrinsic_of_node

let in_scope i =
  i.cpuid <> []
  && List.for_all
       (fun c ->
         List.mem c ["AVX512F"; "AVX512DQ"; "AVX512CD"; "AVX512BW"; "AVX512VL"])
       i.cpuid

let caml_name i = "caml" ^ i.name

let is_imm p = String.equal p.etype "IMM"

let is_ptr p = String.contains p.ctype '*'

(* [__mmaskN*] pointers also have the MASK etype. *)
let is_mask p = String.equal p.etype "MASK" && not (is_ptr p)

let values i = List.filter (fun p -> not (is_imm p)) i.params

let imm_max p =
  match p.immtype with
  | Some "_CMP_" -> 31
  | Some "_MM_CMPINT" -> 7
  | Some "_MM_PERM" -> 255
  | Some "_MM_MANTISSA_NORM" -> 3
  | Some "_MM_MANTISSA_SIGN" -> 2
  | _ -> (
    match p.immwidth with Some w -> (1 lsl int_of_string w) - 1 | None -> 255)

(* Operand classes: vector and GPR widths, masks, VSIB operands, and memory
   (whose XML widths are unreliable). *)

let temp_class : temp -> string option = function
  | ZMM -> Some "ZMM"
  | YMM -> Some "YMM"
  | XMM -> Some "XMM"
  | K -> Some "K"
  | MM -> Some "MM"
  | R8 -> Some "R8"
  | R16 -> Some "R16"
  | R32 -> Some "R32"
  | R64 -> Some "R64"
  | VM32X -> Some "VM32X"
  | VM32Y -> Some "VM32Y"
  | VM32Z -> Some "VM32Z"
  | VM64X -> Some "VM64X"
  | VM64Y -> Some "VM64Y"
  | VM64Z -> Some "VM64Z"
  | M8 | M16 | M32 | M64 | M128 | M256 | M512 -> None

let loc_class : loc -> string option = function
  | Pin _ -> Some "R64"
  | Temp temps -> Array.find_map temp_class temps

let ctype_class ctype =
  List.find_map
    (fun (prefix, c) ->
      if String.starts_with ~prefix (String.trim ctype) then Some c else None)
    ["__m512", "ZMM"; "__m256", "YMM"; "__m128", "XMM"; "__mmask", "K"]

(* "zmm {k}, zmm, m512 {er}" *)
type shape =
  { classes : string list;
    masked : bool;
    zeroing : bool;
    rnd : bool;
    sae : bool
  }

let shape_of_form form =
  let tokens =
    String.split_on_char ',' form
    |> List.map (fun tok ->
        match String.index_opt tok '{' with
        | Some i -> String.trim (String.sub tok 0 i)
        | None -> String.trim tok)
  in
  let class_ = function
    | "xmm" -> Some "XMM"
    | "ymm" -> Some "YMM"
    | "zmm" -> Some "ZMM"
    | "k" -> Some "K"
    | "m8" | "m16" | "m32" | "m64" | "m128" | "m256" | "m512" -> Some "MEM"
    | "r8" | "r16" -> Some "GPRlo"
    | "r32" -> Some "R32"
    | "r64" -> Some "R64"
    | "imm8" -> None
    | t when String.starts_with ~prefix:"vm" t -> Some "VSIB"
    | other -> Some ("?" ^ other)
  in
  let mods =
    String.split_on_char '{' form
    |> List.tl
    |> List.map (fun m -> String.sub m 0 (String.index m '}'))
  in
  { classes = List.filter_map class_ tokens;
    masked = List.mem "k" mods || List.mem "z" mods;
    zeroing = List.mem "z" mods;
    rnd = List.mem "er" mods;
    sae = List.mem "sae" mods
  }

(* Bindings *)

type binding =
  { bname : string;
    instr : instr_emit
  }

let build_by_mnem bindings =
  let tbl = Hashtbl.create 1024 in
  List.iter
    (fun (bname, (instr : instr_emit)) ->
      Hashtbl.add tbl instr.mnemonic { bname; instr })
    bindings;
  tbl

let bindings_of ~by_mnem mnemonic = Hashtbl.find_all by_mnem mnemonic

let has_mask (instr : instr_emit) =
  Array.exists
    (fun (a : arg) -> match a.enc with Mask -> true | _ -> false)
    instr.args

(* Destinations, then value sources. *)
let operands (instr : instr_emit) =
  let args =
    Array.to_list instr.args
    |> List.filter (fun (a : arg) ->
        match a.enc with
        | RM_r | RM_rm | Vex_v -> true
        | Mask | Implicit | Immediate -> false)
  in
  match instr.res with
  | Res rr -> Array.to_list rr @ args
  | Arg _ | Res_none -> args

let classes instr =
  List.filter_map (fun (a : arg) -> loc_class a.loc) (operands instr)

let same_shape a b =
  Bool.equal (has_mask a) (has_mask b)
  && List.equal String.equal (classes a) (classes b)

let dest_is_reg instr =
  match operands instr with { enc = RM_r; _ } :: _ -> true | _ -> false

let all_regs instr =
  List.for_all (fun (a : arg) -> loc_allows_reg a.loc) (operands instr)

let res_is_mem (instr : instr_emit) =
  match instr.res with
  | Res rr -> Array.exists (fun (a : arg) -> loc_allows_mem a.loc) rr
  | Arg _ | Res_none -> false

(* The destination that the merge (~z:false) form also reads. *)
let merge_dest ~zeroing (instr : instr_emit) =
  match instr.res with
  | Res rr when instr.flags.z && merge_reads_dst instr.mnemonic && not zeroing
    ->
    Array.to_list rr
  | Res _ | Arg _ | Res_none -> []

let binding_arity ~zeroing (instr : instr_emit) =
  Array.length instr.args + List.length (merge_dest ~zeroing instr)

let prefer f l = match List.filter f l with [] -> l | l -> l

(* Specs *)

module Bind = struct
  type t =
    { name : string;
      zeroing : bool option
    }

  let make ~zeroing b =
    { name = b.bname;
      zeroing = (if b.instr.flags.z then Some zeroing else None)
    }

  let print ?(ctor = "Instr", "Zeroing") { name; zeroing } =
    let plain, zeroing_ctor = ctor in
    match zeroing with
    | None -> sprintf "%s %s" plain name
    | Some z -> sprintf "%s (%s, %b)" zeroing_ctor name z
end

module Spec = struct
  type t = (string, Bind.t, Bind.t, Bind.t) Intrin.spec

  let values args = Array.of_list (List.map (fun j -> Intrin.Value j) args)

  let print_imm : Intrin.imm -> string = function
    | No_imm -> "No_imm"
    | Imm max -> sprintf "Imm %d" max
    | Getmant (interval, sign) -> sprintf "Getmant (%d, %d)" interval sign
    | Fixed i -> sprintf "Fixed %d" i

  let print_cur = function
    | None -> "None"
    | Some b -> sprintf "Some (%s)" (Bind.print b)

  let print_operands args =
    Array.to_list args
    |> List.map (fun (operand : Intrin.operand) ->
        match operand with
        | Value j -> sprintf "Value %d" j
        | All_ones_mask -> "All_ones_mask"
        | Zero_vec128 -> "Zero_vec128"
        | Zero_vec256 -> "Zero_vec256"
        | Zero_vec512 -> "Zero_vec512")
    |> String.concat "; " |> sprintf "[|%s|]"

  let print : t -> string = function
    | Default { arity; imm; bind; args } ->
      sprintf "Default { arity = %d; imm = %s; bind = %s; args = %s }" arity
        (print_imm imm) (Bind.print bind) (print_operands args)
    | With_round { arity; bind; cur; args } ->
      sprintf "With_round { arity = %d; bind = %s; cur = %s; args = %s }" arity
        (Bind.print ~ctor:("Rnd", "Rnd_zeroing") bind)
        (print_cur cur) (print_operands args)
    | With_sae { arity; imm; bind; cur; args } ->
      sprintf
        "With_sae { arity = %d; imm = %s; bind = %s; cur = %s; args = %s }"
        arity (print_imm imm)
        (Bind.print ~ctor:("Sae", "Sae_zeroing") bind)
        (print_cur cur) (print_operands args)
    | With_sae_imm { arity; bind; cur; args } ->
      sprintf "With_sae_imm { arity = %d; bind = %s; cur = %s; args = %s }"
        arity
        (Bind.print ~ctor:("Sae", "Sae_zeroing") bind)
        (Bind.print cur) (print_operands args)
    | Flag { flag; instr } ->
      sprintf "Flag { flag = %s; instr = %s }"
        (match flag with Zf -> "Zf" | Cf -> "Cf")
        instr
    | Load { arity; bind; args } ->
      sprintf "Load { arity = %d; bind = %s; args = %s }" arity
        (Bind.print bind) (print_operands args)
    | Store { arity; bind; args } ->
      sprintf "Store { arity = %d; bind = %s; args = %s }" arity
        (Bind.print bind) (print_operands args)
    | Gather { arity; instr; args } ->
      sprintf "Gather { arity = %d; instr = %s; args = %s }" arity instr
        (print_operands args)
    | Scatter { arity; instr; args } ->
      sprintf "Scatter { arity = %d; instr = %s; args = %s }" arity instr
        (print_operands args)
end

(* The FMA and two-table permute alternatives differ in which source they
   overwrite. *)
let choose_instruction i =
  match i.instrs with
  | [] -> raise Unsupported
  | [instr] -> instr
  | instr :: _ as instrs ->
    let want =
      if contains ~needle:"_mask3_" i.name
      then "231"
      else if contains ~needle:"_mask2_" i.name
      then "permi2"
      else if contains ~needle:"perm" instr.mnemonic
      then "permt2"
      else "213"
    in
    required
      (List.find_opt (fun instr -> contains ~needle:want instr.mnemonic) instrs)

(* Masked aligned moves never take a memory operand here, so use the unaligned
   form, which cannot fault on a spilled source. *)
let dealign ~by_mnem b =
  match
    List.assoc_opt b.instr.mnemonic
      [ "vmovdqa32", "vmovdqu32";
        "vmovdqa64", "vmovdqu64";
        "vmovaps", "vmovups";
        "vmovapd", "vmovupd" ]
  with
  | Some unaligned -> (
    bindings_of ~by_mnem unaligned
    |> List.filter (fun b' ->
        Bool.equal b'.instr.flags.z b.instr.flags.z
        && same_shape b'.instr b.instr)
    |> prefer (fun b -> dest_is_reg b.instr)
    |> function
    | [b'] -> b'
    | _ -> b)
  | None -> b

let match_register ~by_mnem i instr =
  let s = shape_of_form instr.form in
  let gprs = function
    | "GPRlo" -> ["R64"; "R32"; "R16"; "R8"]
    | "R32" -> ["R32"; "R64"]
    | c -> [c]
  in
  let candidates xml_classes =
    bindings_of ~by_mnem instr.mnemonic
    |> List.filter (fun b ->
        all_regs b.instr
        && Bool.equal (has_mask b.instr) s.masked
        && (match b.instr.flags.r with
          | Rnd_er -> s.rnd
          | Rnd_sae -> s.sae
          | Rnd_none -> not (s.rnd || s.sae))
        && List.equal
             (fun x c -> List.mem c (gprs x))
             xml_classes (classes b.instr))
  in
  let cands =
    (* Some forms omit the destination. *)
    match candidates s.classes, ctype_class i.ret.ctype with
    | [], Some dest -> candidates (dest :: s.classes)
    | cands, _ -> cands
  in
  match
    cands
    |> prefer (fun b -> List.equal String.equal s.classes (classes b.instr))
    |> prefer (fun b -> dest_is_reg b.instr)
  with
  | [b] -> dealign ~by_mnem b
  | [] -> raise Unsupported
  | _ :: _ :: _ -> failwith ("ambiguous " ^ i.name)

(* The non-immediate C arguments, in operand order. *)
let reorder ~binding i =
  let vals = List.mapi (fun j p -> j, p) (values i) in
  let masks, others = List.partition (fun (_, p) -> is_mask p) vals in
  let others = List.map fst others in
  match masks with
  | _ when not (has_mask binding.instr) -> List.map fst vals
  | [(k, _)] when contains ~needle:"_mask3_" i.name -> (
    (* VF*231 overwrites the addend. *)
    match List.rev others with
    | c :: rest -> (c :: List.rev rest) @ [k]
    | [] -> raise Unsupported)
  | [(k, _)] when contains ~needle:"_mask2_" i.name -> (
    (* VPERMI2* overwrites the index. *)
    match others with
    | a :: idx :: rest -> (idx :: a :: rest) @ [k]
    | _ -> raise Unsupported)
  | [(k, _)] -> others @ [k]
  | _ -> raise Unsupported

(* The predicate of e.g. [_mm512_cmplt_epi32_mask]. *)
let fixed_predicate ~mnemonic name =
  let start = required (substring_index ~needle:"cmp" name) + 3 in
  let rec stop j =
    if j < String.length name && name.[j] >= 'a' && name.[j] <= 'z'
    then stop (j + 1)
    else j
  in
  let predicates =
    if String.starts_with ~prefix:"vpcmp" mnemonic
    then ["eq", 0; "lt", 1; "le", 2; "neq", 4; "ne", 4; "ge", 5; "gt", 6]
    else if String.starts_with ~prefix:"vcmp" mnemonic
    then
      [ "eq", 0;
        "lt", 1;
        "le", 2;
        "unord", 3;
        "neq", 4;
        "nlt", 5;
        "nle", 6;
        "ord", 7;
        "ge", 13;
        "gt", 14 ]
    else []
  in
  required
    (List.assoc_opt (String.sub name start (stop start - start)) predicates)

let register_spec ~by_mnem ~binding ~zeroing i : Spec.t =
  let sae_imms, imms =
    List.filter is_imm i.params
    |> List.partition (fun p -> p.immtype = Some "_MM_FROUND_SAE")
  in
  let order = reorder ~binding i in
  if List.length order <> binding_arity ~zeroing binding.instr
  then raise Unsupported;
  let arity = List.length (values i) in
  let bind = Bind.make ~zeroing binding in
  let args = Spec.values order in
  (* The non-rounding form, for _MM_FROUND_CUR_DIRECTION. *)
  let cur () =
    bindings_of ~by_mnem binding.instr.mnemonic
    |> List.find_opt (fun b ->
        b.instr.flags.r = Rnd_none && all_regs b.instr
        && Int.equal (binding_arity ~zeroing b.instr) (List.length order)
        && same_shape b.instr binding.instr)
    |> Option.map (Bind.make ~zeroing)
  in
  let imm () : Intrin.imm =
    match imms with
    | [] -> No_imm
    | [p] -> Imm (imm_max p)
    | [interval; sign] -> Getmant (imm_max interval, imm_max sign)
    | _ -> raise Unsupported
  in
  match binding.instr.flags.r, sae_imms, imms, binding.instr.imm with
  | Rnd_er, [], [_], _ -> With_round { arity; bind; cur = cur (); args }
  | Rnd_sae, [_], _, _ ->
    With_sae { arity; imm = imm (); bind; cur = cur (); args }
  | Rnd_sae, [], [{ immtype = Some "_MM_FROUND"; _ }], _ ->
    With_sae_imm { arity; bind; cur = required (cur ()); args }
  | Rnd_none, [], [], Imm_none -> Default { arity; imm = No_imm; bind; args }
  | Rnd_none, [], [], (Imm_spec | Imm_reg) ->
    let imm = fixed_predicate ~mnemonic:binding.instr.mnemonic i.name in
    Default { arity; imm = Fixed imm; bind; args }
  | Rnd_none, [], _ :: _, (Imm_spec | Imm_reg) ->
    Default { arity; imm = imm (); bind; args }
  | _ -> raise Unsupported

let match_memory ~by_mnem instr =
  let s = shape_of_form instr.form in
  if List.length (List.filter (String.equal "MEM") s.classes) <> 1
  then raise Unsupported;
  let accepts tok (a : arg) =
    Option.equal String.equal (loc_class a.loc) (Some tok)
    || (String.equal tok "MEM" && loc_allows_mem a.loc)
  in
  bindings_of ~by_mnem instr.mnemonic
  |> List.filter (fun b ->
      let ops = operands b.instr in
      Bool.equal (has_mask b.instr) s.masked
      (* [Isimd_mem] only addresses arguments. *)
      && (not (res_is_mem b.instr))
      && List.length ops = List.length s.classes
      && List.for_all2 accepts s.classes ops)
  |> function
  | [b] -> b
  | _ -> raise Unsupported

let memory_spec ~binding ~zeroing i : Spec.t =
  if List.exists is_imm i.params then raise Unsupported;
  let params = List.mapi (fun j p -> j, p) i.params in
  let indices f =
    List.filter_map (fun (j, p) -> if f p then Some j else None) params
  in
  let ptr =
    required
      (List.find_map (fun (j, p) -> if is_ptr p then Some j else None) params)
  in
  let masks = ref (indices is_mask) in
  let vecs = ref (indices (fun p -> not (is_ptr p || is_mask p))) in
  let pop r =
    match !r with
    | x :: rest ->
      r := rest;
      x
    | [] -> raise Unsupported
  in
  let args =
    merge_dest ~zeroing binding.instr
    @ List.filter
        (fun a -> not (arg_is_implicit a))
        (Array.to_list binding.instr.args)
    |> List.map (fun (a : arg) ->
        if loc_allows_mem a.loc
        then ptr
        else match a.loc with Temp [| K |] -> pop masks | _ -> pop vecs)
  in
  if !masks <> [] || !vecs <> [] then raise Unsupported;
  let arity = List.length i.params in
  let bind = Bind.make ~zeroing binding in
  let args = Spec.values args in
  if String.starts_with ~prefix:"__m" i.ret.ctype
  then Load { arity; bind; args }
  else Store { arity; bind; args }

(* The XML forms of gathers and scatters are unreliable, so match on the C types
   and the index width (the d/q after gather/scatter). *)
let gather_scatter_spec ~by_mnem i instr : Spec.t =
  let vals = List.mapi (fun j p -> j, p) (values i) in
  let arity = List.length vals in
  let base =
    required
      (List.find_map (fun (j, p) -> if is_ptr p then Some j else None) vals)
  in
  let k : Intrin.operand =
    match List.find_opt (fun (_, p) -> is_mask p) vals with
    | Some (k, _) -> Value k
    | None -> All_ones_mask
  in
  let vectors = List.filter (fun (_, p) -> not (is_ptr p || is_mask p)) vals in
  let m = instr.mnemonic in
  let key = if contains ~needle:"gather" m then "gather" else "scatter" in
  let width =
    match m.[required (substring_index ~needle:key m) + String.length key] with
    | 'd' -> "32"
    | 'q' -> "64"
    | _ -> raise Unsupported
  in
  let class_of p = required (ctype_class p.ctype) in
  let vsib p = "VM" ^ width ^ String.sub (class_of p) 0 1 in
  let binding expected =
    match
      List.filter
        (fun b -> List.equal String.equal expected (classes b.instr))
        (bindings_of ~by_mnem m)
    with
    | [b] -> b.bname
    | _ -> raise Unsupported
  in
  match ctype_class i.ret.ctype, vectors with
  | Some dst, [(src, _); (vindex, p)] ->
    Gather
      { arity;
        instr = binding [dst; vsib p];
        args = [| Value src; Value base; Value vindex; k |]
      }
  | Some dst, [(vindex, p)] ->
    let zero : Intrin.operand =
      match dst with
      | "XMM" -> Zero_vec128
      | "YMM" -> Zero_vec256
      | _ -> Zero_vec512
    in
    Gather
      { arity;
        instr = binding [dst; vsib p];
        args = [| zero; Value base; Value vindex; k |]
      }
  | None, [(vindex, p); (data, q)] ->
    Scatter
      { arity;
        instr = binding [vsib p; class_of q];
        args = [| Value base; Value vindex; Value data; k |]
      }
  | _ -> raise Unsupported

let flag_spec ~by_mnem i : Spec.t =
  let flag : Intrin.flag =
    if contains ~needle:"testz" i.name
    then Zf
    else if contains ~needle:"testc" i.name
    then Cf
    else raise Unsupported
  in
  match bindings_of ~by_mnem (choose_instruction i).mnemonic with
  | [b] -> Flag { flag; instr = b.bname }
  | _ -> raise Unsupported

(* Single instructions, as compiled by clang, that the XML lists as sequences or
   without an instruction. VEX moves zero the upper bits. *)
let single_instruction =
  [ "_mm512_set1_epi8", "vpbroadcastb_Z_r32";
    "_mm512_set1_epi16", "vpbroadcastw_Z_r32";
    "_mm512_set1_ps", "vbroadcastss_Z_Xm32";
    "_mm512_set1_pd", "vbroadcastsd_Z_Xm64";
    "_mm512_zextps128_ps512", "vmovaps_X_Xm128";
    "_mm512_zextpd128_pd512", "vmovaps_X_Xm128";
    "_mm512_zextsi128_si512", "vmovaps_X_Xm128";
    "_mm512_zextps256_ps512", "vmovaps_Y_Ym256";
    "_mm512_zextpd256_pd512", "vmovaps_Y_Ym256";
    "_mm512_zextsi256_si512", "vmovaps_Y_Ym256" ]

(* Aliases for a cast and the full-width intrinsic. *)
let is_low_half i =
  contains ~needle:"logather" i.name
  || contains ~needle:"loscatter" i.name
  || contains ~needle:"cvt" i.name
     && (contains ~needle:"lo_pd" i.name || contains ~needle:"pslo" i.name)

let spec ~by_mnem i : Spec.t =
  match List.assoc_opt i.name single_instruction with
  | Some name ->
    Default
      { arity = 1;
        imm = No_imm;
        bind = { Bind.name; zeroing = None };
        args = [| Value 0 |]
      }
  | None ->
    if String.equal i.tech "SVML" || i.sequence || is_low_half i
    then raise Unsupported;
    if contains ~needle:"kortest" i.name || contains ~needle:"ktest" i.name
    then flag_spec ~by_mnem i
    else
      let instr = choose_instruction i in
      let s = shape_of_form instr.form in
      if List.mem "VSIB" s.classes
      then gather_scatter_spec ~by_mnem i instr
      else if List.mem "MEM" s.classes
      then
        memory_spec ~binding:(match_memory ~by_mnem instr) ~zeroing:s.zeroing i
      else
        register_spec ~by_mnem
          ~binding:(match_register ~by_mnem i instr)
          ~zeroing:s.zeroing i

let specs ~bindings intrinsics =
  let by_mnem = build_by_mnem bindings in
  List.filter_map
    (fun i ->
      if in_scope i
      then Some (i, try Some (spec ~by_mnem i) with Unsupported -> None)
      else None)
    intrinsics

(* Tests compare each builtin with the C intrinsic, compiled by clang, on the
   same inputs. Stores write identically initialized buffers, which are then
   compared whole. See helpers.ml and helpers.c. *)

type tkind =
  | Vec of int
  | MaskT
  | I32
  | I64
  | F32
  | F64
  | ISub
  | Ptr
  | Void

(* The kind and the OCaml type of a parameter or result. *)
let tkind_of p =
  let unboxed k ty = Some (k, sprintf "(%s[@unboxed])" ty) in
  let vec ty bits width =
    unboxed (Vec width) (sprintf "%s%dx%d" ty bits (width / bits))
  in
  let int_vec width =
    match p.etype with
    | "UI8" | "SI8" | "M8" -> vec "int" 8 width
    | "UI16" | "SI16" -> vec "int" 16 width
    | "UI64" | "SI64" -> vec "int" 64 width
    | _ -> vec "int" 32 width
  in
  match p.ctype with
  | "__m512" -> vec "float" 32 512
  | "__m512d" -> vec "float" 64 512
  | "__m512i" -> int_vec 512
  | "__m256" -> vec "float" 32 256
  | "__m256d" -> vec "float" 64 256
  | "__m256i" -> int_vec 256
  | "__m128" -> vec "float" 32 128
  | "__m128d" -> vec "float" 64 128
  | "__m128i" -> int_vec 128
  | "__mmask8" | "__mmask16" | "__mmask32" | "__mmask64" -> unboxed MaskT "mask"
  | "int" | "unsigned int" | "const int" -> unboxed I32 "int32"
  | "__int64" | "unsigned __int64" | "long long" | "unsigned long long" ->
    unboxed I64 "int64"
  | "char" | "unsigned char" | "short" | "unsigned short" ->
    Some (ISub, "(int[@untagged])")
  | "float" -> unboxed F32 "float32"
  | "double" -> unboxed F64 "float"
  | "void" -> Some (Void, "void")
  | _ when is_ptr p -> Some (Ptr, "addr")
  | _ -> None

type test_kind =
  | Compare_result
  | Compare_buffers

type test =
  { i : intrinsic;
    kind : test_kind;
    memory : bool; (* in the memory-form executable *)
    er : bool;
    reuse_mask : bool
        (* masked gathers and scatters clobber their mask, so also check a
           second instruction using the same mask *)
  }

let tests ~bindings intrinsics =
  let testable i =
    Option.is_some (tkind_of i.ret)
    && List.for_all (fun p -> is_imm p || Option.is_some (tkind_of p)) i.params
    && values i <> []
  in
  specs ~bindings intrinsics
  |> List.filter_map (fun (i, spec) ->
      let test kind ~memory ?(er = false) ?(reuse_mask = false) () =
        Some { i; kind; memory; er; reuse_mask }
      in
      let masked = List.exists is_mask i.params in
      match (spec : Spec.t option) with
      | None -> None
      | Some _ when not (testable i) -> None
      | Some (Default _ | With_sae _ | With_sae_imm _ | Flag _) ->
        test Compare_result ~memory:false ()
      | Some (With_round _) -> test Compare_result ~memory:false ~er:true ()
      | Some (Load _) -> test Compare_result ~memory:true ()
      | Some (Gather _) ->
        test Compare_result ~memory:true ~reuse_mask:masked ()
      | Some (Store _) -> test Compare_buffers ~memory:true ()
      | Some (Scatter _) ->
        test Compare_buffers ~memory:true ~reuse_mask:masked ())
  |> List.sort_uniq (fun a b -> String.compare a.i.name b.i.name)

(* Rounding immediates follow [Simd_selection]: CUR_DIRECTION and NO_EXC with
   each rounding mode (embedded rounding), NO_EXC alone or with CUR_DIRECTION
   (sae), or the instruction's own (VCVTPS2PH). *)
let imm_values ~er p =
  match p.immtype with
  | Some "_CMP_" -> List.init 32 Fun.id
  | Some "_MM_CMPINT" -> List.init 8 Fun.id
  | Some "_MM_FROUND" ->
    if er then [8; 9; 10; 11; 4] else [0; 1; 2; 3; 4; 8; 9; 10; 11; 12]
  | Some "_MM_FROUND_SAE" -> [8; 12; 4]
  | Some "_MM_INDEX_SCALE" -> [1; 2; 4; 8]
  | Some "_MM_MANTISSA_NORM" -> [0; 1; 2; 3]
  | Some "_MM_MANTISSA_SIGN" -> [0; 1; 2]
  | Some "_MM_PERM" -> [0; 27; 177; 255]
  | _ ->
    let max = imm_max p in
    List.sort_uniq compare [0; 1; max / 2; max]

let rec product = function
  | [] -> [[]]
  | vs :: rest ->
    let tails = product rest in
    List.concat_map (fun v -> List.map (fun t -> v :: t) tails) vs

let test_tuples { i; er; _ } =
  product (List.map (imm_values ~er) (List.filter is_imm i.params))

let test_suffix = function
  | [] -> ""
  | tuple -> "_" ^ String.concat "_" (List.map string_of_int tuple)

(* Masks are bound to [m<idx>] and pointers to [p<buffer>], so that the builtin
   and the oracle share them. *)
let value_expr ?(shift = 0) ~buffer ~idx p k =
  let v = "abc".[(idx + shift) mod 3] in
  match k with
  | Vec width when String.equal p.varname "vindex" ->
    let bits = match p.etype with "SI64" | "UI64" -> 64 | _ -> 32 in
    sprintf "(reint idx%d_%d)" bits width
  | Vec width ->
    sprintf "(reint v%c%s)" v (if width = 512 then "" else string_of_int width)
  | MaskT -> sprintf "m%d" idx
  | I32 -> [| "0x12345678l"; "(-7l)"; "0x40000001l" |].(idx mod 3)
  | I64 ->
    [| "0x1122334455667788L"; "(-9L)"; "0x4000000000000001L" |].(idx mod 3)
  | ISub -> [| "23"; "113"; "5" |].(idx mod 3)
  | F32 -> [| "f32a"; "f32b"; "f32c" |].(idx mod 3)
  | F64 -> [| "1.5"; "(-2.25)"; "3.0e10" |].(idx mod 3)
  | Ptr -> sprintf "p%d" buffer
  | Void -> invalid_arg "value_expr"

(* The mask ABI passes the full register, so clear the bits beyond the C
   type. *)
let mask_binding ~idx p =
  let width =
    match p.ctype with
    | "__mmask8" -> 0xFFL
    | "__mmask16" -> 0xFFFFL
    | "__mmask32" -> 0xFFFFFFFFL
    | _ -> -1L
  in
  sprintf "let m%d = mask_arg masks %d 0x%LxL in " idx idx width

let check_fn = function
  | Vec width -> sprintf "check%d" width
  | MaskT -> "check_mask"
  | I32 -> "check_i32"
  | I64 -> "check_i64"
  | F32 -> "check_f32"
  | F64 -> "check_f64"
  | ISub -> "check_int"
  | Ptr | Void -> invalid_arg "check_fn"

(* clang 21 swaps the sources of the masked scalar square roots with
   CUR_DIRECTION, so use an oracle built from SSE. *)
let scalar_sqrt_oracle i tuple =
  match i.name with
  | "_mm_mask_sqrt_ss" | "_mm_maskz_sqrt_ss" | "_mm_mask_sqrt_sd"
  | "_mm_maskz_sqrt_sd" | "_mm_mask_sqrt_round_ss" | "_mm_maskz_sqrt_round_ss"
  | "_mm_mask_sqrt_round_sd" | "_mm_maskz_sqrt_round_sd"
    when tuple = [] || tuple = [4] ->
    let double = String.ends_with ~suffix:"sd" i.name in
    let suffix = if double then "sd" else "ss" in
    let src, k, a, b =
      if contains ~needle:"_maskz_" i.name
      then
        ( (if double then "_mm_setzero_pd()" else "_mm_setzero_ps()"),
          "p0",
          "p1",
          "p2" )
      else "p0", "p1", "p2", "p3"
    in
    let sqrt_args = if double then b ^ ", " ^ b else b in
    Some
      (sprintf "_mm_move_%s(%s, (%s & 1) ? _mm_sqrt_%s(%s) : %s)" suffix a k
         suffix sqrt_args src)
  | _ -> None

let print_test_ml buf ({ i; kind; reuse_mask; _ } as test) =
  let name = caml_name i in
  let vals = values i in
  let val_tys = List.map (fun p -> Option.get (tkind_of p)) vals in
  let ret_ty = Option.get (tkind_of i.ret) in
  let signature tys = String.concat " -> " (List.map snd tys) in
  let imm_tys =
    List.filter_map
      (fun p -> if is_imm p then Some (ISub, "(int[@untagged])") else None)
      i.params
  in
  bprintf buf
    "\n\
     external %s : %s = \"caml_vec512_unreachable\" %S\n\
     [@@noalloc] [@@builtin]\n"
    name
    (signature (imm_tys @ val_tys @ [ret_ty]))
    name;
  let masks =
    List.mapi (fun idx p -> idx, p) vals
    |> List.filter (fun (_, p) -> is_mask p)
  in
  (* [shift] picks different vector arguments. *)
  let args ?shift ~buffer () =
    List.mapi
      (fun idx (p, (k, _)) -> value_expr ?shift ~buffer ~idx p k)
      (List.combine vals val_tys)
    |> String.concat " "
  in
  List.iter
    (fun tuple ->
      let suffix = test_suffix tuple in
      let oracle_name = sprintf "c%s%s" i.name suffix in
      bprintf buf "external %s : %s = \"\" \"ctest%s%s\" [@@noalloc]\n"
        oracle_name
        (signature (val_tys @ [ret_ty]))
        i.name suffix;
      let imm_args = String.concat "" (List.map (sprintf "%d ") tuple) in
      let builtin ?shift buffer =
        sprintf "(%s %s%s)" name imm_args (args ?shift ~buffer ())
      in
      let oracle ?shift buffer =
        sprintf "(%s %s)" oracle_name (args ?shift ~buffer ())
      in
      let reused = "(name ^ \"/reused_mask\")" in
      let body =
        match kind with
        | Compare_result ->
          let check = check_fn (fst ret_ty) in
          if reuse_mask
          then
            sprintf "let r1 = %s in let r2 = %s in %s name r1 %s; %s %s r2 %s"
              (builtin 0) (builtin ~shift:1 0) check (oracle 0) check reused
              (oracle ~shift:1 0)
          else sprintf "%s name %s %s" check (builtin 0) (oracle 0)
        | Compare_buffers ->
          let run a b =
            sprintf "ignore (buf_reset 0); let _ = %s in let _ = %s in " a b
          in
          run (builtin 1) (oracle 2)
          ^ "check_buf name"
          ^
          if reuse_mask
          then sprintf "; %scheck_buf %s" (run (builtin 1) (builtin 2)) reused
          else ""
      in
      let body =
        match kind, List.exists is_ptr vals with
        | _, false -> body
        | Compare_result, true -> "let p0 = buf 0 in " ^ body
        | Compare_buffers, true -> "let p1 = buf 1 in let p2 = buf 2 in " ^ body
      in
      let label = i.name ^ suffix in
      match masks with
      | [] -> bprintf buf "let () = let name = %S in %s\n" label body
      | _ :: _ ->
        let bindings =
          String.concat ""
            (List.map (fun (idx, p) -> mask_binding ~idx p) masks)
        in
        bprintf buf "let () = test_masks %S (fun name masks -> %s%s)\n" label
          bindings body)
    (test_tuples test)

let tests_ml ~memory ~bindings intrinsics =
  let buf = Buffer.create 262144 in
  Buffer.add_string buf
    "(* Generated by tools/simdgen/simdgen_intrins.ml: bit-for-bit checks of\n\
    \   emitted intrinsics against the real C intrinsics. *)\n\n\
     open Helpers\n";
  List.iter
    (fun test -> if Bool.equal test.memory memory then print_test_ml buf test)
    (tests ~bindings intrinsics);
  Buffer.add_string buf "\nlet () = if !failures <> 0 then exit 1\n";
  print_string (Buffer.contents buf)

(* The Intel data uses MSVC type names. *)
let cparam = function
  | "__int64" | "long long" -> "int64_t"
  | "unsigned __int64" | "unsigned long long" -> "uint64_t"
  | c -> c

(* The OCaml side reads the full register. *)
let cret = function
  | c when String.starts_with ~prefix:"__mmask" c -> "__mmask64"
  | "char" | "unsigned char" | "short" | "unsigned short" -> "int64_t"
  | "int" | "unsigned int" | "const int" -> "int32_t"
  | "__int64" | "unsigned __int64" | "long long" | "unsigned long long" ->
    "int64_t"
  | c -> c

(* The arguments are [p0], [p1], ..., with the immediates of [tuple]. *)
let c_call i tuple =
  let rec args j tuple = function
    | [] -> []
    | p :: ps when is_imm p ->
      string_of_int (List.hd tuple) :: args j (List.tl tuple) ps
    | p :: ps ->
      (if is_ptr p then sprintf "(%s)p%d" p.ctype j else sprintf "p%d" j)
      :: args (j + 1) tuple ps
  in
  sprintf "%s(%s)" i.name (String.concat ", " (args 0 tuple i.params))

(* Without AVX512 the oracles are stubs, so that the tests still link. *)
let tests_c ~bindings intrinsics =
  let buf = Buffer.create 262144 in
  let oracles = Buffer.create 262144 in
  let names = ref [] in
  Buffer.add_string buf
    "/* Generated by tools/simdgen/simdgen_intrins.ml. */\n\
     #include <caml/simd.h>\n\
     #include <assert.h>\n\
     #include <stdint.h>\n\n\
     #define BUILTIN(name) void name() { assert(0); }\n\n";
  List.iter
    (fun ({ i; kind; _ } as test) ->
      bprintf buf "BUILTIN(caml%s)\n" i.name;
      let params =
        List.mapi
          (fun j p ->
            if is_ptr p
            then sprintf "void *p%d" j
            else sprintf "%s p%d" (cparam p.ctype) j)
          (values i)
        |> String.concat ", "
      in
      List.iter
        (fun tuple ->
          let name = sprintf "ctest%s%s" i.name (test_suffix tuple) in
          names := name :: !names;
          let call =
            match scalar_sqrt_oracle i tuple with
            | Some expr -> expr
            | None -> c_call i tuple
          in
          match kind with
          | Compare_result ->
            bprintf oracles "%s %s(%s) { return %s; }\n" (cret i.ret.ctype) name
              params call
          | Compare_buffers ->
            bprintf oracles "void %s(%s) { %s; }\n" name params call)
        (test_tuples test))
    (tests ~bindings intrinsics);
  bprintf buf "\n#ifdef ARCH_AVX512\n#include <immintrin.h>\n\n%s\n#else\n\n"
    (Buffer.contents oracles);
  List.iter (bprintf buf "BUILTIN(%s)\n") (List.rev !names);
  Buffer.add_string buf "\n#endif\n";
  print_string (Buffer.contents buf)

let print_selection ~bindings intrinsics =
  print_string
    "(* Generated by tools/simdgen/simdgen_intrins.ml. *)\n\n\
     (* See [Amd64_simd_defs.Intrin]. *)\n\n\
     [@@@ocaml.warning \"+a-4-40-42-70\"]\n\n\
     open! Amd64_simd_instrs\n\
     open! Amd64_simd_defs.Intrin\n\n\
     let table : (string * id t) array =\n\
    \  [|\n";
  specs ~bindings intrinsics
  |> List.filter_map (fun (i, spec) ->
      Option.map (fun spec -> caml_name i, spec) spec)
  |> List.sort_uniq (fun (a, _) (b, _) -> String.compare a b)
  |> List.iter (fun (name, spec) ->
      printf "    %S, %s;\n" name (Spec.print spec));
  print_string "  |]\n"

let print_skipped ~bindings intrinsics =
  print_endline "# Unsupported sequences, SVML calls, casts, and constants.";
  specs ~bindings intrinsics
  |> List.filter_map (fun (i, spec) ->
      if Option.is_none spec then Some i.name else None)
  |> List.sort String.compare |> List.iter print_endline
