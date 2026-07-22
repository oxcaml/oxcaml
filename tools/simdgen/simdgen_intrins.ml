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

(* Translates the Intel Intrinsics Guide data ([x86-intel.xml]) into (1) a
   generated selection match ([amd64_simd_intrins.ml]) recognizing
   [caml_<intel-name>] builtins, and (2) generated C-verified tests. Matching
   between an intrinsic and a registered instruction descriptor happens here,
   where both representations are available: the instruction descriptors come
   from [simdgen.ml]'s CSV parse (as [(binding, instr_emit)] pairs) and the
   intrinsics from the XML. See the header comments in each section. *)

open Amd64_simd_defs
open Simdgen_types
open Printf

(* -------------------------------------------------------------------------- *)
(* Scope *)
(* -------------------------------------------------------------------------- *)

let scope_exts = ["AVX512F"; "AVX512DQ"; "AVX512CD"; "AVX512BW"; "AVX512VL"]

(* -------------------------------------------------------------------------- *)
(* XML intrinsic model *)
(* -------------------------------------------------------------------------- *)

type param =
  { etype : string; (* e.g. FP32, UI64, MASK, IMM *)
    ctype : string; (* C type, e.g. __m512, __mmask16, int, unsigned int * *)
    varname : string;
    memwidth : string option;
    immwidth : string option;
    immtype : string option
  }

type instruction =
  { mnemonic : string; (* lowercased *)
    form : string
  }

type intrinsic =
  { name : string;
    tech : string;
    sequence : bool;
    cpuid : string list;
    ret : param;
    params : param list;
    instrs : instruction list;
    category : string
  }

let param_of_node node =
  { etype = Option.value ~default:"" (Simdgen_xml.attr node "etype");
    ctype = Option.value ~default:"" (Simdgen_xml.attr node "type");
    varname = Option.value ~default:"" (Simdgen_xml.attr node "varname");
    memwidth = Simdgen_xml.attr node "memwidth";
    immwidth = Simdgen_xml.attr node "immwidth";
    immtype = Simdgen_xml.attr node "immtype"
  }

let intrinsic_of_node node =
  let name = Option.value ~default:"" (Simdgen_xml.attr node "name") in
  let tech = Option.value ~default:"" (Simdgen_xml.attr node "tech") in
  let sequence =
    match Simdgen_xml.attr node "sequence" with
    | Some "TRUE" -> true
    | _ -> false
  in
  let cpuid =
    Simdgen_xml.children_named node "CPUID"
    |> List.map (fun n -> n.Simdgen_xml.text)
  in
  let ret =
    match Simdgen_xml.child_named node "return" with
    | Some n -> param_of_node n
    | None ->
      { etype = "";
        ctype = "void";
        varname = "";
        memwidth = None;
        immwidth = None;
        immtype = None
      }
  in
  let params =
    Simdgen_xml.children_named node "parameter" |> List.map param_of_node
  in
  let instrs =
    Simdgen_xml.children_named node "instruction"
    |> List.map (fun n ->
        { mnemonic =
            String.lowercase_ascii
              (Option.value ~default:"" (Simdgen_xml.attr n "name"));
          form = Option.value ~default:"" (Simdgen_xml.attr n "form")
        })
  in
  let category =
    match Simdgen_xml.child_named node "category" with
    | Some n -> n.Simdgen_xml.text
    | None -> ""
  in
  { name; tech; sequence; cpuid; ret; params; instrs; category }

let parse_intrinsics path =
  let root = Simdgen_xml.parse_file path in
  Simdgen_xml.children_named root "intrinsic" |> List.map intrinsic_of_node

let in_scope i =
  (not (List.is_empty i.cpuid))
  && List.for_all (fun c -> List.mem c scope_exts) i.cpuid

(* -------------------------------------------------------------------------- *)
(* Classification *)
(* -------------------------------------------------------------------------- *)

type form_class =
  | Register
  | Memory
  | Gather_scatter
  | No_instruction

let strip_modifiers tok =
  let buf = Buffer.create (String.length tok) in
  let depth = ref 0 in
  String.iter
    (fun c ->
      match c with
      | '{' -> incr depth
      | '}' -> if !depth > 0 then decr depth
      | c -> if !depth = 0 then Buffer.add_char buf c)
    tok;
  String.trim (Buffer.contents buf)

let form_tokens form = String.split_on_char ',' form |> List.map String.trim

let is_mem_token t =
  match strip_modifiers t with
  | "m8" | "m16" | "m32" | "m64" | "m128" | "m256" | "m512" -> true
  | _ -> false

let is_vm_token t =
  let t = strip_modifiers t in
  String.length t >= 2 && t.[0] = 'v' && t.[1] = 'm'

let classify i =
  match i.instrs with
  | [] -> No_instruction
  | instr :: _ ->
    let toks = form_tokens instr.form in
    if List.exists is_vm_token toks
    then Gather_scatter
    else if List.exists is_mem_token toks
    then Memory
    else Register

(* -------------------------------------------------------------------------- *)
(* Register classes *)
(* -------------------------------------------------------------------------- *)

(* A "register class" abstracts an operand for matching: vector width is kept,
   [K] is kept, GPRs keep their width, and memory width is ignored (the XML mem
   widths are unreliable, per the plan). *)
let temp_regclass : temp -> string option = function
  | ZMM -> Some "ZMM"
  | YMM -> Some "YMM"
  | XMM -> Some "XMM"
  | K -> Some "K"
  | MM -> Some "MM"
  | R8 -> Some "R8"
  | R16 -> Some "R16"
  | R32 -> Some "R32"
  | R64 -> Some "R64"
  | M8 | M16 | M32 | M64 | M128 | M256 | M512 | VM32X | VM32Y | VM32Z | VM64X
  | VM64Y | VM64Z ->
    None

let loc_regclass (loc : loc) : string option =
  match loc with
  | Pin _ -> Some "R64"
  | Temp temps -> Array.find_map temp_regclass temps

(* Register class of a C return/parameter type, used to recover an operand the
   XML form omits (some logical ops list only the sources). *)
let ctype_regclass ctype =
  let ctype = String.trim ctype in
  let has p =
    String.length ctype >= String.length p
    && String.sub ctype 0 (String.length p) = p
  in
  if has "__m512"
  then Some "ZMM"
  else if has "__m256"
  then Some "YMM"
  else if has "__m128"
  then Some "XMM"
  else if has "__mmask"
  then Some "K"
  else None

(* -------------------------------------------------------------------------- *)
(* Hardware operand sequence of a registered instruction *)
(* -------------------------------------------------------------------------- *)

let arg_is_value (arg : arg) =
  match arg.enc with
  | RM_r | RM_rm | Vex_v -> true
  | Mask | Implicit | Immediate -> false

(* The ordered register classes of an instruction's hardware operands, dest(s)
   first then value sources, ignoring the write-mask. Also returns whether the
   instruction takes a write mask, and the [enc] of the first destination
   operand (used to break load/store ties). *)
let instr_hw_seq (instr : instr_emit) =
  let value_args = Array.to_list instr.args |> List.filter arg_is_value in
  let value_classes =
    List.filter_map (fun (a : arg) -> loc_regclass a.loc) value_args
  in
  let has_mask =
    Array.exists
      (fun (a : arg) -> match a.enc with Mask -> true | _ -> false)
      instr.args
  in
  match instr.res with
  | Res rr ->
    let dest_classes =
      Array.to_list rr |> List.filter_map (fun (a : arg) -> loc_regclass a.loc)
    in
    let dest_enc = match rr with [||] -> None | _ -> Some rr.(0).enc in
    dest_classes @ value_classes, has_mask, dest_enc
  | Arg _ | Res_none ->
    let dest_enc = match value_args with a :: _ -> Some a.enc | [] -> None in
    value_classes, has_mask, dest_enc

let instr_all_regcap (instr : instr_emit) =
  Array.to_list instr.args |> List.filter arg_is_value
  |> List.for_all (fun (a : arg) -> loc_allows_reg a.loc)
  &&
  match instr.res with
  | Res rr -> Array.for_all (fun (a : arg) -> loc_allows_reg a.loc) rr
  | Arg _ | Res_none -> true

(* -------------------------------------------------------------------------- *)
(* XML form shape *)
(* -------------------------------------------------------------------------- *)

type xml_shape =
  { classes : string list (* register classes of operands, imm dropped *);
    masked : bool;
    zeroing : bool;
    rnd : bool;
    sae : bool
  }

let token_class tok =
  match strip_modifiers tok with
  | "xmm" -> Some "XMM"
  | "ymm" -> Some "YMM"
  | "zmm" -> Some "ZMM"
  | "k" -> Some "K"
  | "r8" | "r16" -> Some "GPRlo"
  | "r32" -> Some "R32"
  | "r64" -> Some "R64"
  | "imm8" -> None
  | other -> Some ("?" ^ other)

let form_modifiers form =
  let mods = ref [] in
  String.iter (fun _ -> ()) form;
  List.iter
    (fun tok ->
      let buf = Buffer.create 8 in
      let depth = ref 0 in
      String.iter
        (fun c ->
          match c with
          | '{' -> incr depth
          | '}' ->
            if !depth > 0
            then (
              decr depth;
              let m = String.trim (Buffer.contents buf) in
              if String.length m > 0 then mods := m :: !mods;
              Buffer.clear buf)
          | c -> if !depth > 0 then Buffer.add_char buf c)
        tok)
    (form_tokens form);
  !mods

let xml_shape_of_form form =
  let classes = List.filter_map token_class (form_tokens form) in
  let mods = form_modifiers form in
  { classes;
    masked = List.mem "k" mods || List.mem "z" mods;
    zeroing = List.mem "z" mods;
    rnd = List.mem "er" mods;
    sae = List.mem "sae" mods
  }

let gpr_variants = function
  | "GPRlo" -> ["R64"; "R32"; "R16"; "R8"]
  | "R32" -> ["R32"; "R64"]
  | "R64" -> ["R64"]
  | c -> [c]

let classes_match xml_classes binding_classes =
  List.length xml_classes = List.length binding_classes
  && List.for_all2
       (fun xc bc ->
         String.equal xc bc
         ||
         match xc with
         | "GPRlo" | "R32" | "R64" -> List.mem bc (gpr_variants xc)
         | _ -> false)
       xml_classes binding_classes

(* -------------------------------------------------------------------------- *)
(* Instruction selection among multiple <instruction> alternatives *)
(* -------------------------------------------------------------------------- *)

let contains ~needle s =
  let nl = String.length needle and sl = String.length s in
  let rec go i = i + nl <= sl && (String.sub s i nl = needle || go (i + 1)) in
  nl = 0 || go 0

(* FMA and permutex2var families have several differing-mnemonic alternatives
   with identical operand shapes; the choice is forced by which C argument is
   the merge/overwrite target (see plan). Returns the chosen instruction, or
   [None] for families handled elsewhere (comi flag-readers). *)
let choose_instruction i =
  match i.instrs with
  | [] -> None
  | [instr] -> Some instr
  | instrs ->
    let is_fma m =
      contains ~needle:"132" m || contains ~needle:"213" m
      || contains ~needle:"231" m
    in
    if List.for_all (fun instr -> is_fma instr.mnemonic) instrs
    then
      (* 132/213/231 differ by which C operand is the overwrite target: the
         [mask3_] forms keep the addend [c] (231); everything else uses 213. *)
      let want = if contains ~needle:"_mask3_" i.name then "231" else "213" in
      match
        List.find_opt (fun instr -> contains ~needle:want instr.mnemonic) instrs
      with
      | Some instr -> Some instr
      | None -> Some (List.hd instrs)
    else if
      List.for_all (fun instr -> contains ~needle:"perm" instr.mnemonic) instrs
    then
      let want =
        if contains ~needle:"_mask2_" i.name then "permi2" else "permt2"
      in
      match
        List.find_opt (fun instr -> contains ~needle:want instr.mnemonic) instrs
      with
      | Some instr -> Some instr
      | None -> Some (List.hd instrs)
    else None

let is_flag_reader_name name =
  contains ~needle:"kortest" name
  || contains ~needle:"ktest" name
  || contains ~needle:"comi" name

(* The 18 forms whose XML omits the k source operand that the CSV has as a mask
   argument (movm -> vector, and broadcast-mask -> vector). *)
let omit_k_source_mnemonics =
  [ "vpmovm2b";
    "vpmovm2w";
    "vpmovm2d";
    "vpmovm2q";
    "vpbroadcastmb2q";
    "vpbroadcastmw2d" ]

(* -------------------------------------------------------------------------- *)
(* Matching a register-form intrinsic to a binding *)
(* -------------------------------------------------------------------------- *)

type binding =
  { bname : string;
    instr : instr_emit
  }

type match_result =
  | Matched of
      { binding : binding;
        zeroing : bool
      }
  | No_binding (* mnemonic/shape not present in the instruction descriptors *)

(* An aligned vector move (VMOVDQA*/VMOVAPS/VMOVAPD) requires a 64-byte-aligned
   memory operand, which faults if the register allocator spills the source into
   an unaligned stack slot. For a register-to-register masked move the aligned
   and unaligned encodings are semantically identical, so use the unaligned one
   ([VMOVDQU*/VMOVUPS/VMOVUPD]) to stay spill-safe. *)
let dealign_mnemonic = function
  | "vmovdqa32" -> Some "vmovdqu32"
  | "vmovdqa64" -> Some "vmovdqu64"
  | "vmovaps" -> Some "vmovups"
  | "vmovapd" -> Some "vmovupd"
  | _ -> None

let dealign ~by_mnem b =
  match dealign_mnemonic b.instr.mnemonic with
  | None -> b
  | Some unaligned -> (
    let target_seq, target_mask, _ = instr_hw_seq b.instr in
    let same b' =
      Bool.equal b'.instr.flags.z b.instr.flags.z
      &&
      let s, m, _ = instr_hw_seq b'.instr in
      Bool.equal m target_mask
      &&
        try List.for_all2 String.equal target_seq s
        with Invalid_argument _ -> false
    in
    let cands =
      Hashtbl.find_opt by_mnem unaligned
      |> Option.value ~default:[] |> List.filter same
    in
    let cands =
      match cands with
      | _ :: _ :: _ ->
        List.filter
          (fun b' ->
            let _, _, dest_enc = instr_hw_seq b'.instr in
            match dest_enc with Some RM_r -> true | _ -> false)
          cands
      | _ -> cands
    in
    match cands with [b'] -> b' | _ -> b)

let match_register ~(by_mnem : (string, binding list) Hashtbl.t) i instr =
  let xs = xml_shape_of_form instr.form in
  let filter_cands classes =
    Hashtbl.find_opt by_mnem instr.mnemonic
    |> Option.value ~default:[]
    |> List.filter (fun b ->
        let seq, has_mask, _ = instr_hw_seq b.instr in
        instr_all_regcap b.instr
        && Bool.equal has_mask xs.masked
        && (if xs.rnd
            then match b.instr.flags.r with Rnd_er -> true | _ -> false
            else if xs.sae
            then match b.instr.flags.r with Rnd_sae -> true | _ -> false
            else match b.instr.flags.r with Rnd_none -> true | _ -> false)
        && classes_match classes seq)
  in
  let cands =
    match filter_cands xs.classes with
    | [] -> (
      (* Some forms (e.g. plain [_mm_or_epi32]) omit the destination operand
         from the XML form; recover it from the return type and retry. *)
      match ctype_regclass i.ret.ctype with
      | Some dest -> filter_cands (dest :: xs.classes)
      | None -> [])
    | cands -> cands
  in
  (* Prefer an exact class match (no GPR-width normalization) when several
     candidates differ only by GPR width, e.g. vcvtsd2si_r32 vs _r64. *)
  let narrow filter cands =
    match cands with
    | _ :: _ :: _ -> ( match filter cands with [] -> cands | l -> l)
    | _ -> cands
  in
  let cands =
    narrow
      (List.filter (fun b ->
           let seq, _, _ = instr_hw_seq b.instr in
           try List.for_all2 String.equal xs.classes seq
           with Invalid_argument _ -> false))
      cands
  in
  let cands =
    (* load/store tie: prefer the binding whose destination is a register
       operand (RM_r), i.e. the value-producing (load-shaped) form. *)
    narrow
      (List.filter (fun b ->
           let _, _, dest_enc = instr_hw_seq b.instr in
           match dest_enc with Some RM_r -> true | _ -> false))
      cands
  in
  match cands with
  | [b] -> Matched { binding = dealign ~by_mnem b; zeroing = xs.zeroing }
  | [] -> No_binding
  | _ :: _ :: _ as l ->
    (* Remaining ambiguity is a matcher bug; surface it. *)
    failwith
      (sprintf "ambiguous match for %s (%s): %s" i.name instr.mnemonic
         (String.concat ", " (List.map (fun b -> b.bname) l)))

(* -------------------------------------------------------------------------- *)
(* Selection-arm emission *)
(* -------------------------------------------------------------------------- *)

let caml_name i = "caml" ^ i.name

let imm_max (p : param) =
  match p.immtype with
  | Some "_CMP_" -> 31
  | Some "_MM_CMPINT" -> 7
  | Some "_MM_PERM" -> 255
  | _ -> (
    match p.immwidth with Some w -> (1 lsl int_of_string w) - 1 | None -> 255)

let is_imm p = String.equal p.etype "IMM"

let is_mask_param p = String.equal p.etype "MASK"

let res_is_k (instr : instr_emit) =
  match instr.res with
  | Res rr ->
    Array.exists
      (fun (a : arg) ->
        match a.loc with
        | Temp ts -> Array.exists (fun t -> t = K) ts
        | _ -> false)
      rr
  | Arg _ | Res_none -> false

(* Value-operand reorder (indices into the non-imm C params, in C order) that
   maps the C argument list to the binding's operand order. Returns [None] for
   shapes not yet handled by the generator. *)
let reorder ~binding ~zeroing i =
  let vals = List.filter (fun p -> not (is_imm p)) i.params in
  let n = List.length vals in
  let idx = List.mapi (fun j p -> j, p) vals in
  let mask_positions =
    List.filter_map (fun (j, p) -> if is_mask_param p then Some j else None) idx
  in
  let non_mask =
    List.filter_map (fun (j, p) -> if is_mask_param p then None else Some j) idx
  in
  let masked =
    Array.exists
      (fun (a : arg) -> match a.enc with Mask -> true | _ -> false)
      binding.instr.args
  in
  if not masked
  then Some (List.init n (fun j -> j))
  else
    match mask_positions with
    | [k] -> (
      if res_is_k binding.instr || zeroing
      then Some (non_mask @ [k])
      else if contains ~needle:"blendm" binding.instr.mnemonic
      then
        (* blend: background is SRC1, duplicated as the merge destination *)
        match non_mask with
        | a :: rest -> Some ((a :: a :: rest) @ [k])
        | [] -> None
      else if contains ~needle:"_mask3_" i.name
      then
        (* accumulator [c] is the overwrite target (VF...231) *)
        match List.rev non_mask with
        | c :: rev_rest -> Some ((c :: List.rev rev_rest) @ [k])
        | [] -> None
      else if contains ~needle:"_mask2_" i.name
      then
        (* index is the overwrite target (VPERMI2) *)
        match non_mask with
        | a :: idxop :: rest -> Some ((idxop :: a :: rest) @ [k])
        | _ -> None
      else
        (* standard merge / masked FMA: first source is the overwrite target *)
        match non_mask with
        | dst :: rest -> Some ((dst :: rest) @ [k])
        | [] -> None)
    | _ -> None

(* Predicate immediate baked into hardcoded-predicate compares (e.g.
   [_mm512_cmplt_epi32_mask] -> VPCMPD with imm 1). The suffix between "cmp" and
   the element-type tag selects the predicate; integer compares use _MM_CMPINT
   and float compares use _CMP_. *)
let cmp_suffix name =
  (* the lowercase-letter run immediately following "cmp" *)
  let n = String.length name in
  let rec find i =
    if i + 3 > n
    then None
    else if String.sub name i 3 = "cmp"
    then (
      let j = ref (i + 3) in
      while !j < n && name.[!j] >= 'a' && name.[!j] <= 'z' do
        incr j
      done;
      Some (String.sub name (i + 3) (!j - (i + 3))))
    else find (i + 1)
  in
  find 0

let cmpint_imm = function
  | "eq" -> Some 0
  | "lt" -> Some 1
  | "le" -> Some 2
  | "neq" | "ne" -> Some 4
  | "ge" -> Some 5
  | "gt" -> Some 6
  | _ -> None

let cmp_imm = function
  | "eq" -> Some 0
  | "lt" -> Some 1
  | "le" -> Some 2
  | "unord" -> Some 3
  | "neq" -> Some 4
  | "nlt" -> Some 5
  | "nle" -> Some 6
  | "ord" -> Some 7
  | "ge" -> Some 13
  | "gt" -> Some 14
  | _ -> None

let hardcoded_predicate_imm ~mnemonic name =
  match cmp_suffix name with
  | None -> None
  | Some suffix ->
    if String.length mnemonic >= 5 && String.sub mnemonic 0 5 = "vpcmp"
    then cmpint_imm suffix
    else if String.length mnemonic >= 4 && String.sub mnemonic 0 4 = "vcmp"
    then cmp_imm suffix
    else None

(* Returns [Ok arm_body] or [Error skip_reason]. *)
let emit_register_arm ~by_mnem ~binding ~zeroing i =
  let imms = List.filter is_imm i.params in
  let sae_imms =
    List.filter (fun p -> p.immtype = Some "_MM_FROUND_SAE") imms
  in
  let real_imms =
    List.filter (fun p -> p.immtype <> Some "_MM_FROUND_SAE") imms
  in
  let vals = List.filter (fun p -> not (is_imm p)) i.params in
  let nvals = List.length vals in
  match reorder ~binding ~zeroing i with
  | None -> Error "phase 1: unhandled operand shape (follow-up)"
  | Some perm -> (
    let has_z = binding.instr.flags.z in
    let pat =
      "["
      ^ String.concat "; " (List.init nvals (fun j -> sprintf "x%d" j))
      ^ "]"
    in
    let reordered =
      "[" ^ String.concat "; " (List.map (fun j -> sprintf "x%d" j) perm) ^ "]"
    in
    let apply extra bname flags_z =
      let z = if flags_z then sprintf " ~z:%b" zeroing else "" in
      match extra, z with
      | "", "" -> bname
      | _ -> sprintf "(%s%s%s)" bname extra z
    in
    let sibling () =
      let target_seq, target_mask, _ = instr_hw_seq binding.instr in
      Hashtbl.find_opt by_mnem binding.instr.mnemonic
      |> Option.value ~default:[]
      |> List.find_opt (fun b ->
          (match b.instr.flags.r with Rnd_none -> true | _ -> false)
          && instr_all_regcap b.instr
          &&
          let s, m, _ = instr_hw_seq b.instr in
          Bool.equal m target_mask
          &&
            try List.for_all2 String.equal target_seq s
            with Invalid_argument _ -> false)
    in
    (* Prefix that extracts the instruction's own immediate (from the C imm
       params other than sae), and the [~i:...] expr referencing it. Returns
       [None] for imm arities we do not handle. *)
    let real_imm_prefix () =
      let imm_of_type (p : param) =
        match p.immtype with
        | Some "_MM_MANTISSA_NORM" -> 3
        | Some "_MM_MANTISSA_SIGN" -> 2
        | Some "_CMP_" -> 31
        | Some "_MM_CMPINT" -> 7
        | _ -> imm_max p
      in
      match real_imms with
      | [] -> Some ("", "")
      | [p] ->
        Some
          ( sprintf "let i, args = Sel.extract_constant args ~max:%d op in "
              (imm_of_type p),
            " ~i" )
      | [interval; sign] ->
        (* getmant packs two enums into one imm8: sign<<2 | interval. *)
        Some
          ( sprintf
              "let i0, args = Sel.extract_constant args ~max:%d op in let i1, \
               args = Sel.extract_constant args ~max:%d op in "
              (imm_of_type interval) (imm_of_type sign),
            " ~i:((i1 lsl 2) lor i0)" )
      | _ -> None
    in
    match binding.instr.flags.r with
    | Rnd_er ->
      if List.length real_imms <> 1 || sae_imms <> []
      then Error "phase 1: multi-imm (follow-up)"
      else
        let variants =
          [ "8", "Amd64_simd_defs.Rnd_near";
            "9", "Amd64_simd_defs.Rnd_down";
            "10", "Amd64_simd_defs.Rnd_up";
            "11", "Amd64_simd_defs.Rnd_zero" ]
        in
        let cases =
          List.map
            (fun (v, rnd) ->
              v, apply (sprintf " ~rnd:%s" rnd) binding.bname has_z)
            variants
          @
          match sibling () with
          | Some b -> ["4", apply "" b.bname b.instr.flags.z]
          | None -> []
        in
        let arms =
          String.concat " "
            (List.map
               (fun (v, e) -> sprintf "| %s -> Sel.instr %s %s" v e reordered)
               cases)
        in
        Ok
          (sprintf
             "(let i, args = Sel.extract_constant args ~max:255 op in match \
              args with %s -> (match i with %s | _ -> Sel.bad_immediate op) | \
              _ -> Sel.bad_arity op)"
             pat arms)
    | Rnd_sae -> (
      if List.length sae_imms <> 1
      then Error "phase 1: multi-imm (follow-up)"
      else
        match real_imm_prefix () with
        | None -> Error "phase 1: multi-imm (follow-up)"
        | Some (prefix, iexpr) ->
          let cases =
            ["8", apply " ~sae:()" binding.bname has_z]
            @
            match sibling () with
            | Some b -> ["4", apply "" b.bname b.instr.flags.z]
            | None -> []
          in
          let arms =
            String.concat " "
              (List.map
                 (fun (v, e) ->
                   sprintf "| %s -> Sel.instr %s%s %s" v e iexpr reordered)
                 cases)
          in
          Ok
            (sprintf
               "(%slet sae, args = Sel.extract_constant args ~max:255 op in \
                match args with %s -> (match sae with %s | _ -> \
                Sel.bad_immediate op) | _ -> Sel.bad_arity op)"
               prefix pat arms))
    | Rnd_none -> (
      if sae_imms <> []
      then Error "phase 1: multi-imm (follow-up)"
      else
        let bind_expr = apply "" binding.bname has_z in
        match binding.instr.imm, real_imms with
        | Imm_none, [] ->
          Ok
            (sprintf
               "(match args with %s -> Sel.instr %s %s | _ -> Sel.bad_arity op)"
               pat bind_expr reordered)
        | (Imm_spec | Imm_reg), [_] | (Imm_spec | Imm_reg), [_; _] -> (
          match real_imm_prefix () with
          | None -> Error "phase 1: multi-imm (follow-up)"
          | Some (prefix, iexpr) ->
            Ok
              (sprintf
                 "(%smatch args with %s -> Sel.instr %s%s %s | _ -> \
                  Sel.bad_arity op)"
                 prefix pat bind_expr iexpr reordered))
        | (Imm_spec | Imm_reg), [] -> (
          match
            hardcoded_predicate_imm ~mnemonic:binding.instr.mnemonic i.name
          with
          | Some imm ->
            Ok
              (sprintf
                 "(match args with %s -> Sel.instr %s ~i:%d %s | _ -> \
                  Sel.bad_arity op)"
                 pat bind_expr imm reordered)
          | None -> Error "phase 1: hardcoded predicate (follow-up)")
        | Imm_none, _ :: _ -> Error "phase 1: multi-imm (follow-up)"
        | (Imm_spec | Imm_reg), _ -> Error "phase 1: multi-imm (follow-up)"))

(* Flag-reader intrinsics (kortest/ktest z/c) map to a curated [Simd.Seq]
   pseudo-op: the KORTEST/KTEST instruction followed by a SETcc. The mask width
   comes from the chosen instruction's mnemonic binding. *)
let emit_flag_reader i =
  match choose_instruction i with
  | None -> Error "phase 1b: flag-reader (no instruction)"
  | Some instr -> (
    let flavor =
      if contains ~needle:"kortestz" i.name
      then Some "kortestz"
      else if contains ~needle:"kortestc" i.name
      then Some "kortestc"
      else if contains ~needle:"ktestz" i.name
      then Some "ktestz"
      else if contains ~needle:"ktestc" i.name
      then Some "ktestc"
      else None
    in
    match flavor with
    | None -> Error "phase 1b: flag-reader (unknown flavor)"
    | Some flavor ->
      Ok
        (sprintf
           "(match args with [x0; x1] -> Sel.%s %s [x0; x1] | _ -> \
            Sel.bad_arity             op)"
           flavor instr.mnemonic))

(* -------------------------------------------------------------------------- *)
(* Disposition of each in-scope intrinsic *)
(* -------------------------------------------------------------------------- *)

type disposition =
  | Emit_register of
      { name : string; (* caml_<intel-name> *)
        body : string (* generated arm body *)
      }
  | Skip of string

let has_out_pointer i =
  List.exists (fun p -> String.contains p.ctype '*') i.params

let disposition ~by_mnem i : disposition =
  if is_flag_reader_name i.name
  then
    if contains ~needle:"comi" i.name
    then Skip "flag-reader: comi predicate+sae (curated follow-up)"
    else if has_out_pointer i
    then Skip "flag-reader with out-pointer (composable from z/c variants)"
    else
      match emit_flag_reader i with
      | Ok body -> Emit_register { name = caml_name i; body }
      | Error reason -> Skip reason
  else if String.equal i.tech "SVML"
  then Skip "SVML (library-level)"
  else if i.sequence
  then Skip "sequence (library-level)"
  else
    match classify i with
    | No_instruction -> Skip "no instruction (cast/undefined)"
    | Gather_scatter -> Skip "phase 3: gather/scatter"
    | Memory -> Skip "phase 2: memory form"
    | Register
      when contains ~needle:"cvt" i.name
           && (contains ~needle:"lo_pd" i.name || contains ~needle:"pslo" i.name)
      ->
      Skip "lo/hi convert variant (full-width arg, low/high half used)"
    | Register -> (
      match choose_instruction i with
      | None -> Skip "unhandled multi-instruction form"
      | Some instr -> (
        if List.mem instr.mnemonic omit_k_source_mnemonics
        then
          (* Special-18: XML omits the k source operand; match by mnemonic and
             vector width against the (single) k-taking binding. *)
          let cand =
            Hashtbl.find_opt by_mnem instr.mnemonic
            |> Option.value ~default:[]
            |> List.filter (fun b ->
                let xs = xml_shape_of_form instr.form in
                let seq, _, _ = instr_hw_seq b.instr in
                (* binding seq is [vector; K]; XML seq is [vector] *)
                match seq with
                | [v; "K"] -> classes_match xs.classes [v]
                | _ -> false)
          in
          match cand with
          | [binding] -> (
            (* The k is a data source (RM_rm), not a write mask, so the normal
               emitter handles it as a unary op. *)
            match emit_register_arm ~by_mnem ~binding ~zeroing:false i with
            | Ok body -> Emit_register { name = caml_name i; body }
            | Error reason -> Skip reason)
          | _ -> Skip "no matching instruction descriptor (amd64.csv gap)"
        else
          match match_register ~by_mnem i instr with
          | Matched { binding; zeroing } -> (
            match emit_register_arm ~by_mnem ~binding ~zeroing i with
            | Ok body -> Emit_register { name = caml_name i; body }
            | Error reason -> Skip reason)
          | No_binding ->
            Skip "no matching instruction descriptor (amd64.csv gap)"))

(* -------------------------------------------------------------------------- *)
(* Coverage report and skip list *)
(* -------------------------------------------------------------------------- *)

let build_by_mnem (bindings : (string * instr_emit) list) =
  let tbl = Hashtbl.create 1024 in
  List.iter
    (fun (bname, (instr : instr_emit)) ->
      let mnem = instr.mnemonic in
      let prev = Hashtbl.find_opt tbl mnem |> Option.value ~default:[] in
      Hashtbl.replace tbl mnem ({ bname; instr } :: prev))
    bindings;
  tbl

let dispositions ~bindings intrinsics =
  let by_mnem = build_by_mnem bindings in
  List.filter_map
    (fun i -> if in_scope i then Some (i, disposition ~by_mnem i) else None)
    intrinsics

(* -------------------------------------------------------------------------- *)
(* Generated C-oracle tests                                                    *)
(* -------------------------------------------------------------------------- *)

(* OCaml type for a 512-bit-vector / mask parameter, or [None] if the test
   generator does not cover this shape (imm, 128/256, or scalar). *)
let test_oty (p : param) =
  match p.ctype with
  | "__m512" -> Some "float32x16"
  | "__m512d" -> Some "float64x8"
  | "__m512i" -> (
    match p.etype with
    | "UI8" | "SI8" | "M8" -> Some "int8x64"
    | "UI16" | "SI16" -> Some "int16x32"
    | "UI64" | "SI64" -> Some "int64x8"
    | _ -> Some "int32x16")
  | "__mmask8" | "__mmask16" | "__mmask32" | "__mmask64" -> Some "mask"
  | _ -> None

let is_mask_ctype c = String.length c >= 7 && String.sub c 0 7 = "__mmask"

(* An emitted intrinsic is testable here when it has no immediate and every
   parameter and the return value is a 512-bit vector or a mask. *)
let testable i =
  List.for_all (fun p -> not (is_imm p)) i.params
  && Option.is_some (test_oty i.ret)
  && List.for_all (fun p -> Option.is_some (test_oty p)) i.params
  && i.params <> []

let test_intrinsics ~bindings intrinsics =
  dispositions ~bindings intrinsics
  |> List.filter_map (fun (i, d) ->
      match d with Emit_register _ when testable i -> Some i | _ -> None)
  |> List.sort_uniq (fun a b -> String.compare a.name b.name)

let tests_ml_preamble =
  {ml|(* Generated by tools/simdgen/simdgen_intrins.ml: bit-for-bit checks of each
   emitted register intrinsic against the real C intrinsic. *)

open Stdlib

external of_w :
  int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64
  -> int64x8 = "" "vec512_of_int64s"
[@@noalloc] [@@unboxed]

external reint : 'a -> 'b = "%identity"

external w : (int64x8[@unboxed]) -> (int[@untagged]) -> (int64[@unboxed])
  = "" "vec512_wi"
[@@noalloc]

external mask_of_int64 : int64 -> mask
  = "caml_vec512_unreachable" "caml_mask_of_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external int64_of_mask : mask -> int64
  = "caml_vec512_unreachable" "caml_int64_of_mask"
[@@noalloc] [@@unboxed] [@@builtin]

let failures = ref 0

let check512 name a b =
  let a : int64x8 = reint a
  and b : int64x8 = reint b in
  let ok = ref true in
  for i = 0 to 7 do
    if not (Int64.equal (w a i) (w b i)) then ok := false
  done;
  if not !ok then (
    incr failures;
    Printf.printf "MISMATCH %s\n" name)

let check_mask name (a : mask) (b : mask) =
  if not (Int64.equal (int64_of_mask a) (int64_of_mask b)) then (
    incr failures;
    Printf.printf "MISMATCH %s: %Lx <> %Lx\n" name (int64_of_mask a)
      (int64_of_mask b))

let va = of_w 0x3f8000004048f5c3L 0xbff0000040a00000L 0x0000000100000002L
    0xfffffffe7fffffffL 0x8000000012345678L 0x40490fdbc0000000L
    0x0102030405060708L 0x1122334455667788L

let vb = of_w 0x4000000040400000L 0x3fc00000c1200000L 0x00000003fffffffdL
    0x000000057fffffffL 0x9abcdef0deadbeefL 0x3ff0000040000000L
    0x0807060504030201L 0x8877665544332211L

let vc = of_w 0x41000000c1a00000L 0x4048f5c33f800000L 0x0000000600000007L
    0xaaaaaaaa55555555L 0x0f0f0f0ff0f0f0f0L 0x400921fb54442d18L
    0xdeadbeefcafebabeL 0x0123456789abcdefL

|ml}

(* Renders the OCaml value expression passed for the [n]-th operand of the given
   role, and the C argument name. *)
let tests_ml ~bindings intrinsics =
  let buf = Buffer.create 65536 in
  Buffer.add_string buf tests_ml_preamble;
  let vecs = [| "va"; "vb"; "vc" |] in
  List.iter
    (fun i ->
      let name = caml_name i in
      let oty p = Option.get (test_oty p) in
      let sig_ =
        String.concat " -> "
          (List.map (fun p -> sprintf "(%s[@unboxed])" (oty p)) i.params
          @ [sprintf "(%s[@unboxed])" (oty i.ret)])
      in
      Buffer.add_string buf
        (sprintf
           "\n\
            external %s : %s = \"caml_vec512_unreachable\" %S [@@noalloc] \
            [@@builtin]\n"
           name sig_ name);
      Buffer.add_string buf
        (sprintf "external c_%s : %s = \"\" \"ctest_%s\" [@@noalloc]\n" i.name
           sig_ i.name);
      let vi = ref 0 and mi = ref 0 in
      let mask_width c =
        match c with
        | "__mmask8" -> 0xFFL
        | "__mmask16" -> 0xFFFFL
        | "__mmask32" -> 0xFFFFFFFFL
        | _ -> -1L (* __mmask64: all bits *)
      in
      let args =
        List.map
          (fun p ->
            if is_mask_ctype p.ctype
            then (
              (* Pass a mask valid for this parameter width: the C oracle reads
                 the full register, so upper bits must be zero. *)
              let pat =
                if !mi mod 2 = 0
                then 0xA5A5A5A5A5A5A5A5L
                else 0x3C3C3C3C3C3C3C3CL
              in
              incr mi;
              sprintf "(mask_of_int64 0x%LxL)"
                (Int64.logand pat (mask_width p.ctype)))
            else
              let a = sprintf "(reint %s)" vecs.(!vi mod 3) in
              incr vi;
              a)
          i.params
        |> String.concat " "
      in
      let chk =
        if is_mask_ctype i.ret.ctype then "check_mask" else "check512"
      in
      Buffer.add_string buf
        (sprintf "let () = %s %S (%s %s) (c_%s %s)\n" chk i.name name args
           i.name args))
    (test_intrinsics ~bindings intrinsics);
  Buffer.add_string buf "\nlet () = if !failures <> 0 then exit 1\n";
  print_string (Buffer.contents buf)

let tests_c ~bindings intrinsics =
  let buf = Buffer.create 65536 in
  Buffer.add_string buf
    "/* Generated by tools/simdgen/simdgen_intrins.ml. */\n\
     #include <caml/simd.h>\n\
     #include <assert.h>\n\
     #include <stdint.h>\n\n\
     #define BUILTIN(name) void name() { assert(0); }\n\n\
     BUILTIN(caml_vec512_unreachable);\n\
     BUILTIN(caml_mask_of_int64);\n\
     BUILTIN(caml_int64_of_mask);\n";
  let intrs = test_intrinsics ~bindings intrinsics in
  List.iter
    (fun i -> Buffer.add_string buf (sprintf "BUILTIN(caml%s);\n" i.name))
    intrs;
  Buffer.add_string buf
    "\n\
     #ifdef ARCH_AVX512\n\
     #include <immintrin.h>\n\n\
     static int64_t vec512_extract(__m512i v, int i) { int64_t t[8]; \
     _mm512_storeu_si512((void *)t, v); return t[i]; }\n\
     int64_t vec512_wi(__m512i v, int i) { return vec512_extract(v, i); }\n\
     __m512i vec512_of_int64s(int64_t a, int64_t b, int64_t c, int64_t d, \
     int64_t e, int64_t f, int64_t g, int64_t h) { return _mm512_set_epi64(h, \
     g, f, e, d, c, b, a); }\n\n";
  List.iter
    (fun i ->
      let params =
        List.mapi (fun j p -> sprintf "%s p%d" p.ctype j) i.params
        |> String.concat ", "
      in
      let args =
        List.mapi (fun j _ -> sprintf "p%d" j) i.params |> String.concat ", "
      in
      let cret =
        if is_mask_ctype i.ret.ctype then "__mmask64" else i.ret.ctype
      in
      Buffer.add_string buf
        (sprintf "%s ctest_%s(%s) { return %s(%s); }\n" cret i.name params
           i.name args))
    intrs;
  Buffer.add_string buf "\n#endif\n";
  print_string (Buffer.contents buf)

let report ~bindings intrinsics =
  let ds = dispositions ~bindings intrinsics in
  let emitted = ref 0 and skipped = ref 0 in
  let skip_reasons = Hashtbl.create 32 in
  List.iter
    (fun (_, d) ->
      match d with
      | Emit_register _ -> incr emitted
      | Skip reason ->
        incr skipped;
        Hashtbl.replace skip_reasons reason
          (1 + (Hashtbl.find_opt skip_reasons reason |> Option.value ~default:0)))
    ds;
  eprintf "AVX512 intrinsics coverage (in scope: %d)\n" (List.length ds);
  eprintf "  emitted: %d\n" !emitted;
  eprintf "  skipped: %d\n" !skipped;
  Hashtbl.fold (fun r n acc -> (r, n) :: acc) skip_reasons []
  |> List.sort (fun (_, a) (_, b) -> compare b a)
  |> List.iter (fun (r, n) -> eprintf "    %5d  %s\n" n r);
  eprintf "  total accounted: %d\n" (!emitted + !skipped)

let print_dump ~bindings intrinsics =
  let ds = dispositions ~bindings intrinsics in
  List.iter
    (fun (_i, d) ->
      match d with
      | Emit_register { name; body } -> printf "%s\t%s\n" name body
      | Skip _ -> ())
    ds

let selection_preamble =
  {ocaml|(* Generated by tools/simdgen/simdgen_intrins.ml. *)

[@@@ocaml.warning "+a-4-40-42-70"]

open! Amd64_simd_instrs

module type Sel = sig
  type expr

  type result

  val instr : Amd64_simd_instrs.instr -> ?i:int -> expr list -> result

  val none : result

  val bad_arity : string -> result

  val bad_immediate : string -> result

  val extract_constant : expr list -> max:int -> string -> int * expr list

  val kortestz : Amd64_simd_instrs.instr -> expr list -> result

  val kortestc : Amd64_simd_instrs.instr -> expr list -> result

  val ktestz : Amd64_simd_instrs.instr -> expr list -> result

  val ktestc : Amd64_simd_instrs.instr -> expr list -> result
end

module Make (Sel : Sel) = struct
  let select_operation op (args : Sel.expr list) : Sel.result =
    match op with|ocaml}

let print_selection ~bindings intrinsics =
  let ds = dispositions ~bindings intrinsics in
  let arms =
    List.filter_map
      (fun (_, d) ->
        match d with
        | Emit_register { name; body } -> Some (name, body)
        | Skip _ -> None)
      ds
    |> List.sort_uniq (fun (a, _) (b, _) -> String.compare a b)
  in
  print_string selection_preamble;
  List.iter (fun (name, body) -> printf "\n    | %S -> %s" name body) arms;
  print_string "\n    | _ -> Sel.none\nend\n"

let print_skiplist ~bindings intrinsics =
  let ds = dispositions ~bindings intrinsics in
  printf "# AVX512 intrinsics skip list (generated by simdgen_intrins).\n";
  printf "# Every in-scope intrinsic is either emitted or listed here with a\n";
  printf
    "# reason. Regenerate with the [intrins-skiplist] simdgen subcommand.\n";
  ds
  |> List.filter_map (fun (i, d) ->
      match d with Skip reason -> Some (i.name, reason) | _ -> None)
  |> List.sort compare
  |> List.iter (fun (name, reason) -> printf "%s\t%s\n" name reason)
