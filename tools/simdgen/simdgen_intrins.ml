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
  | None -> Error "unhandled operand shape"
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
      then Error "embedded-rounding form with unexpected immediates (follow-up)"
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
      then Error "rounding-control immediate on sae instruction (follow-up)"
      else
        match real_imm_prefix () with
        | None -> Error "more than two immediates (follow-up)"
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
      then Error "sae immediate on non-sae binding (follow-up)"
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
          | None -> Error "more than two immediates (follow-up)"
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
          | None -> Error "unhandled hardcoded-predicate form")
        | Imm_none, _ :: _ ->
          Error "immediate parameter but instruction takes no immediate"
        | (Imm_spec | Imm_reg), _ ->
          Error "more than two immediates (follow-up)"))

(* Flag-reader intrinsics (kortest/ktest z/c) map to a curated [Simd.Seq]
   pseudo-op: the KORTEST/KTEST instruction followed by a SETcc. The mask width
   comes from the chosen instruction's mnemonic binding. *)
let emit_flag_reader ~by_mnem i =
  match choose_instruction i with
  | None -> Error "flag-reader without an instruction"
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
    | None -> Error "flag-reader with unknown flavor"
    | Some flavor -> (
      match Hashtbl.find_opt by_mnem instr.mnemonic with
      | Some [binding] ->
        Ok
          (sprintf
             "(match args with [x0; x1] -> Sel.%s %s [x0; x1] | _ -> \
              Sel.bad_arity op)"
             flavor binding.bname)
      | Some (_ :: _ :: _) | Some [] | None ->
        Error "no matching instruction descriptor (amd64.csv gap)"))

(* -------------------------------------------------------------------------- *)
(* Memory-form matching and emission (loads/stores) *)
(* -------------------------------------------------------------------------- *)

let mem_token tok =
  match strip_modifiers tok with
  | "xmm" -> Some "XMM"
  | "ymm" -> Some "YMM"
  | "zmm" -> Some "ZMM"
  | "k" -> Some "K"
  | "m8" | "m16" | "m32" | "m64" | "m128" | "m256" | "m512" -> Some "MEM"
  | "r32" -> Some "R32"
  | "r64" -> Some "R64"
  | "imm8" -> None
  | other -> Some ("?" ^ other)

(* Tokens a binding operand can match: its register class and/or MEM. *)
let op_accepts (loc : loc) =
  (match loc_regclass loc with Some c -> [c] | None -> [])
  @ if loc_allows_mem loc then ["MEM"] else []

(* Hardware operand order (dest first) as [loc]s, excluding the write mask. *)
let mem_hw_locs (instr : instr_emit) =
  let value_args =
    Array.to_list instr.args |> List.filter arg_is_value
    |> List.map (fun (a : arg) -> a.loc)
  in
  match instr.res with
  | Res rr ->
    Array.to_list rr |> List.map (fun (a : arg) -> a.loc) |> fun d ->
    d @ value_args
  | Arg _ | Res_none -> value_args

let is_ptr_param p = String.contains p.ctype '*'

(* Match a memory-form intrinsic to its binding. Returns [Some (binding,
   is_load)]. *)
let match_memory ~by_mnem i instr =
  let xs = xml_shape_of_form instr.form in
  if
    contains ~needle:"gather" instr.mnemonic
    || contains ~needle:"scatter" instr.mnemonic
  then None
  else
    let xml_tokens = List.filter_map mem_token (form_tokens instr.form) in
    let is_load =
      String.length i.ret.ctype >= 3 && String.sub i.ret.ctype 0 3 = "__m"
    in
    let one_mem =
      List.length (List.filter (String.equal "MEM") xml_tokens) = 1
    in
    if not one_mem
    then None
    else
      let cands =
        Hashtbl.find_opt by_mnem instr.mnemonic
        |> Option.value ~default:[]
        |> List.filter (fun b ->
            let has_mask =
              Array.exists
                (fun (a : arg) -> match a.enc with Mask -> true | _ -> false)
                b.instr.args
            in
            let locs = mem_hw_locs b.instr in
            let res_has_mem =
              match b.instr.res with
              | Res rr ->
                Array.exists (fun (a : arg) -> loc_allows_mem a.loc) rr
              | Arg _ | Res_none -> false
            in
            Bool.equal has_mask xs.masked
            (* [Isimd_mem] can only address a memory operand that lives in the
               argument list, so reject bindings whose result is the memory. *)
            && (not res_has_mem)
            && List.length locs = List.length xml_tokens
            && List.for_all2
                 (fun tok loc -> List.mem tok (op_accepts loc))
                 xml_tokens locs)
      in
      match cands with [b] -> Some (b, is_load) | _ -> None

(* Emit a load/store arm: walk the binding operands, sending the pointer C arg
   to the memory operand, the mask C arg to the write mask, and vector C args to
   the register operands, then build an [Isimd_mem] load or store. *)
let emit_mem_arm ~binding ~is_load ~zeroing i =
  let params = i.params in
  let tagged = List.mapi (fun j p -> p, sprintf "x%d" j) params in
  let names = List.map snd tagged in
  let ptr =
    List.find_map (fun (p, n) -> if is_ptr_param p then Some n else None) tagged
  in
  let masks =
    List.filter_map
      (fun (p, n) -> if is_mask_param p then Some n else None)
      tagged
  in
  let vectors =
    List.filter_map
      (fun (p, n) ->
        if (not (is_ptr_param p)) && (not (is_mask_param p)) && not (is_imm p)
        then Some n
        else None)
      tagged
  in
  match ptr with
  | None -> Error "memory-form instruction without a pointer argument"
  | Some ptr ->
    let mask = ref masks and vec = ref vectors in
    let pop r =
      match !r with
      | x :: t ->
        r := t;
        Some x
      | [] -> None
    in
    let stored =
      Array.to_list binding.instr.args
      |> List.filter (fun (a : arg) -> not (arg_is_implicit a))
    in
    (* The merge (~z:false) variant prepends the destination as a source; the
       stored operand list (shared with ~z:true) omits it. *)
    let operand_locs =
      if binding.instr.flags.z && not zeroing
      then
        match binding.instr.res with
        | Res rr -> Array.to_list rr @ stored
        | Arg _ | Res_none -> stored
      else stored
    in
    let operands =
      List.map
        (fun (a : arg) ->
          if loc_allows_mem a.loc
          then Some ptr
          else match a.enc with Mask -> pop mask | _ -> pop vec)
        operand_locs
    in
    if List.exists Option.is_none operands || !mask <> [] || !vec <> []
    then Error "memory form with unhandled operand shape (follow-up)"
    else
      let arglist =
        "[" ^ String.concat "; " (List.filter_map Fun.id operands) ^ "]"
      in
      let has_z = binding.instr.flags.z in
      let bind_expr =
        if has_z
        then sprintf "(%s ~z:%b)" binding.bname zeroing
        else binding.bname
      in
      let pat = "[" ^ String.concat "; " names ^ "]" in
      let f = if is_load then "Sel.simd_load" else "Sel.simd_store" in
      Ok
        (sprintf "(match args with %s -> %s %s %s | _ -> Sel.bad_arity op)" pat
           f bind_expr arglist)

(* -------------------------------------------------------------------------- *)
(* Gather/scatter matching and emission *)
(* -------------------------------------------------------------------------- *)

(* Operand class for VSIB matching: VM temps keep their exact name. *)
let gs_loc_class (loc : loc) =
  match loc with
  | Pin _ -> None
  | Temp temps ->
    Array.find_map
      (fun t ->
        match t with
        | VM32X -> Some "VM32X"
        | VM32Y -> Some "VM32Y"
        | VM32Z -> Some "VM32Z"
        | VM64X -> Some "VM64X"
        | VM64Y -> Some "VM64Y"
        | VM64Z -> Some "VM64Z"
        | _ -> temp_regclass t)
      temps

let gs_token tok =
  match strip_modifiers tok with
  | "xmm" -> Some "XMM"
  | "ymm" -> Some "YMM"
  | "zmm" -> Some "ZMM"
  | ("vm32x" | "vm32y" | "vm32z" | "vm64x" | "vm64y" | "vm64z") as vm ->
    Some (String.uppercase_ascii vm)
  | _ -> None

(* AVX512 gathers/scatters have a single (mandatory-mask) binding per width. The
   XML forms are unreliable here (several i64gather/scatter entries carry the
   wrong vm token or destination width), so derive the match key from the C
   types and the mnemonic instead: index width from the d/q in the mnemonic,
   register classes from the C return/parameter types. *)
let match_gs ~by_mnem i instr =
  let is_gather =
    String.length i.ret.ctype >= 3 && String.sub i.ret.ctype 0 3 = "__m"
  in
  let non_imm = List.filter (fun p -> not (is_imm p)) i.params in
  let vec_classes =
    List.filter_map
      (fun p ->
        if (not (is_ptr_param p)) && not (is_mask_param p)
        then ctype_regclass p.ctype
        else None)
      non_imm
  in
  let index_width =
    (* vpgatherDd vs vpgatherQd etc.: the letter after "gather"/"scatter" gives
       the index element width. *)
    let mnem = instr.mnemonic in
    let after key =
      let rec go i =
        if i + String.length key > String.length mnem
        then None
        else if String.sub mnem i (String.length key) = key
        then Some mnem.[i + String.length key]
        else go (i + 1)
      in
      go 0
    in
    match after "gather", after "scatter" with
    | Some c, _ | _, Some c -> (
      match c with 'd' -> Some "32" | 'q' -> Some "64" | _ -> None)
    | None, None -> None
  in
  let vm w idx = "VM" ^ w ^ String.sub idx 0 1 in
  let expected =
    match index_width, is_gather, vec_classes, ctype_regclass i.ret.ctype with
    (* gather masked: (src, vindex); plain: (vindex) -- dst from return type *)
    | Some w, true, ([idx] | [_; idx]), Some dst -> Some [dst; vm w idx]
    (* scatter: (vindex, data) *)
    | Some w, false, [idx; data], _ -> Some [vm w idx; data]
    | _ -> None
  in
  match expected with
  | None -> None
  | Some expected -> (
    let cands =
      Hashtbl.find_opt by_mnem instr.mnemonic
      |> Option.value ~default:[]
      |> List.filter (fun b ->
          let value_classes =
            Array.to_list b.instr.args |> List.filter arg_is_value
            |> List.filter_map (fun (a : arg) -> gs_loc_class a.loc)
          in
          try List.for_all2 String.equal expected value_classes
          with Invalid_argument _ -> false)
    in
    match cands with [b] -> Some b | _ -> None)

(* Emit a gather/scatter arm. The C scale immediate becomes the addressing-mode
   scale; unmasked variants synthesize an all-ones mask (and, for gathers, a
   zero destination) since the AVX512 instructions are mask-only. *)
let emit_gs_arm ~binding i =
  let is_gather =
    String.length i.ret.ctype >= 3 && String.sub i.ret.ctype 0 3 = "__m"
  in
  let non_imm = List.filter (fun p -> not (is_imm p)) i.params in
  let names = List.mapi (fun j p -> p, sprintf "x%d" j) non_imm in
  let find f =
    List.find_map (fun (p, n) -> if f p then Some n else None) names
  in
  let vectors =
    List.filter_map
      (fun (p, n) -> if is_ptr_param p || is_mask_param p then None else Some n)
      names
  in
  let base = find is_ptr_param in
  let kexpr =
    match find is_mask_param with Some k -> k | None -> "Sel.all_ones_mask"
  in
  let zero_dst () =
    match gs_loc_class binding.instr.args.(0).loc with
    | Some "XMM" -> Some "Sel.zero_vec128"
    | Some "YMM" -> Some "Sel.zero_vec256"
    | Some "ZMM" -> Some "Sel.zero_vec512"
    | _ -> None
  in
  match base, is_gather, vectors with
  | Some base, true, [src; vindex] ->
    Ok
      (sprintf
         "(let scale, args = Sel.extract_scale args op in match args with [%s] \
          -> Sel.simd_load_scaled %s ~scale [%s; %s; %s; %s] | _ -> \
          Sel.bad_arity op)"
         (String.concat "; " (List.map snd names))
         binding.bname src base vindex kexpr)
  | Some base, true, [vindex] -> (
    match zero_dst () with
    | None -> Error "unknown gather destination width"
    | Some zero ->
      Ok
        (sprintf
           "(let scale, args = Sel.extract_scale args op in match args with \
            [%s] -> Sel.simd_load_scaled %s ~scale [%s; %s; %s; %s] | _ -> \
            Sel.bad_arity op)"
           (String.concat "; " (List.map snd names))
           binding.bname zero base vindex kexpr))
  | Some base, false, [vindex; data] ->
    Ok
      (sprintf
         "(let scale, args = Sel.extract_scale args op in match args with [%s] \
          -> Sel.simd_store_scaled %s ~scale [%s; %s; %s; %s] | _ -> \
          Sel.bad_arity op)"
         (String.concat "; " (List.map snd names))
         binding.bname base vindex data kexpr)
  | _ -> Error "unhandled gather/scatter shape"

(* -------------------------------------------------------------------------- *)
(* Disposition of each in-scope intrinsic *)
(* -------------------------------------------------------------------------- *)

type disposition =
  | Emit_register of
      { name : string; (* caml_<intel-name> *)
        body : string; (* generated arm body *)
        er : bool (* embedded-rounding arm (tests enumerate imms 8-11) *)
      }
  | Skip of string

let has_out_pointer i =
  List.exists (fun p -> String.contains p.ctype '*') i.params

(* Whether the emitted arm dispatches on an embedded-rounding immediate;
   recorded so the generated tests enumerate the matching values. *)
let binding_er binding =
  match binding.instr.flags.r with
  | Rnd_er -> true
  | Rnd_sae | Rnd_none -> false

let is_lo_convert i =
  contains ~needle:"cvt" i.name
  && (contains ~needle:"lo_pd" i.name || contains ~needle:"pslo" i.name)

let disposition ~by_mnem i : disposition =
  if is_flag_reader_name i.name
  then
    if contains ~needle:"comi" i.name
    then Skip "flag-reader: comi predicate+sae (curated follow-up)"
    else if has_out_pointer i
    then Skip "flag-reader with out-pointer (composable from z/c variants)"
    else
      match emit_flag_reader ~by_mnem i with
      | Ok body -> Emit_register { name = caml_name i; body; er = false }
      | Error reason -> Skip reason
  else if String.equal i.tech "SVML"
  then Skip "SVML (library-level)"
  else if i.sequence
  then Skip "sequence (library-level)"
  else
    match classify i with
    | No_instruction -> Skip "no instruction (cast/undefined)"
    | Gather_scatter -> (
      match choose_instruction i with
      | None -> Skip "unhandled multi-instruction gather/scatter"
      | Some instr -> (
        match match_gs ~by_mnem i instr with
        | None -> Skip "no matching gather/scatter descriptor"
        | Some binding -> (
          match emit_gs_arm ~binding i with
          | Ok body -> Emit_register { name = caml_name i; body; er = false }
          | Error reason -> Skip reason)))
    | Memory
      when contains ~needle:"logather" i.name
           || contains ~needle:"loscatter" i.name ->
      Skip "lo gather/scatter variant (512-bit index arg; compose via cast)"
    | Memory -> (
      match choose_instruction i with
      | None -> Skip "unhandled multi-instruction memory form"
      | Some instr -> (
        match match_memory ~by_mnem i instr with
        | None -> Skip "memory form without a register-addressable descriptor"
        | Some (binding, is_load) -> (
          let zeroing = (xml_shape_of_form instr.form).zeroing in
          match emit_mem_arm ~binding ~is_load ~zeroing i with
          | Ok body -> Emit_register { name = caml_name i; body; er = false }
          | Error reason -> Skip reason)))
    | Register when is_lo_convert i ->
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
            | Ok body ->
              let er = binding_er binding in
              Emit_register { name = caml_name i; body; er }
            | Error reason -> Skip reason)
          | _ -> Skip "no matching instruction descriptor (amd64.csv gap)"
        else
          match match_register ~by_mnem i instr with
          | Matched { binding; zeroing } -> (
            match emit_register_arm ~by_mnem ~binding ~zeroing i with
            | Ok body ->
              let er = binding_er binding in
              Emit_register { name = caml_name i; body; er }
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
(* Generated C-oracle tests *)
(* -------------------------------------------------------------------------- *)

(* Kind of a testable C parameter/return value. *)
type tkind =
  | Vec of int (* 128 / 256 / 512 *)
  | MaskT
  | I32
  | I64
  | ISub (* sub-word integer, passed untagged *)

let tkind_of_ctype ~etype ctype =
  match ctype with
  | "__m512" -> Some (Vec 512, "float32x16")
  | "__m512d" -> Some (Vec 512, "float64x8")
  | "__m512i" ->
    Some
      ( Vec 512,
        match etype with
        | "UI8" | "SI8" | "M8" -> "int8x64"
        | "UI16" | "SI16" -> "int16x32"
        | "UI64" | "SI64" -> "int64x8"
        | _ -> "int32x16" )
  | "__m256" -> Some (Vec 256, "float32x8")
  | "__m256d" -> Some (Vec 256, "float64x4")
  | "__m256i" ->
    Some
      ( Vec 256,
        match etype with
        | "UI8" | "SI8" | "M8" -> "int8x32"
        | "UI16" | "SI16" -> "int16x16"
        | "UI64" | "SI64" -> "int64x4"
        | _ -> "int32x8" )
  | "__m128" -> Some (Vec 128, "float32x4")
  | "__m128d" -> Some (Vec 128, "float64x2")
  | "__m128i" ->
    Some
      ( Vec 128,
        match etype with
        | "UI8" | "SI8" | "M8" -> "int8x16"
        | "UI16" | "SI16" -> "int16x8"
        | "UI64" | "SI64" -> "int64x2"
        | _ -> "int32x4" )
  | "__mmask8" | "__mmask16" | "__mmask32" | "__mmask64" -> Some (MaskT, "mask")
  | "int" | "unsigned int" | "const int" -> Some (I32, "int32")
  | "__int64" | "unsigned __int64" | "long long" | "unsigned long long" ->
    Some (I64, "int64")
  | "char" | "unsigned char" | "short" | "unsigned short" -> Some (ISub, "int")
  | _ -> None

let tkind_of (p : param) = tkind_of_ctype ~etype:p.etype p.ctype

let oty_with_attr (k, oty) =
  match k with
  | ISub -> sprintf "(%s[@untagged])" oty
  | _ -> sprintf "(%s[@unboxed])" oty

let is_mask_ctype c = String.length c >= 7 && String.sub c 0 7 = "__mmask"

(* An emitted intrinsic is testable when every non-imm parameter and the return
   value map to a register-passable kind (memory forms and gathers/scatters are
   excluded by their pointer argument and are covered by hand-written tests). *)
let testable i =
  Option.is_some (tkind_of i.ret)
  && List.for_all (fun p -> is_imm p || Option.is_some (tkind_of p)) i.params
  && List.exists (fun p -> not (is_imm p)) i.params

let test_intrinsics ~bindings intrinsics =
  dispositions ~bindings intrinsics
  |> List.filter_map (fun (i, d) ->
      match d with
      | Emit_register { er; _ } when testable i -> Some (i, er)
      | _ -> None)
  |> List.sort_uniq (fun (a, _) (b, _) -> String.compare a.name b.name)

(* Values enumerated for an immediate parameter. Rounding immediates follow the
   dispatch in the generated arm: embedded-rounding arms accept the four
   [_MM_FROUND_TO_*|_MM_FROUND_NO_EXC] combinations plus CUR_DIRECTION, sae arms
   NO_EXC plus CUR_DIRECTION. *)
let imm_values ~er (p : param) =
  match p.immtype with
  | Some "_CMP_" -> List.init 32 (fun v -> v)
  | Some "_MM_CMPINT" -> List.init 8 (fun v -> v)
  | Some "_MM_FROUND" -> if er then [8; 9; 10; 11; 4] else [8; 4]
  | Some "_MM_FROUND_SAE" -> [8; 4]
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

(* OCaml expression for the [idx]-th value argument of the given kind. Masks are
   truncated to the parameter width (the OCaml<->C mask ABI passes the full
   register, so upper bits must be zero for the C oracle). *)
let value_expr ~idx (p : param) k =
  match k with
  | Vec 512 -> sprintf "(reint v%c)" "abc".[idx mod 3]
  | Vec 256 -> sprintf "(reint v%c256)" "abc".[idx mod 3]
  | Vec 128 -> sprintf "(reint v%c128)" "abc".[idx mod 3]
  | Vec _ -> assert false
  | MaskT ->
    let pat =
      if idx mod 2 = 0 then 0xA5A5A5A5A5A5A5A5L else 0x3C3C3C3C3C3C3C3CL
    in
    let width =
      match p.ctype with
      | "__mmask8" -> 0xFFL
      | "__mmask16" -> 0xFFFFL
      | "__mmask32" -> 0xFFFFFFFFL
      | _ -> -1L
    in
    sprintf "(mask_of_int64 0x%LxL)" (Int64.logand pat width)
  | I32 -> [| "0x12345678l"; "(-7l)"; "0x40000001l" |].(idx mod 3)
  | I64 ->
    [| "0x1122334455667788L"; "(-9L)"; "0x4000000000000001L" |].(idx mod 3)
  | ISub -> [| "23"; "113"; "5" |].(idx mod 3)

let check_fn (k, _) =
  match k with
  | Vec 512 -> "check512"
  | Vec 256 -> "check256"
  | Vec 128 -> "check128"
  | Vec _ -> assert false
  | MaskT -> "check_mask"
  | I32 -> "check_i32"
  | I64 -> "check_i64"
  | ISub -> "check_int"

(* clang (checked at 21.1) miscompiles the masked scalar sqrt intrinsics when
   the rounding is CUR_DIRECTION (including the non-round forms): the DAG path
   swaps the [a]/[b] operands, computing sqrt(a) with the upper bits from [b],
   contradicting the Intel pseudocode and clang's own constant folder. Our
   selection follows the spec, so skip the C-oracle comparison there. *)
let oracle_bug i tuple =
  match i.name with
  | "_mm_mask_sqrt_ss" | "_mm_maskz_sqrt_ss" | "_mm_mask_sqrt_sd"
  | "_mm_maskz_sqrt_sd" ->
    true
  | "_mm_mask_sqrt_round_ss" | "_mm_maskz_sqrt_round_ss"
  | "_mm_mask_sqrt_round_sd" | "_mm_maskz_sqrt_round_sd" -> (
    match tuple with [4] -> true | _ -> false)
  | _ -> false

let tests_ml_preamble =
  {ml|(* Generated by tools/simdgen/simdgen_intrins.ml: bit-for-bit checks
   of each emitted register intrinsic against the real C intrinsic. *)

open Stdlib

external of_w :
  int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64
  -> int64x8 = "" "vec512_of_int64s"
[@@noalloc] [@@unboxed]

external of_w256 : int64 -> int64 -> int64 -> int64 -> int64x4
  = "" "vec256_of_int64s"
[@@noalloc] [@@unboxed]

external of_w128 : int64 -> int64 -> int64x2 = "" "vec128_of_int64s"
[@@noalloc] [@@unboxed]

external reint : 'a -> 'b = "%identity"

external w : (int64x8[@unboxed]) -> (int[@untagged]) -> (int64[@unboxed])
  = "" "vec512_wi"
[@@noalloc]

external w256 : (int64x4[@unboxed]) -> (int[@untagged]) -> (int64[@unboxed])
  = "" "vec256_wi"
[@@noalloc]

external w128 : (int64x2[@unboxed]) -> (int[@untagged]) -> (int64[@unboxed])
  = "" "vec128_wi"
[@@noalloc]

external mask_of_int64 : int64 -> mask
  = "caml_vec512_unreachable" "caml_mask_of_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external int64_of_mask : mask -> int64
  = "caml_vec512_unreachable" "caml_int64_of_mask"
[@@noalloc] [@@unboxed] [@@builtin]

let failures = ref 0

let fail name =
  incr failures;
  Printf.printf "MISMATCH %s\n" name

let checkw n name a b =
  let ok = ref true in
  for i = 0 to n - 1 do
    if not (Int64.equal (w a i) (w b i)) then ok := false
  done;
  if not !ok then fail name

let check512 name a b = checkw 8 name (reint a) (reint b)

let check256 name a b =
  let a : int64x4 = reint a and b : int64x4 = reint b in
  let ok = ref true in
  for i = 0 to 3 do
    if not (Int64.equal (w256 a i) (w256 b i)) then ok := false
  done;
  if not !ok then fail name

let check128 name a b =
  let a : int64x2 = reint a and b : int64x2 = reint b in
  let ok = ref true in
  for i = 0 to 1 do
    if not (Int64.equal (w128 a i) (w128 b i)) then ok := false
  done;
  if not !ok then fail name

let check_mask name (a : mask) (b : mask) =
  if not (Int64.equal (int64_of_mask a) (int64_of_mask b)) then fail name

let check_i32 name (a : int32) (b : int32) =
  if not (Int32.equal a b) then fail name

let check_i64 name (a : int64) (b : int64) =
  if not (Int64.equal a b) then fail name

let check_int name (a : int) (b : int) = if a <> b then fail name

let va = of_w 0x3f8000004048f5c3L 0xbff0000040a00000L 0x0000000100000002L
    0xfffffffe7fffffffL 0x8000000012345678L 0x40490fdbc0000000L
    0x0102030405060708L 0x1122334455667788L

let vb = of_w 0x4000000040400000L 0x3fc00000c1200000L 0x00000003fffffffdL
    0x000000057fffffffL 0x9abcdef0deadbeefL 0x3ff0000040000000L
    0x0807060504030201L 0x8877665544332211L

let vc = of_w 0x41000000c1a00000L 0x4048f5c33f800000L 0x0000000600000007L
    0xaaaaaaaa55555555L 0x0f0f0f0ff0f0f0f0L 0x400921fb54442d18L
    0xdeadbeefcafebabeL 0x0123456789abcdefL

let va256 = of_w256 0x3f8000004048f5c3L 0xbff0000040a00000L
    0x0000000100000002L 0xfffffffe7fffffffL

let vb256 = of_w256 0x4000000040400000L 0x3fc00000c1200000L
    0x00000003fffffffdL 0x000000057fffffffL

let vc256 = of_w256 0x41000000c1a00000L 0x4048f5c33f800000L
    0x0000000600000007L 0xaaaaaaaa55555555L

let va128 = of_w128 0x3f8000004048f5c3L 0xbff0000040a00000L

let vb128 = of_w128 0x4000000040400000L 0x3fc00000c1200000L

let vc128 = of_w128 0x41000000c1a00000L 0x4048f5c33f800000L
|ml}

let tests_ml ~bindings intrinsics =
  let buf = Buffer.create 262144 in
  Buffer.add_string buf tests_ml_preamble;
  List.iter
    (fun (i, er) ->
      let name = caml_name i in
      let imms = List.filter is_imm i.params in
      let tuples = product (List.map (imm_values ~er) imms) in
      let all_bugged = List.for_all (oracle_bug i) tuples in
      if all_bugged
      then
        Buffer.add_string buf
          (sprintf "\n(* %s: C oracle miscompiled by clang; skipped. *)\n"
             i.name)
      else begin
        let vals = List.filter (fun p -> not (is_imm p)) i.params in
        let val_tys = List.map (fun p -> Option.get (tkind_of p)) vals in
        let ret_ty = Option.get (tkind_of i.ret) in
        (* builtin external: imm params leading (untagged), then value params *)
        let bsig =
          String.concat " -> "
            (List.map (fun _ -> "(int[@untagged])") imms
            @ List.map oty_with_attr val_tys
            @ [oty_with_attr ret_ty])
        in
        Buffer.add_string buf
          (sprintf
             "\n\
              external %s : %s = \"caml_vec512_unreachable\" %S [@@noalloc] \
              [@@builtin]\n"
             name bsig name);
        let csig =
          String.concat " -> "
            (List.map oty_with_attr val_tys @ [oty_with_attr ret_ty])
        in
        let val_exprs =
          List.mapi
            (fun idx (p, (k, _)) -> value_expr ~idx p k)
            (List.combine vals val_tys)
          |> String.concat " "
        in
        let chk = check_fn ret_ty in
        List.iter
          (fun tuple ->
            if oracle_bug i tuple
            then
              Buffer.add_string buf
                (sprintf "(* %s%s: C oracle miscompiled by clang; skipped. *)\n"
                   i.name
                   (match tuple with
                   | [] -> ""
                   | t -> "_" ^ String.concat "_" (List.map string_of_int t)))
            else begin
              let suffix =
                match tuple with
                | [] -> ""
                | t -> "_" ^ String.concat "_" (List.map string_of_int t)
              in
              Buffer.add_string buf
                (sprintf
                   "external c%s%s : %s = \"\" \"ctest%s%s\" [@@noalloc]\n"
                   i.name suffix csig i.name suffix);
              let imm_lits = List.map string_of_int tuple in
              Buffer.add_string buf
                (sprintf "let () = %s \"%s%s\" (%s %s%s%s) (c%s%s %s)\n" chk
                   i.name suffix name
                   (String.concat " " imm_lits)
                   (if imm_lits = [] then "" else " ")
                   val_exprs i.name suffix val_exprs)
            end)
          tuples
      end)
    (test_intrinsics ~bindings intrinsics);
  Buffer.add_string buf "\nlet () = if !failures <> 0 then exit 1\n";
  print_string (Buffer.contents buf)

let tests_c ~bindings intrinsics =
  let buf = Buffer.create 262144 in
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
    (fun (i, _) -> Buffer.add_string buf (sprintf "BUILTIN(caml%s);\n" i.name))
    intrs;
  Buffer.add_string buf
    "\n\
     #ifdef ARCH_AVX512\n\
     #include <immintrin.h>\n\n\
     static int64_t vec512_extract(__m512i v, int i) { int64_t t[8]; \
     _mm512_storeu_si512((void *)t, v); return t[i]; }\n\
     int64_t vec512_wi(__m512i v, int i) { return vec512_extract(v, i); }\n\
     int64_t vec256_wi(__m256i v, int i) { int64_t t[4]; \
     _mm256_storeu_si256((void *)t, v); return t[i]; }\n\
     int64_t vec128_wi(__m128i v, int i) { int64_t t[2]; \
     _mm_storeu_si128((void *)t, v); return t[i]; }\n\
     __m512i vec512_of_int64s(int64_t a, int64_t b, int64_t c, int64_t d, \
     int64_t e, int64_t f, int64_t g, int64_t h) { return _mm512_set_epi64(h, \
     g, f, e, d, c, b, a); }\n\
     __m256i vec256_of_int64s(int64_t a, int64_t b, int64_t c, int64_t d) { \
     return _mm256_set_epi64x(d, c, b, a); }\n\
     __m128i vec128_of_int64s(int64_t a, int64_t b) { return _mm_set_epi64x(b, \
     a); }\n\n";
  List.iter
    (fun (i, er) ->
      let imms = List.filter is_imm i.params in
      let vals = List.filter (fun p -> not (is_imm p)) i.params in
      (* Widen narrow returns: the OCaml side reads the full register. *)
      let cret =
        if is_mask_ctype i.ret.ctype
        then "__mmask64"
        else
          match i.ret.ctype with
          | "char" | "unsigned char" | "short" | "unsigned short" -> "int64_t"
          | "int" | "unsigned int" | "const int" -> "int32_t"
          | "__int64" | "unsigned __int64" | "long long" | "unsigned long long"
            ->
            "int64_t"
          | c -> c
      in
      let cparam ctype =
        (* The Intel data uses MSVC type names. *)
        match ctype with
        | "__int64" | "long long" -> "int64_t"
        | "unsigned __int64" | "unsigned long long" -> "uint64_t"
        | c -> c
      in
      let params =
        List.mapi (fun j p -> sprintf "%s p%d" (cparam p.ctype) j) vals
        |> String.concat ", "
      in
      List.iter
        (fun tuple ->
          if oracle_bug i tuple
          then ()
          else begin
            let suffix =
              match tuple with
              | [] -> ""
              | t -> "_" ^ String.concat "_" (List.map string_of_int t)
            in
            (* Rebuild the C argument list in original order, immediates
               baked. *)
            let tup = ref tuple in
            let vidx = ref 0 in
            let cargs =
              List.map
                (fun p ->
                  if is_imm p
                  then
                    match !tup with
                    | v :: rest ->
                      tup := rest;
                      string_of_int v
                    | [] -> assert false
                  else
                    let a = sprintf "p%d" !vidx in
                    incr vidx;
                    a)
                i.params
              |> String.concat ", "
            in
            Buffer.add_string buf
              (sprintf "%s ctest%s%s(%s) { return %s(%s); }\n" cret i.name
                 suffix params i.name cargs)
          end)
        (product (List.map (imm_values ~er) imms)))
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
        let count =
          Hashtbl.find_opt skip_reasons reason |> Option.value ~default:0
        in
        Hashtbl.replace skip_reasons reason (count + 1))
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

  val simd_load : Amd64_simd_instrs.instr -> expr list -> result

  val simd_store : Amd64_simd_instrs.instr -> expr list -> result

  val extract_scale : expr list -> string -> int * expr list

  val simd_load_scaled :
    Amd64_simd_instrs.instr -> scale:int -> expr list -> result

  val simd_store_scaled :
    Amd64_simd_instrs.instr -> scale:int -> expr list -> result

  val all_ones_mask : expr

  val zero_vec128 : expr

  val zero_vec256 : expr

  val zero_vec512 : expr
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
