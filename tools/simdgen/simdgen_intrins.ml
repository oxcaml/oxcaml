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
   generated selection table ([amd64_simd_intrins.ml]) recognizing
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
    immwidth : string option;
    immtype : string option;
    varname : string
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
    instrs : instruction list
  }

let param_of_node node =
  { etype = Option.value ~default:"" (Simdgen_xml.attr node "etype");
    ctype = Option.value ~default:"" (Simdgen_xml.attr node "type");
    immwidth = Simdgen_xml.attr node "immwidth";
    immtype = Simdgen_xml.attr node "immtype";
    varname = Option.value ~default:"" (Simdgen_xml.attr node "varname")
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
        immwidth = None;
        immtype = None;
        varname = ""
      }
  in
  let params =
    Simdgen_xml.children_named node "parameter"
    |> List.map param_of_node
    (* [f(void)] is listed as a single parameter of type [void]. *)
    |> List.filter (fun p -> not (String.equal p.ctype "void"))
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
  { name; tech; sequence; cpuid; ret; params; instrs }

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
  String.starts_with ~prefix:"vm" t

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
   [K] is kept, GPRs keep their width, and memory width is ignored (the XML
   memory widths are unreliable). *)
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
  let has prefix = String.starts_with ~prefix ctype in
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

let instr_has_mask (instr : instr_emit) =
  Array.exists
    (fun (a : arg) -> match a.enc with Mask -> true | _ -> false)
    instr.args

(* Hardware operands, dest(s) first then value sources, excluding the
   write-mask. *)
let instr_operands (instr : instr_emit) =
  let args = Array.to_list instr.args |> List.filter arg_is_value in
  match instr.res with
  | Res rr -> Array.to_list rr @ args
  | Arg _ | Res_none -> args

(* Register classes, write-mask presence, and destination encoding (used to
   break load/store ties). *)
let instr_hw_seq instr =
  let operands = instr_operands instr in
  let classes =
    List.filter_map (fun (a : arg) -> loc_regclass a.loc) operands
  in
  let dest_enc = match operands with a :: _ -> Some a.enc | [] -> None in
  classes, instr_has_mask instr, dest_enc

let instr_all_regcap instr =
  List.for_all (fun (a : arg) -> loc_allows_reg a.loc) (instr_operands instr)

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

let classes_match = List.equal (fun xc bc -> List.mem bc (gpr_variants xc))

(* -------------------------------------------------------------------------- *)
(* Instruction selection among multiple <instruction> alternatives *)
(* -------------------------------------------------------------------------- *)

let contains ~needle s =
  let nl = String.length needle and sl = String.length s in
  let rec go i = i + nl <= sl && (String.sub s i nl = needle || go (i + 1)) in
  nl = 0 || go 0

(* FMA and permutex2var families have several differing-mnemonic alternatives
   with identical operand shapes; the choice is forced by which C argument is
   the merge/overwrite target. Returns the chosen instruction, or [None] for
   families handled elsewhere (comi flag-readers). *)
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
      Bool.equal m target_mask && List.equal String.equal target_seq s
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
           List.equal String.equal xs.classes seq))
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
(* Operand order and immediates *)
(* -------------------------------------------------------------------------- *)

let caml_name i = "caml" ^ i.name

let imm_max (p : param) =
  match p.immtype with
  | Some "_CMP_" -> 31
  | Some "_MM_CMPINT" -> 7
  | Some "_MM_PERM" -> 255
  | Some "_MM_MANTISSA_NORM" -> 3
  | Some "_MM_MANTISSA_SIGN" -> 2
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
  if not (instr_has_mask binding.instr)
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
    if String.starts_with ~prefix:"vpcmp" mnemonic
    then cmpint_imm suffix
    else if String.starts_with ~prefix:"vcmp" mnemonic
    then cmp_imm suffix
    else None

(* -------------------------------------------------------------------------- *)
(* Selection specs *)
(* -------------------------------------------------------------------------- *)

(* The selection of each emitted intrinsic is printed as a value of the
   [Amd64_simd_intrins.t] data type (see [selection_preamble]), which
   [Simd_selection] interprets. Generating data rather than code keeps the
   generated module cheap to compile. These types mirror that one, naming
   instruction descriptors by their binding. *)

type g_bind =
  { binding_name : string;
    zeroing : bool option (* [Some z] when the binding takes [~z] *)
  }

type g_imm =
  | G_no_imm
  | G_imm of int (* max *)
  | G_getmant of int * int (* interval max, sign max *)
  | G_fixed of int

type g_operand =
  | G_value of int
  | G_all_ones_mask
  | G_zero_vec of int (* width in bits *)

type g_spec =
  | G_register of
      { arity : int;
        imm : g_imm;
        bind : g_bind;
        args : int list
      }
  | G_embedded_rounding of
      { arity : int;
        bind : g_bind;
        cur : g_bind option;
        args : int list
      }
  | G_suppress_all_exceptions of
      { arity : int;
        imm : g_imm;
        bind : g_bind;
        cur : g_bind option;
        args : int list
      }
  | G_flag_reader of
      { flag : string;
        bname : string
      }
  | G_load of
      { arity : int;
        bind : g_bind;
        args : int list
      }
  | G_store of
      { arity : int;
        bind : g_bind;
        args : int list
      }
  | G_gather of
      { arity : int;
        bname : string;
        args : g_operand list
      }
  | G_scatter of
      { arity : int;
        bname : string;
        args : g_operand list
      }

let g_bind ~zeroing (b : binding) =
  { binding_name = b.bname;
    zeroing = (if b.instr.flags.z then Some zeroing else None)
  }

(* Number of operation arguments the binding takes, with [~z:zeroing] applied
   when it is zeroing-parameterized (the merge form also reads the
   destination). *)
let binding_arity ~zeroing (instr : instr_emit) =
  let n = Array.length instr.args in
  match instr.res with
  | Res rr when instr.flags.z && not zeroing -> n + Array.length rr
  | Res _ | Arg _ | Res_none -> n

(* Returns [Ok spec] or [Error skip_reason]. *)
let register_spec ~by_mnem ~binding ~zeroing i =
  let imms = List.filter is_imm i.params in
  let sae_imms, real_imms =
    List.partition (fun p -> p.immtype = Some "_MM_FROUND_SAE") imms
  in
  let arity = List.length (List.filter (fun p -> not (is_imm p)) i.params) in
  match reorder ~binding ~zeroing i with
  | None -> Error "unhandled operand shape"
  | Some args when List.length args <> binding_arity ~zeroing binding.instr ->
    (* e.g. [_mm512_setzero_ps], an xor of a register with itself. *)
    Error "operand count differs from the instruction's (zeroing idiom)"
  | Some args -> (
    let bind = g_bind ~zeroing binding in
    (* The non-rounding form selected by _MM_FROUND_CUR_DIRECTION. *)
    let cur () =
      let target_seq, target_mask, _ = instr_hw_seq binding.instr in
      Hashtbl.find_opt by_mnem binding.instr.mnemonic
      |> Option.value ~default:[]
      |> List.find_opt (fun b ->
          (match b.instr.flags.r with Rnd_none -> true | _ -> false)
          && instr_all_regcap b.instr
          && binding_arity ~zeroing b.instr = List.length args
          &&
          let s, m, _ = instr_hw_seq b.instr in
          Bool.equal m target_mask && List.equal String.equal target_seq s)
      |> Option.map (g_bind ~zeroing)
    in
    (* The instruction's own immediate, from the C imm params other than sae. *)
    let real_imm () =
      match real_imms with
      | [] -> Some G_no_imm
      | [p] -> Some (G_imm (imm_max p))
      | [interval; sign] ->
        (* getmant packs two enums into one imm8: sign<<2 | interval. *)
        Some (G_getmant (imm_max interval, imm_max sign))
      | _ -> None
    in
    match binding.instr.flags.r with
    | Rnd_er ->
      if List.length real_imms <> 1 || sae_imms <> []
      then Error "embedded-rounding form with unexpected immediates (follow-up)"
      else Ok (G_embedded_rounding { arity; bind; cur = cur (); args })
    | Rnd_sae -> (
      if List.length sae_imms <> 1
      then Error "rounding-control immediate on sae instruction (follow-up)"
      else
        match real_imm () with
        | None -> Error "more than two immediates (follow-up)"
        | Some imm ->
          Ok
            (G_suppress_all_exceptions { arity; imm; bind; cur = cur (); args })
      )
    | Rnd_none -> (
      if sae_imms <> []
      then Error "sae immediate on non-sae binding (follow-up)"
      else
        let register imm = Ok (G_register { arity; imm; bind; args }) in
        match binding.instr.imm, real_imms with
        | Imm_none, [] -> register G_no_imm
        | (Imm_spec | Imm_reg), [_] | (Imm_spec | Imm_reg), [_; _] -> (
          match real_imm () with
          | None -> Error "more than two immediates (follow-up)"
          | Some imm -> register imm)
        | (Imm_spec | Imm_reg), [] -> (
          match
            hardcoded_predicate_imm ~mnemonic:binding.instr.mnemonic i.name
          with
          | Some imm -> register (G_fixed imm)
          | None -> Error "unhandled hardcoded-predicate form")
        | Imm_none, _ :: _ ->
          Error "immediate parameter but instruction takes no immediate"
        | (Imm_spec | Imm_reg), _ ->
          Error "more than two immediates (follow-up)"))

(* Flag-reader intrinsics (kortest/ktest z/c) map to a curated [Simd.Seq]
   pseudo-op: the KORTEST/KTEST instruction followed by a SETcc. The mask width
   comes from the chosen instruction's mnemonic binding. *)
let flag_reader_spec ~by_mnem i =
  match choose_instruction i with
  | None -> Error "flag-reader without an instruction"
  | Some instr -> (
    let flavor =
      List.find_opt
        (fun (needle, _) -> contains ~needle i.name)
        ["testz", "Zf"; "testc", "Cf"]
    in
    match flavor with
    | None -> Error "flag-reader with unknown flavor"
    | Some (_, flag) -> (
      match Hashtbl.find_opt by_mnem instr.mnemonic with
      | Some [binding] when Array.length binding.instr.args = 2 ->
        Ok (G_flag_reader { flag; bname = binding.bname })
      | Some _ | None ->
        Error "no matching instruction descriptor (amd64.csv gap)"))

(* -------------------------------------------------------------------------- *)
(* Memory forms (loads/stores) *)
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
    let is_load = String.starts_with ~prefix:"__m" i.ret.ctype in
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
            let locs =
              List.map (fun (a : arg) -> a.loc) (instr_operands b.instr)
            in
            let res_has_mem =
              match b.instr.res with
              | Res rr ->
                Array.exists (fun (a : arg) -> loc_allows_mem a.loc) rr
              | Arg _ | Res_none -> false
            in
            Bool.equal (instr_has_mask b.instr) xs.masked
            (* [Isimd_mem] can only address a memory operand that lives in the
               argument list, so reject bindings whose result is the memory. *)
            && (not res_has_mem)
            && List.length locs = List.length xml_tokens
            && List.for_all2
                 (fun tok loc -> List.mem tok (op_accepts loc))
                 xml_tokens locs)
      in
      match cands with [b] -> Some (b, is_load) | _ -> None

(* The load/store spec: walk the binding operands, sending the pointer C arg to
   the memory operand, the mask C arg to the write mask, and vector C args to
   the register operands, then build an [Isimd_mem] load or store. *)
let memory_spec ~binding ~is_load ~zeroing i =
  let params = List.mapi (fun j p -> p, j) i.params in
  let indices f =
    List.filter_map (fun (p, j) -> if f p then Some j else None)
  in
  let ptr =
    List.find_map (fun (p, j) -> if is_ptr_param p then Some j else None) params
  in
  let masks = indices is_mask_param params in
  let vectors =
    indices
      (fun p ->
        (not (is_ptr_param p)) && (not (is_mask_param p)) && not (is_imm p))
      params
  in
  match ptr with
  | None -> Error "memory-form instruction without a pointer argument"
  | Some _ when List.exists is_imm i.params ->
    Error "memory form with an immediate (follow-up)"
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
      let arity = List.length i.params in
      let bind = g_bind ~zeroing binding in
      let args = List.filter_map Fun.id operands in
      Ok
        (if is_load
         then G_load { arity; bind; args }
         else G_store { arity; bind; args })

(* -------------------------------------------------------------------------- *)
(* Gathers/scatters *)
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

(* AVX512 gathers/scatters have a single (mandatory-mask) binding per width. The
   XML forms are unreliable here (several i64gather/scatter entries carry the
   wrong vm token or destination width), so derive the match key from the C
   types and the mnemonic instead: index width from the d/q in the mnemonic,
   register classes from the C return/parameter types. *)
let match_gs ~by_mnem i instr =
  let is_gather = String.starts_with ~prefix:"__m" i.ret.ctype in
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
          List.equal String.equal expected value_classes)
    in
    match cands with [b] -> Some b | _ -> None)

(* The gather/scatter spec. The C scale immediate becomes the addressing-mode
   scale; unmasked variants synthesize an all-ones mask (and, for gathers, a
   zero destination) since the AVX512 instructions are mask-only. *)
let gather_scatter_spec ~binding i =
  let is_gather = String.starts_with ~prefix:"__m" i.ret.ctype in
  let non_imm = List.filter (fun p -> not (is_imm p)) i.params in
  let arity = List.length non_imm in
  let params = List.mapi (fun j p -> p, j) non_imm in
  let find f =
    List.find_map (fun (p, j) -> if f p then Some j else None) params
  in
  let vectors =
    List.filter_map
      (fun (p, j) -> if is_ptr_param p || is_mask_param p then None else Some j)
      params
  in
  let base = find is_ptr_param in
  let k =
    match find is_mask_param with
    | Some k -> G_value k
    | None -> G_all_ones_mask
  in
  let zero_dst () =
    match gs_loc_class binding.instr.args.(0).loc with
    | Some "XMM" -> Some (G_zero_vec 128)
    | Some "YMM" -> Some (G_zero_vec 256)
    | Some "ZMM" -> Some (G_zero_vec 512)
    | _ -> None
  in
  let gather dst base vindex =
    Ok
      (G_gather
         { arity;
           bname = binding.bname;
           args = [dst; G_value base; G_value vindex; k]
         })
  in
  match base, is_gather, vectors with
  | Some base, true, [src; vindex] -> gather (G_value src) base vindex
  | Some base, true, [vindex] -> (
    match zero_dst () with
    | None -> Error "unknown gather destination width"
    | Some zero -> gather zero base vindex)
  | Some base, false, [vindex; data] ->
    Ok
      (G_scatter
         { arity;
           bname = binding.bname;
           args = [G_value base; G_value vindex; G_value data; k]
         })
  | _ -> Error "unhandled gather/scatter shape"

(* -------------------------------------------------------------------------- *)
(* Disposition of each in-scope intrinsic *)
(* -------------------------------------------------------------------------- *)

type disposition =
  | Emit of
      { name : string; (* caml_<intel-name> *)
        spec : g_spec
      }
  | Skip of string

let is_lo_convert i =
  contains ~needle:"cvt" i.name
  && (contains ~needle:"lo_pd" i.name || contains ~needle:"pslo" i.name)

let disposition ~by_mnem i : disposition =
  let emit = function
    | Ok spec -> Emit { name = caml_name i; spec }
    | Error reason -> Skip reason
  in
  if is_flag_reader_name i.name
  then
    if contains ~needle:"comi" i.name
    then Skip "flag-reader: comi predicate+sae (curated follow-up)"
    else if List.exists is_ptr_param i.params
    then Skip "flag-reader with out-pointer (composable from z/c variants)"
    else emit (flag_reader_spec ~by_mnem i)
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
        | Some binding -> emit (gather_scatter_spec ~binding i)))
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
        | Some (binding, is_load) ->
          let zeroing = (xml_shape_of_form instr.form).zeroing in
          emit (memory_spec ~binding ~is_load ~zeroing i)))
    | Register when is_lo_convert i ->
      Skip "lo/hi convert variant (full-width arg, low/high half used)"
    | Register -> (
      match choose_instruction i with
      | None -> Skip "unhandled multi-instruction form"
      | Some instr -> (
        match match_register ~by_mnem i instr with
        | Matched { binding; zeroing } ->
          emit (register_spec ~by_mnem ~binding ~zeroing i)
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

(* Each emitted intrinsic is checked bit-for-bit against the C intrinsic of the
   same name, compiled by clang as the oracle. Register forms and loads compare
   results. Stores run the builtin and the C intrinsic against two separate but
   identically initialized 512-byte buffers ([buf 1] and [buf 2]) and compare
   the whole buffers, catching writes outside the destination; loads and gathers
   read [buf 0]. Pointers are 128 bytes into their (64-byte aligned) buffer and
   gather/scatter indices stay within [-8, 31] elements, so every access is in
   bounds at any scale. The C side of the helpers is in
   [oxcaml/tests/simd/avx512/intrins/helpers.c]. *)

(* Kind of a testable C parameter/return value. *)
type tkind =
  | Vec of int (* 128 / 256 / 512 *)
  | MaskT
  | I32
  | I64
  | ISub (* sub-word integer, passed untagged *)
  | Ptr (* pointer, passed as [nativeint#] *)
  | Void (* no result *)

let tkind_of (p : param) =
  let etype = p.etype in
  match p.ctype with
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
  | "void" -> Some (Void, "void")
  | c when is_ptr_param p && String.length c > 0 -> Some (Ptr, "addr")
  | _ -> None

let oty_with_attr (k, oty) =
  match k with
  | ISub -> sprintf "(%s[@untagged])" oty
  | Ptr | Void -> oty
  | Vec _ | MaskT | I32 | I64 -> sprintf "(%s[@unboxed])" oty

let is_mask_ctype c = String.starts_with ~prefix:"__mmask" c

type test_kind =
  | Compare_result (* register forms, loads, gathers *)
  | Compare_buffers (* stores, scatters *)

type test =
  { i : intrinsic;
    kind : test_kind;
    memory : bool; (* goes in the memory-form test executable *)
    er : bool; (* embedded rounding: enumerate the rounding modes *)
    reuse_mask : bool
        (* masked gather/scatter, which clobbers its mask register: also check
           that a second instruction using the same mask is unaffected (the two
           gathers differ in their merge source, so they are not CSE'd) *)
  }

(* Every emitted intrinsic whose parameters and result have a testable kind. *)
let tests ~bindings intrinsics =
  let testable i =
    Option.is_some (tkind_of i.ret)
    && List.for_all (fun p -> is_imm p || Option.is_some (tkind_of p)) i.params
    && List.exists (fun p -> not (is_imm p)) i.params
  in
  dispositions ~bindings intrinsics
  |> List.filter_map (fun (i, d) ->
      let test kind ~memory ?(er = false) ?(reuse_mask = false) () =
        if testable i then Some { i; kind; memory; er; reuse_mask } else None
      in
      let masked = List.exists is_mask_param i.params in
      match d with
      | Skip _ -> None
      | Emit { spec; _ } -> (
        match spec with
        | G_register _ | G_suppress_all_exceptions _ | G_flag_reader _ ->
          test Compare_result ~memory:false ()
        | G_embedded_rounding _ -> test Compare_result ~memory:false ~er:true ()
        | G_load _ -> test Compare_result ~memory:true ()
        | G_gather _ -> test Compare_result ~memory:true ~reuse_mask:masked ()
        | G_store _ -> test Compare_buffers ~memory:true ()
        | G_scatter _ -> test Compare_buffers ~memory:true ~reuse_mask:masked ()
        ))
  |> List.sort_uniq (fun a b -> String.compare a.i.name b.i.name)

(* Values enumerated for an immediate parameter. Rounding immediates follow the
   dispatch in [Simd_selection]: embedded-rounding specs accept the four
   [_MM_FROUND_TO_*|_MM_FROUND_NO_EXC] combinations plus CUR_DIRECTION, sae
   specs NO_EXC (alone or with CUR_DIRECTION) plus CUR_DIRECTION. *)
let imm_values ~er (p : param) =
  match p.immtype with
  | Some "_CMP_" -> List.init 32 (fun v -> v)
  | Some "_MM_CMPINT" -> List.init 8 (fun v -> v)
  | Some "_MM_FROUND" -> if er then [8; 9; 10; 11; 4] else [8; 4]
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

let is_index_param p = String.equal p.varname "vindex"

let index_bits (p : param) =
  match p.etype with "SI64" | "UI64" -> 64 | _ -> 32

let index_name ~bits ~width = sprintf "idx%d_%d" bits width

(* Distinct index lanes within [-8, 31], packed into 64-bit words. *)
let index_words ~bits ~width =
  let lanes =
    List.init (width / bits) (fun j ->
        (j * (if bits = 32 then 7 else 11) mod 40) - 8)
  in
  let rec pack = function
    | lo :: hi :: rest ->
      Int64.logor
        (Int64.logand (Int64.of_int lo) 0xFFFFFFFFL)
        (Int64.shift_left (Int64.of_int hi) 32)
      :: pack rest
    | [] -> []
    | [_] -> assert false
  in
  if bits = 64 then List.map Int64.of_int lanes else pack lanes

(* OCaml expression for the [idx]-th value argument of the given kind. Masks are
   bound once per test to [m<idx>] (see [mask_binding]) and memory operands to
   [p<buffer>], so that consecutive builtin calls share their operands. *)
let value_expr ?(shift = 0) ~buffer ~idx (p : param) k =
  let v = "abc".[(idx + shift) mod 3] in
  match k with
  | Vec width when is_index_param p ->
    sprintf "(reint %s)" (index_name ~bits:(index_bits p) ~width)
  | Vec 512 -> sprintf "(reint v%c)" v
  | Vec 256 -> sprintf "(reint v%c256)" v
  | Vec 128 -> sprintf "(reint v%c128)" v
  | Vec _ -> assert false
  | MaskT -> sprintf "m%d" idx
  | I32 -> [| "0x12345678l"; "(-7l)"; "0x40000001l" |].(idx mod 3)
  | I64 ->
    [| "0x1122334455667788L"; "(-9L)"; "0x4000000000000001L" |].(idx mod 3)
  | ISub -> [| "23"; "113"; "5" |].(idx mod 3)
  | Ptr -> sprintf "p%d" buffer
  | Void -> invalid_arg "value_expr"

(* Masks are truncated to the parameter width (the OCaml<->C mask ABI passes the
   full register, so upper bits must be zero for the C oracle). *)
let mask_binding ~idx (p : param) =
  let width =
    match p.ctype with
    | "__mmask8" -> 0xFFL
    | "__mmask16" -> 0xFFFFL
    | "__mmask32" -> 0xFFFFFFFFL
    | _ -> -1L
  in
  sprintf "let m%d = mask_arg masks %d 0x%LxL in " idx idx width

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
  | Ptr | Void -> invalid_arg "check_fn"

(* clang (checked at 21.1) miscompiles the masked scalar sqrt intrinsics when
   the rounding is CUR_DIRECTION (including the non-round forms): the DAG path
   swaps the [a]/[b] operands, computing sqrt(a) with the upper bits from [b],
   contradicting the Intel pseudocode and clang's own constant folder. Our
   selection follows the spec. Build these oracles from SSE sqrt and moves
   instead, so the affected cases still get tested. *)
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

let test_suffix = function
  | [] -> ""
  | tuple -> "_" ^ String.concat "_" (List.map string_of_int tuple)

let test_tuples { i; er; _ } =
  product (List.map (imm_values ~er) (List.filter is_imm i.params))

let tests_ml_preamble =
  {ml|(* Generated by tools/simdgen/simdgen_intrins.ml: bit-for-bit checks
   of emitted intrinsics against the real C intrinsics. *)

(* Some of the shared helpers are unused in each test. *)
[@@@ocaml.warning "-32-34"]

open Stdlib

type void : void

type addr = nativeint_u

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

external buf : (int[@untagged]) -> addr = "" "test_buf" [@@noalloc]

external buf_reset : (int[@untagged]) -> (int[@untagged])
  = "" "test_buf_reset"
[@@noalloc]

external buf_eq : (int[@untagged]) -> (int[@untagged]) = "" "test_buf_eq"
[@@noalloc]

(* Include both outcomes of mask flag tests and both scalar mask-bit states. *)
let test_masks name f =
  List.iteri
    (fun i masks -> f (Printf.sprintf "%s/mask%d" name i) masks)
    [ 0L, 0L; -1L, 0L; 0L, -1L; -1L, -1L;
      0xa5a5a5a5a5a5a5a5L, 0x3c3c3c3c3c3c3c3cL;
      0xa5a5a5a5a5a5a5a5L, 0x5a5a5a5a5a5a5a5aL;
      0xa5a5a5a5a5a5a5a5L, 0xa5a5a5a5a5a5a5a5L;
      1L, 1L; Int64.min_int, Int64.min_int ]

let mask_arg masks idx width =
  let bits = if idx mod 2 = 0 then fst masks else snd masks in
  mask_of_int64 (Int64.logand bits width)

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

let check_buf name = if buf_eq 0 <> 1 then fail name

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

let print_index_vectors buf =
  List.iter
    (fun bits ->
      List.iter
        (fun (width, of_w) ->
          bprintf buf "\nlet %s = %s %s\n" (index_name ~bits ~width) of_w
            (String.concat " "
               (List.map (sprintf "0x%LxL") (index_words ~bits ~width))))
        [512, "of_w"; 256, "of_w256"; 128, "of_w128"])
    [32; 64]

let print_test_ml buf ({ i; kind; reuse_mask; _ } as test) =
  let name = caml_name i in
  let imms = List.filter is_imm i.params in
  let vals = List.filter (fun p -> not (is_imm p)) i.params in
  let val_tys = List.map (fun p -> Option.get (tkind_of p)) vals in
  let ret_ty = Option.get (tkind_of i.ret) in
  let signature tys = String.concat " -> " (List.map oty_with_attr tys) in
  (* Immediates lead, as untagged ints. *)
  bprintf buf
    "\n\
     external %s : %s = \"caml_vec512_unreachable\" %S\n\
     [@@noalloc] [@@builtin]\n"
    name
    (signature (List.map (fun _ -> ISub, "int") imms @ val_tys @ [ret_ty]))
    name;
  let masks =
    List.mapi (fun idx p -> idx, p) vals
    |> List.filter (fun (_, p) -> is_mask_param p)
  in
  (* [shift] selects different (non-index) vector arguments. *)
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
          let check = check_fn ret_ty in
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
        if not (List.exists is_ptr_param vals)
        then body
        else
          match kind with
          | Compare_result -> "let p0 = buf 0 in " ^ body
          | Compare_buffers -> "let p1 = buf 1 in let p2 = buf 2 in " ^ body
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

(* [memory] selects the memory-form tests, which go in a separate executable. *)
let tests_ml ~memory ~bindings intrinsics =
  let buf = Buffer.create 262144 in
  Buffer.add_string buf tests_ml_preamble;
  print_index_vectors buf;
  List.iter
    (fun test -> if Bool.equal test.memory memory then print_test_ml buf test)
    (tests ~bindings intrinsics);
  Buffer.add_string buf "\nlet () = if !failures <> 0 then exit 1\n";
  print_string (Buffer.contents buf)

(* The Intel data uses MSVC type names. *)
let cparam ctype =
  match ctype with
  | "__int64" | "long long" -> "int64_t"
  | "unsigned __int64" | "unsigned long long" -> "uint64_t"
  | c -> c

(* Widen narrow returns: the OCaml side reads the full register. *)
let cret ctype =
  if is_mask_ctype ctype
  then "__mmask64"
  else
    match ctype with
    | "char" | "unsigned char" | "short" | "unsigned short" -> "int64_t"
    | "int" | "unsigned int" | "const int" -> "int32_t"
    | "__int64" | "unsigned __int64" | "long long" | "unsigned long long" ->
      "int64_t"
    | c -> c

(* The C call of [i] with the immediates of [tuple] baked in and the other
   arguments named [p0], [p1], ... in order (pointers passed as [void *]). *)
let c_call i tuple =
  let tup = ref tuple in
  let vidx = ref 0 in
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
        if is_ptr_param p then sprintf "(%s)%s" p.ctype a else a)
    i.params
  |> String.concat ", " |> sprintf "%s(%s)" i.name

(* The C oracles of both test executables. Without AVX512 (when the stubs are
   compiled by a C compiler lacking the newer intrinsic names, in which case the
   tests do not run) every oracle is an aborting stub, so that the test
   executables still link. *)
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
        List.filter (fun p -> not (is_imm p)) i.params
        |> List.mapi (fun j p ->
            if is_ptr_param p
            then sprintf "void *p%d" j
            else sprintf "%s p%d" (cparam p.ctype) j)
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

(* -------------------------------------------------------------------------- *)
(* Selection table *)
(* -------------------------------------------------------------------------- *)

let print_bind ?(ctor = "Instr", "Zeroing") { binding_name; zeroing } =
  let plain, zeroing_ctor = ctor in
  match zeroing with
  | None -> sprintf "%s %s" plain binding_name
  | Some z -> sprintf "%s (%s, %b)" zeroing_ctor binding_name z

let print_imm = function
  | G_no_imm -> "No_imm"
  | G_imm max -> sprintf "Imm %d" max
  | G_getmant (interval, sign) -> sprintf "Getmant (%d, %d)" interval sign
  | G_fixed i -> sprintf "Fixed %d" i

let print_cur = function
  | None -> "None"
  | Some b -> sprintf "Some (%s)" (print_bind b)

let print_operands args =
  List.map
    (function
      | G_value j -> sprintf "Value %d" j
      | G_all_ones_mask -> "All_ones_mask"
      | G_zero_vec width -> sprintf "Zero_vec%d" width)
    args
  |> String.concat "; " |> sprintf "[|%s|]"

let print_args args = print_operands (List.map (fun j -> G_value j) args)

let print_spec = function
  | G_register { arity; imm; bind; args } ->
    sprintf "Register { arity = %d; imm = %s; bind = %s; args = %s }" arity
      (print_imm imm) (print_bind bind) (print_args args)
  | G_embedded_rounding { arity; bind; cur; args } ->
    sprintf "Embedded_rounding { arity = %d; bind = %s; cur = %s; args = %s }"
      arity
      (print_bind ~ctor:("Rnd", "Rnd_zeroing") bind)
      (print_cur cur) (print_args args)
  | G_suppress_all_exceptions { arity; imm; bind; cur; args } ->
    sprintf
      "Suppress_all_exceptions { arity = %d; imm = %s; bind = %s; cur = %s; \
       args = %s }"
      arity (print_imm imm)
      (print_bind ~ctor:("Sae", "Sae_zeroing") bind)
      (print_cur cur) (print_args args)
  | G_flag_reader { flag; bname } ->
    sprintf "Flag_reader { flag = %s; instr = %s }" flag bname
  | G_load { arity; bind; args } ->
    sprintf "Load { arity = %d; bind = %s; args = %s }" arity (print_bind bind)
      (print_args args)
  | G_store { arity; bind; args } ->
    sprintf "Store { arity = %d; bind = %s; args = %s }" arity (print_bind bind)
      (print_args args)
  | G_gather { arity; bname; args } ->
    sprintf "Gather { arity = %d; instr = %s; args = %s }" arity bname
      (print_operands args)
  | G_scatter { arity; bname; args } ->
    sprintf "Scatter { arity = %d; instr = %s; args = %s }" arity bname
      (print_operands args)

let selection_preamble =
  {ocaml|(* Generated by tools/simdgen/simdgen_intrins.ml. *)

(* Selection data for the AVX512 [caml_<intel-name>] builtins, interpreted by
   [Simd_selection]. This module is compiled on every architecture, so it only
   refers to the arch-independent instruction descriptors. *)

[@@@ocaml.warning "+a-4-40-42-70"]

open! Amd64_simd_instrs

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
      (* A leading rounding-control constant selects [bind] with a static
         rounding mode (8-11) or [cur] (4, _MM_FROUND_CUR_DIRECTION). *)
  | Suppress_all_exceptions of
      { arity : int;
        imm : imm;
        bind : sae_bind;
        cur : bind option;
        args : operand array
      }
      (* After [imm], a constant selects [bind] (8 or 12, _MM_FROUND_NO_EXC)
         or [cur] (4, _MM_FROUND_CUR_DIRECTION). *)
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

let table : (string * t) array =
  [|
|ocaml}

let selection_postamble =
  {ocaml|
  |]

let find =
  let tbl =
    lazy
      (let tbl = Hashtbl.create (Array.length table) in
       Array.iter (fun (name, t) -> Hashtbl.replace tbl name t) table;
       tbl)
  in
  fun name -> Hashtbl.find_opt (Lazy.force tbl) name
|ocaml}

let print_selection ~bindings intrinsics =
  let ds = dispositions ~bindings intrinsics in
  let arms =
    List.filter_map
      (fun (_, d) ->
        match d with Emit { name; spec } -> Some (name, spec) | Skip _ -> None)
      ds
    |> List.sort_uniq (fun (a, _) (b, _) -> String.compare a b)
  in
  print_string selection_preamble;
  List.iter
    (fun (name, spec) -> printf "    %S, %s;\n" name (print_spec spec))
    arms;
  print_string selection_postamble

let print_skiplist ~bindings intrinsics =
  let ds = dispositions ~bindings intrinsics in
  printf "# AVX512 intrinsics skip list (generated by simdgen_intrins).\n";
  printf "# Every in-scope intrinsic is either emitted or listed here with a\n";
  printf
    "# reason. Regenerate with the [intrins skiplist] simdgen subcommand.\n";
  ds
  |> List.filter_map (fun (i, d) ->
      match d with Skip reason -> Some (i.name, reason) | _ -> None)
  |> List.sort compare
  |> List.iter (fun (name, reason) -> printf "%s\t%s\n" name reason)
