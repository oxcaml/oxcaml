[@@@ocaml.warning "+a-4-40-41-42"]

open! Int_replace_polymorphic_compare
open Arm64_ast.Ast
module DLL = Doubly_linked_list
module U = Peephole_utils
module Gp = U.Gp

type peephole_stats =
  { mutable fuse_str_pairs : int;
    mutable fuse_ldr_pairs : int;
    mutable merge_add_immediates : int;
    mutable remove_redundant_cmp : int;
    mutable compose_shift_pairs : int
  }

let create_peephole_stats () =
  { fuse_str_pairs = 0;
    fuse_ldr_pairs = 0;
    merge_add_immediates = 0;
    remove_redundant_cmp = 0;
    compose_shift_pairs = 0
  }

let peephole_stats_to_counters stats =
  Profile.Counters.create ()
  |> Profile.Counters.set "arm64_peephole.fuse_str_pairs" stats.fuse_str_pairs
  |> Profile.Counters.set "arm64_peephole.fuse_ldr_pairs" stats.fuse_ldr_pairs
  |> Profile.Counters.set "arm64_peephole.merge_add_immediates"
       stats.merge_add_immediates
  |> Profile.Counters.set "arm64_peephole.remove_redundant_cmp"
       stats.remove_redundant_cmp
  |> Profile.Counters.set "arm64_peephole.compose_shift_pairs"
       stats.compose_shift_pairs

(* Decoding helpers over the typed AST. Rewrite rules never reuse a matched
   register or operand in a rebuilt instruction (the GADT existentials make that
   awkward); they decode operands to plain values here and rebuild concrete
   operands through the [DSL] smart constructors. *)

let ins_of_cell cell =
  match DLL.value cell with U.Ins ins -> Some ins | U.Directive _ -> None

let twelve_value (type w) (op : [`Imm of w] Operand.t) : int option =
  match op with Imm (Twelve value) -> Some value | Imm _ -> None

let six_value (op : [`Imm of [`Six]] Operand.t) : int =
  match op with Imm (Six value) -> value

let optional_is_none (type a) (op : [`Optional of a option] Operand.t) : bool =
  match op with Optional None -> true | Optional (Some _) -> false

(* An X-class transfer register, remembering the assembly spelling so that
   rebuilt instructions use the same one. *)
type xreg =
  | Xn of int
  | Xlr

let decode_xreg (type a) (reg : [`GP of a] Reg.t) : xreg option =
  match reg.reg_name with
  | GP X -> Some (Xn reg.index)
  | GP LR -> Some Xlr
  | GP (W | WZR | XZR | WSP | SP | FP) -> None

let xreg_gp = function Xn i -> Gp.Reg64 i | Xlr -> Gp.Reg64 30

(* Rewrite rule: fuse two adjacent 64-bit loads (resp. stores) with the same
   base register and byte offsets differing by exactly 8 into a single [ldp]
   (resp. [stp]).

   Pattern: str rA, [rB, #k]; str rC, [rB, #k+-8] (and dually for ldr) Rewrite:
   stp rlo, rhi, [rB, #min(k, k')]

   Safety: - adjacency (no label in between, cf. [U.next_instruction])
   guarantees that no call or GC point is crossed and that the second
   instruction has no other predecessor; - the two stores are to the same base
   with disjoint offsets, so their relative order is not observable; - for
   loads, the two destinations must differ (such an [ldp] is
   constrained-unpredictable) and the first destination must not be the base
   register (the original second load would have used the updated base); - the
   fused offset must be a multiple of 8 in [-512, 504], the range of the scaled
   7-bit immediate (also checked by [DSL.mem_offset_pair]). *)

type transfer =
  { is_load : bool;
    reg : xreg;
    base : Gp.t;
    offset : int
  }

let decode_transfer (Instruction.I { name; operands }) : transfer option =
  match name, operands with
  | LDR, Pair (Reg reg, mem) -> (
    match decode_xreg reg, U.decode_simple_mem mem with
    | Some reg, Some (base, offset) ->
      Some { is_load = true; reg; base; offset }
    | None, _ | _, None -> None)
  | STR, Pair (Reg reg, mem) -> (
    match decode_xreg reg, U.decode_simple_mem mem with
    | Some reg, Some (base, offset) ->
      Some { is_load = false; reg; base; offset }
    | None, _ | _, None -> None)
  | _, _ -> None

let make_transfer_pair ~is_load ~base ~offset ~lo ~hi : Instruction.t option =
  let ins name operands = Instruction.I { name; operands } in
  let mem () = DSL.mem_offset_pair ~base ~offset in
  match is_load, lo, hi with
  | false, Xn a, Xn b ->
    Some (ins (STP X) (Triple (DSL.reg_x a, DSL.reg_x b, mem ())))
  | false, Xn a, Xlr ->
    Some (ins (STP X) (Triple (DSL.reg_x a, DSL.lr, mem ())))
  | false, Xlr, Xn b ->
    Some (ins (STP X) (Triple (DSL.lr, DSL.reg_x b, mem ())))
  | false, Xlr, Xlr -> Some (ins (STP X) (Triple (DSL.lr, DSL.lr, mem ())))
  | true, Xn a, Xn b ->
    Some (ins (LDP X) (Triple (DSL.reg_x a, DSL.reg_x b, mem ())))
  | true, Xn a, Xlr -> Some (ins (LDP X) (Triple (DSL.reg_x a, DSL.lr, mem ())))
  | true, Xlr, Xn b -> Some (ins (LDP X) (Triple (DSL.lr, DSL.reg_x b, mem ())))
  | true, Xlr, Xlr ->
    (* Rejected before this point: equal load destinations. *)
    None

let fuse_memory_pairs stats cell =
  match Option.bind (ins_of_cell cell) decode_transfer with
  | None -> U.No_match
  | Some t1 -> (
    match U.next_instruction cell with
    | None -> U.No_match
    | Some cell2 -> (
      match Option.bind (ins_of_cell cell2) decode_transfer with
      | None -> U.No_match
      | Some t2 -> (
        let lo, hi = if t1.offset < t2.offset then t1, t2 else t2, t1 in
        let offset_ok =
          abs (t1.offset - t2.offset) = 8
          && lo.offset mod 8 = 0
          && lo.offset >= -512 && lo.offset <= 504
        in
        let load_ok =
          (not t1.is_load)
          || (not (Gp.equal (xreg_gp t1.reg) (xreg_gp t2.reg)))
             && not (Gp.equal (xreg_gp t1.reg) t1.base)
        in
        if
          not
            (Bool.equal t1.is_load t2.is_load
            && Gp.equal t1.base t2.base && offset_ok && load_ok)
        then U.No_match
        else
          let fused =
            match t1.base with
            | Gp.Reg64 i ->
              make_transfer_pair ~is_load:t1.is_load ~base:(Reg.reg_x i)
                ~offset:lo.offset ~lo:lo.reg ~hi:hi.reg
            | Gp.Sp ->
              make_transfer_pair ~is_load:t1.is_load ~base:Reg.sp
                ~offset:lo.offset ~lo:lo.reg ~hi:hi.reg
            | Gp.Reg32 _ | Gp.Zr64 | Gp.Zr32 ->
              (* Cannot happen: the base register of an addressing mode is an X
                 register or [sp]. *)
              None
          in
          match fused with
          | None -> U.No_match
          | Some fused ->
            DLL.set_value cell (U.Ins fused);
            U.delete_next_instruction_and_locs cell;
            if t1.is_load
            then stats.fuse_ldr_pairs <- stats.fuse_ldr_pairs + 1
            else stats.fuse_str_pairs <- stats.fuse_str_pairs + 1;
            U.Matched (Some cell))))

(* Rewrite rule: merge two adjacent add/sub-immediates through the same
   register.

   Pattern: add rd, rn, #k1; add rd, rd, #k2 (and the sub/mixed variants)
   Rewrite: add rd, rn, #(k1+k2) (or sub, depending on the sign of the sum)

   The destination is restricted to plain [x0]-[x28]: excluding [sp] keeps the
   rule away from stack adjustments and their paired CFI directives, and
   excluding [fp]/[lr] (x29/x30) is conservative. The combined immediate must be
   encodable: 12 bits, optionally shifted left by 12 (the domain of
   [DSL.imm]). *)

type add_src =
  | Src_x of int
  | Src_sp
  | Src_fp

let decode_add_src (type a) (reg : [`GP of a] Reg.t) : add_src option =
  match reg.reg_name with
  | GP X -> Some (Src_x reg.index)
  | GP (SP | WSP) -> Some Src_sp
  | GP FP -> Some Src_fp
  | GP (W | WZR | XZR | LR) -> None

let decode_add_dest (type a) (reg : [`GP of a] Reg.t) : int option =
  match reg.reg_name with
  | GP X -> if reg.index <= 28 then Some reg.index else None
  | GP (W | WZR | XZR | WSP | SP | LR | FP) -> None

type add_imm =
  { rd : int;
    src : add_src;
    value : int (* signed: positive for add, negative for sub *)
  }

let decode_add_imm (Instruction.I { name; operands }) : add_imm option =
  let decode ~negate rd rn imm shift =
    if not (optional_is_none shift)
    then None
    else
      match decode_add_dest rd, decode_add_src rn, twelve_value imm with
      | Some rd, Some src, Some value ->
        Some { rd; src; value = (if negate then -value else value) }
      | None, _, _ | _, None, _ | _, _, None -> None
  in
  match name, operands with
  | ADD_immediate, Quad (Reg rd, Reg rn, imm, shift) ->
    decode ~negate:false rd rn imm shift
  | SUB_immediate, Quad (Reg rd, Reg rn, imm, shift) ->
    decode ~negate:true rd rn imm shift
  | _, _ -> None

let encodable_imm12 value =
  value >= 0 && (value <= 0xFFF || (value <= 0xFFF_000 && value land 0xFFF = 0))

let make_add_imm ~rd ~src ~value : Instruction.t option =
  let ins name operands = Instruction.I { name; operands } in
  if value >= 0
  then
    let imm = DSL.imm value in
    match src with
    | Src_x j ->
      Some
        (ins ADD_immediate
           (Quad (DSL.reg_x rd, DSL.reg_x j, imm, DSL.optional_none)))
    | Src_sp ->
      Some
        (ins ADD_immediate
           (Quad (DSL.reg_x rd, DSL.sp, imm, DSL.optional_none)))
    | Src_fp ->
      Some
        (ins ADD_immediate
           (Quad (DSL.reg_x rd, DSL.fp, imm, DSL.optional_none)))
  else
    let imm = DSL.imm (-value) in
    match src with
    | Src_x j ->
      Some
        (ins SUB_immediate
           (Quad (DSL.reg_x rd, DSL.reg_x j, imm, DSL.optional_none)))
    | Src_sp ->
      Some
        (ins SUB_immediate
           (Quad (DSL.reg_x rd, DSL.sp, imm, DSL.optional_none)))
    | Src_fp ->
      (* [SUB_immediate] does not admit an [fp] source operand. *)
      None

let merge_add_immediates stats cell =
  match Option.bind (ins_of_cell cell) decode_add_imm with
  | None -> U.No_match
  | Some d1 -> (
    match U.next_instruction cell with
    | None -> U.No_match
    | Some cell2 -> (
      match Option.bind (ins_of_cell cell2) decode_add_imm with
      | None -> U.No_match
      | Some d2 -> (
        let second_reads_and_rewrites_first_dest =
          match d2.src with
          | Src_x j -> j = d1.rd && d2.rd = d1.rd
          | Src_sp | Src_fp -> false
        in
        let value = d1.value + d2.value in
        if
          not
            (second_reads_and_rewrites_first_dest && encodable_imm12 (abs value))
        then U.No_match
        else
          match make_add_imm ~rd:d1.rd ~src:d1.src ~value with
          | None -> U.No_match
          | Some merged ->
            DLL.set_value cell (U.Ins merged);
            U.delete_next_instruction_and_locs cell;
            stats.merge_add_immediates <- stats.merge_add_immediates + 1;
            U.Matched (Some cell))))

(* Rewrite rule: remove a compare that is identical to an earlier one, with no
   intervening write to the flags or to the compared registers.

   Pattern: subs xzr, rn, op; ...; subs xzr, rn, op Rewrite: subs xzr, rn, op;
   ...

   The scan continues through conditional branches: they read but do not write
   the flags, and the second compare can only be reached by fall-through because
   any other entry point would require a label, which stops the scan. The scan
   stops at any other control flow, at labels and other barrier directives, at
   memory-ordering instructions, at flag writers, and at any instruction that
   may write to (a register overlapping) one of the compared registers -- note
   that a write to [wN] invalidates a compare reading [xN]. *)

type shift_kind =
  | Sk_lsl
  | Sk_lsr
  | Sk_asr

let equal_shift_kind left right =
  match left, right with
  | Sk_lsl, Sk_lsl -> true
  | Sk_lsr, Sk_lsr -> true
  | Sk_asr, Sk_asr -> true
  | (Sk_lsl | Sk_lsr | Sk_asr), _ -> false

type cmp_shape =
  | Cmp_imm of
      { is64 : bool;
        rn : Gp.t;
        value : int;
        shifted : bool
      }
  | Cmp_reg of
      { rn : Gp.t;
        rm : Gp.t;
        shift : (shift_kind * int) option
      }

let decode_zr (type a) (reg : [`GP of a] Reg.t) : bool option =
  match reg.reg_name with
  | GP XZR -> Some true
  | GP WZR -> Some false
  | GP (W | X | WSP | SP | LR | FP) -> None

(* Returns [None] if the shift operand cannot be decoded, [Some None] if there
   is no shift, and [Some (Some _)] for a decoded shift. *)
let decode_optional_shift (type k)
    (op : [`Optional of [`Shift of k * [`Six]] option] Operand.t) :
    (shift_kind * int) option option =
  match op with
  | Optional None -> Some None
  | Optional (Some (Shift { kind; amount })) -> (
    let kind =
      match kind with
      | Operand.Shift.Kind.LSL -> Sk_lsl
      | Operand.Shift.Kind.LSR -> Sk_lsr
      | Operand.Shift.Kind.ASR -> Sk_asr
    in
    match amount with Six amount -> Some (Some (kind, amount)))

let decode_cmp (Instruction.I { name; operands }) : cmp_shape option =
  match name, operands with
  | SUBS_immediate, Quad (Reg zr, Reg rn, imm, shift) -> (
    match decode_zr zr, twelve_value imm with
    | Some is64, Some value ->
      Some
        (Cmp_imm
           { is64;
             rn = Gp.of_reg rn;
             value;
             shifted = not (optional_is_none shift)
           })
    | None, _ | _, None -> None)
  | SUBS_shifted_register, Quad (Reg zr, Reg rn, Reg rm, shift) -> (
    match decode_zr zr with
    | Some true -> (
      match decode_optional_shift shift with
      | None -> None
      | Some shift ->
        Some (Cmp_reg { rn = Gp.of_reg rn; rm = Gp.of_reg rm; shift }))
    | Some false | None -> None)
  | _, _ -> None

let equal_cmp_shape left right =
  match left, right with
  | Cmp_imm l, Cmp_imm r ->
    Bool.equal l.is64 r.is64 && Gp.equal l.rn r.rn && l.value = r.value
    && Bool.equal l.shifted r.shifted
  | Cmp_reg l, Cmp_reg r ->
    Gp.equal l.rn r.rn && Gp.equal l.rm r.rm
    && Option.equal
         (fun (k1, a1) (k2, a2) -> equal_shift_kind k1 k2 && a1 = a2)
         l.shift r.shift
  | Cmp_imm _, Cmp_reg _ | Cmp_reg _, Cmp_imm _ -> false

let cmp_reads = function
  | Cmp_imm { rn; _ } -> [rn]
  | Cmp_reg { rn; rm; _ } -> [rn; rm]

let find_redundant_cmp shape start_cell =
  let reads = cmp_reads shape in
  let rec loop cell_opt =
    match cell_opt with
    | None -> None
    | Some cell -> (
      match DLL.value cell with
      | U.Directive d ->
        if U.directive_is_barrier d then None else loop (DLL.next cell)
      | U.Ins ins -> (
        match decode_cmp ins with
        | Some shape' when equal_cmp_shape shape shape' -> Some cell
        | Some _ | None ->
          if U.is_conditional_branch ins
          then loop (DLL.next cell)
          else if
            U.is_hard_barrier (U.Ins ins)
            || U.writes_flags ins
            || U.writes_any_gp_reg ins ~regs:reads
          then None
          else loop (DLL.next cell)))
  in
  loop (DLL.next start_cell)

let remove_redundant_cmp stats cell =
  match Option.bind (ins_of_cell cell) decode_cmp with
  | None -> U.No_match
  | Some shape -> (
    match find_redundant_cmp shape cell with
    | None -> U.No_match
    | Some redundant_cell ->
      DLL.delete_curr redundant_cell;
      stats.remove_redundant_cmp <- stats.remove_redundant_cmp + 1;
      U.Matched (Some cell))

(* Rewrite rule: compose two adjacent 64-bit immediate shifts through the same
   register into a single instruction.

   The emitter produces shifts as raw [ubfm]/[sbfm]; this rule recognizes the
   lsl/lsr/asr shapes and rewrites e.g. lsl rd, rn, #a; lsr rd, rd, #b into ubfm
   rd, rn, #(b-a mod 64), #(63-a) (i.e. ubfx/ubfiz), and lsr rd, rn, #a; lsl rd,
   rd, #a into an [and] that clears the low [a] bits. Shifts in the same
   direction add their amounts (an [asr] chain saturates at 63; over-shifted
   [lsl]/[lsr] chains, which produce zero, are left alone). The intermediate
   value of [rd] is dead by construction since the second instruction overwrites
   it and the instructions are adjacent. *)

type shift_op =
  | Lsl of int
  | Lsr of int
  | Asr of int

type shift_ins =
  { rd : int;
    rn : int;
    op : shift_op
  }

let decode_xn (type a) (reg : [`GP of a] Reg.t) : int option =
  match reg.reg_name with
  | GP X -> Some reg.index
  | GP (W | WZR | XZR | WSP | SP | LR | FP) -> None

let decode_shift_ins (Instruction.I { name; operands }) : shift_ins option =
  match name, operands with
  | UBFM, Quad (Reg rd, Reg rn, immr, imms) -> (
    match decode_xn rd, decode_xn rn with
    | Some rd, Some rn ->
      let immr = six_value immr and imms = six_value imms in
      if imms = 63 && immr >= 1
      then Some { rd; rn; op = Lsr immr }
      else if immr = imms + 1 && imms <= 62
      then Some { rd; rn; op = Lsl (63 - imms) }
      else None
    | None, _ | _, None -> None)
  | SBFM, Quad (Reg rd, Reg rn, immr, imms) -> (
    match decode_xn rd, decode_xn rn with
    | Some rd, Some rn ->
      let immr = six_value immr and imms = six_value imms in
      if imms = 63 && immr >= 1 then Some { rd; rn; op = Asr immr } else None
    | None, _ | _, None -> None)
  | _, _ -> None

type composed_shift =
  | Comp_ubfm of
      { immr : int;
        imms : int
      }
  | Comp_sbfm of
      { immr : int;
        imms : int
      }
  | Comp_clear_low_bits of int

let compose_shift_ops first second =
  match first, second with
  | Lsl a, Lsr b ->
    Some (Comp_ubfm { immr = (b - a + 64) mod 64; imms = 63 - a })
  | Lsl a, Asr b ->
    Some (Comp_sbfm { immr = (b - a + 64) mod 64; imms = 63 - a })
  | (Lsr a | Asr a), Lsl b ->
    if a = b then Some (Comp_clear_low_bits a) else None
  | Lsl a, Lsl b ->
    if a + b <= 63
    then Some (Comp_ubfm { immr = 64 - (a + b); imms = 63 - (a + b) })
    else None
  | Lsr a, (Lsr b | Asr b) ->
    (* After [lsr #a] with [a >= 1] the top bit is zero, so a following [asr]
       behaves as [lsr]. *)
    if a + b <= 63 then Some (Comp_ubfm { immr = a + b; imms = 63 }) else None
  | Asr a, Asr b -> Some (Comp_sbfm { immr = min (a + b) 63; imms = 63 })
  | Asr _, Lsr _ ->
    (* Not expressible as a single [ubfm]/[sbfm]. *)
    None

let make_composed_shift ~rd ~rn comp : Instruction.t =
  match comp with
  | Comp_ubfm { immr; imms } ->
    Instruction.I
      { name = UBFM;
        operands =
          Quad (DSL.reg_x rd, DSL.reg_x rn, DSL.imm_six immr, DSL.imm_six imms)
      }
  | Comp_sbfm { immr; imms } ->
    Instruction.I
      { name = SBFM;
        operands =
          Quad (DSL.reg_x rd, DSL.reg_x rn, DSL.imm_six immr, DSL.imm_six imms)
      }
  | Comp_clear_low_bits bits ->
    Instruction.I
      { name = AND_immediate;
        operands =
          Triple
            ( DSL.reg_x rd,
              DSL.reg_x rn,
              DSL.bitmask (Nativeint.shift_left (-1n) bits) )
      }

let compose_shift_pairs stats cell =
  match Option.bind (ins_of_cell cell) decode_shift_ins with
  | None -> U.No_match
  | Some s1 -> (
    match U.next_instruction cell with
    | None -> U.No_match
    | Some cell2 -> (
      match Option.bind (ins_of_cell cell2) decode_shift_ins with
      | None -> U.No_match
      | Some s2 -> (
        if not (s2.rn = s1.rd && s2.rd = s1.rd)
        then U.No_match
        else
          match compose_shift_ops s1.op s2.op with
          | None -> U.No_match
          | Some comp ->
            DLL.set_value cell
              (U.Ins (make_composed_shift ~rd:s1.rd ~rn:s1.rn comp));
            U.delete_next_instruction_and_locs cell;
            stats.compose_shift_pairs <- stats.compose_shift_pairs + 1;
            U.Matched (Some cell))))

(* Apply all rewrite rules in sequence using a pipeline. *)
let apply stats cell =
  let[@inline always] if_no_match ~enabled f result =
    match result with
    | U.Matched _ -> result
    | U.No_match -> if enabled then f stats cell else U.No_match
  in
  U.No_match
  |> if_no_match
       ~enabled:!Oxcaml_flags.arm64_peephole_fuse_memory_pairs
       fuse_memory_pairs
  |> if_no_match
       ~enabled:!Oxcaml_flags.arm64_peephole_merge_add_immediates
       merge_add_immediates
  |> if_no_match
       ~enabled:!Oxcaml_flags.arm64_peephole_remove_redundant_cmp
       remove_redundant_cmp
  |> if_no_match
       ~enabled:!Oxcaml_flags.arm64_peephole_compose_shift_pairs
       compose_shift_pairs
