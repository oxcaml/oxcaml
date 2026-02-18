[@@@ocaml.warning "+a-40-41-42"]

module Save_simd_regs = struct
  type t =
    | Save_none
    | Save_xmm
    | Save_ymm
    | Save_zmm

  let all = [Save_none; Save_xmm; Save_ymm; Save_zmm]

  let extension_name : t -> string option = function
    | Save_none -> None
    | Save_xmm -> Some "sse"
    | Save_ymm -> Some "avx"
    | Save_zmm -> Some "avx512"

  let symbol_suffix t =
    match extension_name t with None -> "" | Some ext -> "_" ^ ext
end

module T = struct
  type reg_class =
    | GPR
    | SIMD

  type[@ocamlformat "disable"] _ phys_reg_classed =
    | RAX : [`GPR] phys_reg_classed | RBX : [`GPR] phys_reg_classed
    | RDI : [`GPR] phys_reg_classed | RSI : [`GPR] phys_reg_classed
    | RDX : [`GPR] phys_reg_classed | RCX : [`GPR] phys_reg_classed
    | R8  : [`GPR] phys_reg_classed | R9  : [`GPR] phys_reg_classed
    | R12 : [`GPR] phys_reg_classed | R13 : [`GPR] phys_reg_classed
    | R10 : [`GPR] phys_reg_classed | R11 : [`GPR] phys_reg_classed
    | RBP : [`GPR] phys_reg_classed

    | MM0  : [`SIMD] phys_reg_classed | MM1  : [`SIMD] phys_reg_classed
    | MM2  : [`SIMD] phys_reg_classed | MM3  : [`SIMD] phys_reg_classed
    | MM4  : [`SIMD] phys_reg_classed | MM5  : [`SIMD] phys_reg_classed
    | MM6  : [`SIMD] phys_reg_classed | MM7  : [`SIMD] phys_reg_classed
    | MM8  : [`SIMD] phys_reg_classed | MM9  : [`SIMD] phys_reg_classed
    | MM10 : [`SIMD] phys_reg_classed | MM11 : [`SIMD] phys_reg_classed
    | MM12 : [`SIMD] phys_reg_classed | MM13 : [`SIMD] phys_reg_classed
    | MM14 : [`SIMD] phys_reg_classed | MM15 : [`SIMD] phys_reg_classed

  type phys_reg = P : _ phys_reg_classed -> phys_reg [@@unboxed]

  let phys_gpr_regs_sans_frame_pointer_classed =
    [| RAX; RBX; RDI; RSI; RDX; RCX; R8; R9; R12; R13; R10; R11 |]

  let phys_gpr_regs_classed =
    Array.append phys_gpr_regs_sans_frame_pointer_classed [| RBP |]

  let[@ocamlformat "disable"] phys_simd_regs_classed =
    [| MM0;  MM1;  MM2;  MM3;  MM4;  MM5;  MM6;  MM7;
       MM8;  MM9;  MM10; MM11; MM12; MM13; MM14; MM15 |]

  module Reg_class = struct
    type t = reg_class

    let of_machtype : Cmm.machtype_component -> t = function
      | Val | Int | Addr -> GPR
      | Float | Float32 | Vec128 | Vec256 | Vec512 | Valx2 -> SIMD

    let all = [GPR; SIMD]

    let[@inline] to_int : t -> int = function GPR -> 0 | SIMD -> 1

    let hash = to_int

    let equal l r = Int.equal (to_int l) (to_int r)

    let print : Format.formatter -> t -> unit =
     fun ppf reg_class ->
      Format.fprintf ppf "%s"
        (match reg_class with GPR -> "GPR" | SIMD -> "SIMD")
  end

  module Phys_reg = struct
    type t = phys_reg

    let[@ocamlformat "disable"] name = function
      | P RAX -> "rax" | P RBX -> "rbx" | P RDI -> "rdi" | P RSI -> "rsi"
      | P RDX -> "rdx" | P RCX -> "rcx" | P R8  -> "r8"  | P R9  -> "r9"
      | P R12 -> "r12" | P R13 -> "r13" | P R10 -> "r10" | P R11 -> "r11"
      | P RBP -> "rbP"

      | P MM0  -> "mm0"  | P MM1 -> "mm1"   | P MM2  -> "mm2"  | P MM3  -> "mm3"
      | P MM4  -> "mm4"  | P MM5 -> "mm5"   | P MM6  -> "mm6"  | P MM7  -> "mm7"
      | P MM8  -> "mm8"  | P MM9 -> "mm9"   | P MM10 -> "mm10" | P MM11 -> "mm11"
      | P MM12 -> "mm12" | P MM13 -> "mm13" | P MM14 -> "mm14" | P MM15 -> "mm15"

    let[@inline][@ocamlformat "disable"] to_int : t -> int = function
      | P RAX  -> 0  | P RBX  -> 1  | P RDI  -> 2  | P RSI  -> 3
      | P RDX  -> 4  | P RCX  -> 5  | P R8   -> 6  | P R9   -> 7
      | P R12  -> 8  | P R13  -> 9  | P R10  -> 10 | P R11  -> 11
      | P RBP  -> 12

      | P MM0  -> 13 | P MM1  -> 14 | P MM2  -> 15 | P MM3  -> 16
      | P MM4  -> 17 | P MM5  -> 18 | P MM6  -> 19 | P MM7  -> 20
      | P MM8  -> 21 | P MM9  -> 22 | P MM10 -> 23 | P MM11 -> 24
      | P MM12 -> 25 | P MM13 -> 26 | P MM14 -> 27 | P MM15 -> 28

    include Identifiable.Make (struct
      type t = phys_reg

      let hash = to_int

      let equal l r = Int.equal (to_int l) (to_int r)

      let compare l r = Int.compare (to_int l) (to_int r)

      let output out_channel t = Out_channel.output_string out_channel (name t)

      let print formatter t = Format.pp_print_string formatter (name t)
    end)

    let[@inline] reg_class = function
      | P
          ( RAX | RBX | RDI | RSI | RDX | RCX | R8 | R9 | R12 | R13 | R10 | R11
          | RBP ) ->
        GPR
      | P
          ( MM0 | MM1 | MM2 | MM3 | MM4 | MM5 | MM6 | MM7 | MM8 | MM9 | MM10
          | MM11 | MM12 | MM13 | MM14 | MM15 ) ->
        SIMD
  end

  let phys_gpr_regs_sans_frame_pointer =
    Array.map (fun p -> P p) phys_gpr_regs_sans_frame_pointer_classed

  let phys_gpr_regs = Array.map (fun p -> P p) phys_gpr_regs_classed

  let phys_simd_regs = Array.map (fun p -> P p) phys_simd_regs_classed

  let[@inline] registers = function
    | GPR -> phys_gpr_regs
    | SIMD -> phys_simd_regs

  let[@inline] registers_sans_frame_pointer = function
    | GPR -> phys_gpr_regs_sans_frame_pointer
    | SIMD -> phys_simd_regs

  let available_registers =
    if Config.with_frame_pointers
    then registers_sans_frame_pointer
    else registers

  let num_available_registers reg_class =
    Array.length (available_registers reg_class)

  (* See "System V Application Binary Interface, AMD64 Architecture Processor
     Supplement" (www.x86-64.org/documentation/abi.pdf) page 57, fig. 3.36. *)
  let[@ocamlformat "disable"] dwarf_reg_number = function
    | P RAX  -> 0  | P RDX  -> 1  | P RCX  -> 2  | P RBX  -> 3
    | P RSI  -> 4  | P RDI  -> 5  | P RBP  -> 6
    | P R8   -> 8  | P R9   -> 9  | P R10  -> 10 | P R11  -> 11
    | P R12  -> 12 | P R13  -> 13

    | P MM0  -> 17 | P MM1  -> 18 | P MM2  -> 19 | P MM3  -> 20
    | P MM4  -> 21 | P MM5  -> 22 | P MM6  -> 23 | P MM7  -> 24
    | P MM8  -> 25 | P MM9  -> 26 | P MM10 -> 27 | P MM11 -> 28
    | P MM12 -> 29 | P MM13 -> 30 | P MM14 -> 31 | P MM15 -> 32

  let check_typ_reg_class typ phys_reg =
    let typ_reg_class = Reg_class.of_machtype typ in
    let phys_reg_reg_class = Phys_reg.reg_class phys_reg in
    if not (Reg_class.equal typ_reg_class phys_reg_reg_class)
    then
      Misc.fatal_errorf
        "Class %a of register %a does not match class %a of type"
        Reg_class.print phys_reg_reg_class Phys_reg.print phys_reg
        Reg_class.print typ_reg_class
    else typ_reg_class

  let dwarf_reg_number typ phys_reg =
    let _ : reg_class = check_typ_reg_class typ phys_reg in
    dwarf_reg_number phys_reg

  let to_target_name =
    let target_prefix =
      match Config.ccomp_type with "msvc" -> "" | _ -> "%"
    in
    fun ~kind ->
      let prefix = target_prefix ^ kind in
      fun phys_reg -> prefix ^ Phys_reg.name phys_reg

  let gpr_name = phys_gpr_regs |> Array.map (to_target_name ~kind:"")

  let xmm_name = phys_simd_regs |> Array.map (to_target_name ~kind:"x")

  let ymm_name = phys_simd_regs |> Array.map (to_target_name ~kind:"y")

  let zmm_name = phys_simd_regs |> Array.map (to_target_name ~kind:"z")

  let first_in_class reg_class = Phys_reg.to_int (registers reg_class).(0)

  let index_in_class phys_reg =
    let reg_class = Phys_reg.reg_class phys_reg in
    Phys_reg.to_int phys_reg - first_in_class reg_class

  let register_name typ phys_reg =
    let _ : reg_class = check_typ_reg_class typ phys_reg in
    let index_in_class = index_in_class phys_reg in
    let names =
      match (typ : Cmm.machtype_component) with
      | Int | Addr | Val -> gpr_name
      | Float | Float32 | Vec128 | Valx2 -> xmm_name
      | Vec256 -> ymm_name
      | Vec512 -> zmm_name
    in
    names.(index_in_class)

  let[@inline] gc_regs_offset ~(simd : Save_simd_regs.t) typ phys_reg =
    (* Given register with type [typ] and index [reg_index], return the offset
       (the number of [value] slots, not their size in bytes) of the register
       from the [gc_regs] pointer during GC at runtime. Keep in sync with
       [amd64.S]. *)
    let reg_class = check_typ_reg_class typ phys_reg in
    let index = index_in_class phys_reg in
    match reg_class with
    | GPR -> index
    | SIMD ->
      let slot_size_in_vals =
        match simd with
        | Save_none ->
          Misc.fatal_error "reg_class=SIMD, but no simd regs are live"
        | Save_xmm -> 2
        | Save_ymm -> 4
        | Save_zmm -> 8
      in
      if Config.runtime5
      then
        (* simd slots are above regular slots based at [gc_regs_bucket] *)
        let num_regular_slots =
          (* rbp is always spilled even without frame pointers *)
          Array.length phys_gpr_regs
        in
        num_regular_slots + (index * slot_size_in_vals)
      else
        (* simd slots are below [gc_regs] pointer *)
        let num_simd_slots =
          match simd with
          | Save_none ->
            Misc.fatal_error "reg_class=SIMD, but no simd regs are live"
          | Save_xmm | Save_ymm -> 16
          | Save_zmm -> 32
        in
        let offset = Int.neg (num_simd_slots * slot_size_in_vals) in
        offset + (index * slot_size_in_vals)
end

include T
module Reg_class_tbl = Regs_utils.Make_reg_class_tbl (T)
