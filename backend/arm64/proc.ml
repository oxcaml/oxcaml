(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                 Benedikt Meurer, University of Siegen                  *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2012 Benedikt Meurer.                                      *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
[@@@ocaml.warning "+a-40-41-42"]
(* Description of the ARM processor in 64-bit mode *)

open! Int_replace_polymorphic_compare

open Misc
open Reg
open Arch

(* Instruction selection *)

let word_addressed = false

(* Registers available for register allocation *)

(* Integer register map:
    x0 - x15              general purpose (caller-save)
    x16, x17              temporaries (used by call veeners)
    x18                   platform register (reserved)
    x19 - x25             general purpose (callee-save)
    x26                   trap pointer
    x27                   alloc pointer
    x28                   domain state pointer
    x29                   frame pointer
    x30                   return address
    sp / xzr              stack pointer / zero register
   Floating-point register map:
    d0 - d7               general purpose (caller-save)
    d8 - d15              general purpose (callee-save)
    d16 - d31             general purpose (caller-save)
   Vector register map:   general purpose (caller-save)
*)

let types_are_compatible left right =
  match left.typ, right.typ with
  | (Int | Val | Addr), (Int | Val | Addr)
  | Float, Float -> true
  | Float32, Float32 -> true
  | Vec128, Vec128 -> true
  | Valx2,Valx2 -> true
  | (Vec256 | Vec512), _ | _, (Vec256 | Vec512) ->
    Misc.fatal_error "arm64: got 256/512 bit vector"
  | (Int | Val | Addr | Float | Float32 | Vec128 | Valx2), _ -> false

(* Representation of hard registers by pseudo-registers *)

let hard_reg_gen typ =
  let reg_class = Regs.Reg_class.of_machtype typ in
  Regs.registers reg_class
  |> Array.map (fun phys_reg ->
     Reg.create_at_location typ (Reg phys_reg))

let hard_int_reg = hard_reg_gen Int
let hard_float_reg = hard_reg_gen Float

let hard_vec128_reg = Array.map (Reg.create_alias ~typ:Vec128) hard_float_reg
let hard_float32_reg = Array.map (Reg.create_alias ~typ:Float32) hard_float_reg

let all_phys_regs =
  Array.concat [hard_int_reg; hard_float_reg; hard_float32_reg; hard_vec128_reg; ]

let precolored_regs =
  let phys_regs = Reg.set_of_array all_phys_regs in
  fun () -> phys_regs

let phys_reg typ phys_reg =
  let index_in_class = Regs.index_in_class phys_reg in
  match (typ : Cmm.machtype_component) with
  | Int | Addr | Val ->
    (* CR yusumez: We need physical registers to have the appropriate machtype
       for the LLVM backend. However, this breaks an invariant the IRC register
       allocator relies on. It is safe to guard it with this flag since the LLVM
       backend doesn't get that far. *)
    let r = hard_int_reg.(index_in_class) in
    if !Clflags.llvm_backend
    then Reg.create_alias r ~typ
    else r
  | Float -> hard_float_reg.(index_in_class)
  | Float32 -> hard_float32_reg.(index_in_class)
  | Vec128 | Valx2 -> hard_vec128_reg.(index_in_class)
  | Vec256 | Vec512 -> Misc.fatal_error "arm64: got 256/512 bit vector"

let reg_x8 = phys_reg Int X8

let stack_slot slot ty =
  Reg.create_at_location ty (Stack slot)

(* Calling conventions *)

let size_domainstate_args = 64 * size_int

let calling_conventions
    int_registers float_registers make_stack first_stack arg =
  let loc = Array.make (Array.length arg) Reg.dummy in
  let int_registers = ref int_registers in
  let float_registers = ref float_registers in
  let ofs = ref first_stack in
  for i = 0 to Array.length arg - 1 do
    let ty : Cmm.machtype_component = arg.(i) in
    let registers, size =
      match ty with
      | Val | Int | Addr -> int_registers, size_int
      | Float | Float32 -> float_registers, Arch.size_float
      | Vec128 -> float_registers, Arch.size_vec128
      | Vec256 | Vec512 -> Misc.fatal_error "arm64: got 256/512 bit vector"
      | Valx2 -> Misc.fatal_error "Unexpected machtype_component Valx2"
    in
    match !registers with
    | reg :: regs ->
      registers := regs;
      loc.(i) <- phys_reg ty reg
    | [] ->
      ofs := Misc.align !ofs size;
      loc.(i) <- stack_slot (make_stack !ofs) ty;
      ofs := !ofs + size;
  done;
  (* CR mslater: (SIMD) will need to be 32/64 if vec256/512 are used. *)
  (loc, Misc.align (max 0 !ofs) 16)  (* keep stack 16-aligned *)

let incoming ofs =
  if ofs >= 0
  then Incoming ofs
  else Domainstate (ofs + size_domainstate_args)
let outgoing ofs =
  if ofs >= 0
  then Outgoing ofs
  else Domainstate (ofs + size_domainstate_args)
let not_supported _ofs = fatal_error "Proc.loc_results: cannot call"

(* OCaml calling convention:
     first integer args in r0...r15
     first float args in d0...d15
     remaining args in domain area, then on stack.
   Return values in r0...r15 or d0...d15. *)

let max_arguments_for_tailcalls = 16 (* in regs *) + 64 (* in domain state *)

let int_registers : Regs.Phys_reg.t list =
  if macosx
  then [X0; X1; X2; X3; X4; X5; X6; X7]
  else [X0; X1; X2; X3; X4; X5; X6; X7; X8; X9; X10; X11; X12; X13; X14; X15]

let float_registers : Regs.Phys_reg.t list =
  [D0; D1; D2; D3; D4; D5; D6; D7; D8; D9; D10; D11; D12; D13; D14; D15]

let loc_arguments arg =
  calling_conventions int_registers float_registers
                      outgoing (- size_domainstate_args) arg
let loc_parameters arg =
  let (loc, _) =
    calling_conventions int_registers float_registers
                        incoming (- size_domainstate_args) arg
  in
  loc
let loc_results_call res =
  calling_conventions
    int_registers float_registers outgoing (- size_domainstate_args) res
let loc_results_return res =
  let (loc, _) =
    calling_conventions
      int_registers float_registers incoming (- size_domainstate_args) res
  in
  loc

(* C calling convention:
     first integer args in r0...r7
     first float args in d0...d7
     remaining args on stack.
   macOS/iOS peculiarities: scalars passed on stack occupy only their size in
   bytes, while the AAPCS64 pads them to 8 bytes.
   Return values in r0...r1 or d0. *)

let external_calling_conventions
    int_registers float_registers make_stack ty_args =
  let loc = Array.make (List.length ty_args) [| Reg.dummy |] in
  let int_registers = ref int_registers in
  let float_registers = ref float_registers in
  let ofs = ref 0 in
  List.iteri (fun i (ty_arg : Cmm.exttype) ->
    let (ty : Cmm.machtype_component), registers, divisor, size =
      match ty_arg with
      | XInt | XInt64 -> Int, int_registers, 1, size_int
      | XInt32 -> Int, int_registers, 2, size_int
      | XInt16 -> Int, int_registers, 4, size_int
      | XInt8 -> Int, int_registers, 8, size_int
      | XFloat -> Float, float_registers, 1, size_float
      | XFloat32 -> Float32, float_registers, 2, size_float
      | XVec128 -> Vec128, float_registers, 1, size_vec128
      | XVec256 | XVec512 ->
        Misc.fatal_error "XVec256 and XVec512 not supported on ARM64"
    in
    match !registers with
    | reg :: regs ->
      registers := regs;
      loc.(i) <- [| phys_reg ty reg |]
    | [] ->
      let size = if macosx then size / divisor else size in
      ofs := Misc.align !ofs size;
      loc.(i) <- [| stack_slot (make_stack !ofs) ty |];
      ofs := !ofs + size)
    ty_args;
  (loc, Misc.align !ofs 16, Cmm.Align_16) (* keep stack 16-aligned *)

let loc_external_arguments ty_args =
  external_calling_conventions
    [X0; X1; X2; X3; X4; X5; X6; X7]
    [D0; D1; D2; D3; D4; D5; D6; D7]
     outgoing ty_args

let loc_external_results res =
  let (loc, _) = calling_conventions [X0; X1] [D0; D1] not_supported 0 res in
  loc

let loc_exn_bucket = phys_reg Int X0

let stack_ptr_dwarf_register_number = 31

let domainstate_ptr_dwarf_register_number = 28

(* Registers destroyed by operations *)

let destroyed_at_c_noalloc_call =
  (* x19-x28, d8-d15 preserved *)
  let int_regs_destroyed_at_c_noalloc_call =
    Regs.[| X0;X1;X2;X3;X4;X5;X6;X7;X8;X9;X10;X11;X12;X13;X14;X15 |]
  in
  let float_regs_destroyed_at_c_noalloc_call =
    Regs.[|D0;D1;D2;D3;D4;D5;D6;D7;
           D16;D17;D18;D19;D20;D21;D22;D23;
           D24;D25;D26;D27;D28;D29;D30;D31|]
  in
  let vec128_regs_destroyed_at_c_noalloc_call =
    (* Registers v8-v15 must be preserved by a callee across
       subroutine calls; the remaining registers (v0-v7, v16-v31) do
       not need to be preserved (or should be preserved by the
       caller). Additionally, only the bottom 64 bits of each value
       stored in v8-v15 need to be preserved [8]; it is the
       responsibility of the caller to preserve larger values.

       https://github.com/ARM-software/abi-aa/blob/
       3952cfbfd2404c442bb6bb6f59ff7b923ab0c148/
       aapcs64/aapcs64.rst?plain=1#L837
    *)
    hard_vec128_reg
  in
  Array.concat [
    Array.map (phys_reg Int) int_regs_destroyed_at_c_noalloc_call;
    Array.map (phys_reg Float) float_regs_destroyed_at_c_noalloc_call;
    Array.map (phys_reg Float32) float_regs_destroyed_at_c_noalloc_call;
    vec128_regs_destroyed_at_c_noalloc_call;
  ]

(* CSE needs to know that all versions of neon are destroyed. *)
let destroy_neon_reg (reg : Regs.Phys_reg.t) =
  [| phys_reg Float reg; phys_reg Float32 reg;
     phys_reg Vec128 reg; |]

let destroy_neon_reg7 = destroy_neon_reg D7

let destroyed_at_raise = all_phys_regs

let destroyed_at_reloadretaddr = [| |]

let destroyed_at_pushtrap = [| |]

let destroyed_at_alloc_or_poll = [| reg_x8 |]

let destroyed_at_basic (basic : Cfg_intf.S.basic) =
  match basic with
  | Reloadretaddr ->
    destroyed_at_reloadretaddr
  | Pushtrap _ ->
    destroyed_at_pushtrap
  | Op Poll -> destroyed_at_alloc_or_poll
  | Op (Alloc _) ->
    destroyed_at_alloc_or_poll
  | Op(Load {memory_chunk = Single { reg = Float64 }; _ }
      | Store(Single { reg = Float64 }, _, _))
    -> destroy_neon_reg7
  | Op (Load {memory_chunk=Single {reg=Float32}; _ })
  | Op (Store (Single {reg=Float32}, _, _))
  | Op (Load
          {memory_chunk=(Byte_unsigned|Byte_signed|Sixteen_unsigned|
                         Sixteen_signed|Thirtytwo_unsigned|Thirtytwo_signed|
                         Word_int|Word_val|Double|Onetwentyeight_unaligned|
                         Onetwentyeight_aligned);
           _ })
  | Op (Store
          ((Byte_unsigned|Byte_signed|Sixteen_unsigned|Sixteen_signed|
            Thirtytwo_unsigned|Thirtytwo_signed|Word_int|Word_val|Double|
            Onetwentyeight_unaligned|Onetwentyeight_aligned),
           _, _))
    -> [||]
  | Op (Static_cast
          (Int_of_float _ | Float_of_int _
          | Float_of_float32|Float32_of_float))
    -> [||]
  | Op (Static_cast
          (V128_of_scalar _|Scalar_of_v128 _))
  | Op (Intop Ipopcnt) ->
      if !Arch.feat_cssc then
        [||]
      else
        destroy_neon_reg7
  | Op (Intop (Iadd  | Isub | Imul | Idiv|Imod|Iand|Ior|Ixor|Ilsl
              |Ilsr|Iasr|Imulh _|Iclz _|Ictz _|Icomp _))
  | Op (Int128op (Iadd128 | Isub128 | Imul64 _))
  | Op (Specific _
        | Move | Spill | Reload | Dummy_use
        | Floatop _
        | Csel _
        | Const_int _
        | Const_float32 _ | Const_float _
        | Const_symbol _ | Const_vec128 _
        | Stackoffset _
        | Intop_imm _ | Intop_atomic _
        | Name_for_debugger _ | Probe_is_enabled _ | Opaque | Pause
        | Begin_region | End_region | Dls_get | Tls_get | Domain_index)
  | Poptrap _ | Prologue | Epilogue
  | Op (Reinterpret_cast (Int_of_value | Value_of_int | Float_of_float32 |
                          Float32_of_float | Float_of_int64 | Int64_of_float |
                          Float32_of_int32 | Int32_of_float32 |
                          V128_of_vec Vec128))
    -> [||]
  | Stack_check _ ->
    (* This case is used by [Cfg_available_regs] *)
    [||]
  | Op (Const_vec256 _ | Const_vec512 _)
  | Op (Load
          {memory_chunk=(Twofiftysix_aligned|Twofiftysix_unaligned|
                         Fivetwelve_aligned|Fivetwelve_unaligned);
           _ })
  | Op (Store
          ((Twofiftysix_aligned|Twofiftysix_unaligned|
            Fivetwelve_aligned|Fivetwelve_unaligned),
            _, _))
  | Op (Reinterpret_cast (V128_of_vec (Vec256 | Vec512) |
                          V256_of_vec _ | V512_of_vec _))
  | Op (Static_cast (V256_of_scalar _ | Scalar_of_v256 _ |
                     V512_of_scalar _ | Scalar_of_v512 _))
    -> Misc.fatal_error "arm64: got 256/512 bit vector"

(* note: keep this function in sync with `is_destruction_point` below. *)
let destroyed_at_terminator (terminator : Cfg_intf.S.terminator) =
  match terminator with
  | Never -> assert false
  | Call {op = Indirect _ | Direct _; _} ->
    all_phys_regs
  | Always _ | Parity_test _ | Truth_test _ | Float_test _
  | Int_test _ | Switch _ | Return | Raise _ | Tailcall_self _
  | Tailcall_func _ | Prim {op = Probe _; _} ->
    [||]
  | Call_no_return { func_symbol = _; alloc; ty_res = _; ty_args = _; stack_ofs; _ }
  | Prim {op  = External { func_symbol = _; alloc; ty_res = _; ty_args = _; stack_ofs; _ }; _} ->
    if alloc || stack_ofs > 0 then all_phys_regs else destroyed_at_c_noalloc_call
  | Invalid { message = _; stack_ofs; stack_align = _; label_after = _ } ->
    if stack_ofs > 0 then all_phys_regs else destroyed_at_c_noalloc_call

(* CR-soon xclerc for xclerc: consider having more destruction points.
   We current return `true` when `destroyed_at_terminator` returns
   `all_phys_regs`; we could also return `true` when `destroyed_at_terminator`
   returns `destroyed_at_c_call` for instance. *)
(* note: keep this function in sync with `destroyed_at_terminator` above. *)
let is_destruction_point ~(more_destruction_points : bool) (terminator : Cfg_intf.S.terminator) =
  match terminator with
  | Never -> assert false
  | Call {op = Indirect _ | Direct _; _} ->
    true
  | Always _ | Parity_test _ | Truth_test _ | Float_test _
  | Int_test _ | Switch _ | Return | Raise _ | Tailcall_self _
  | Tailcall_func _ | Prim {op = Probe _; _} ->
    false
  | Call_no_return { func_symbol = _; alloc; ty_res = _; ty_args = _;
                     stack_ofs; _ }
  | Prim {op  = External { func_symbol = _; alloc; ty_res = _; ty_args = _;
                           stack_ofs; _ }; _ } ->
    if more_destruction_points then
      true
    else
      if alloc || stack_ofs > 0 then true else false
  | Invalid _ -> more_destruction_points

(* Layout of the stack *)

let initial_stack_offset ~num_stack_slots ~contains_calls =
  Stack_class.Tbl.total_size_in_bytes num_stack_slots
  + if contains_calls then 8 else 0

let trap_frame_size_in_bytes = 16

let frame_size ~stack_offset ~contains_calls ~num_stack_slots =
  let sz =
    stack_offset + initial_stack_offset ~num_stack_slots ~contains_calls
  in
  Misc.align sz 16

let frame_required ~fun_contains_calls ~fun_num_stack_slots =
  fun_contains_calls || Stack_class.Tbl.exists fun_num_stack_slots ~f:(fun _stack_class num -> num > 0)

let prologue_required ~fun_contains_calls ~fun_num_stack_slots =
  frame_required ~fun_contains_calls ~fun_num_stack_slots

type slot_offset =
  | Bytes_relative_to_stack_pointer of int
  | Bytes_relative_to_domainstate_pointer of int
[@@ocaml.warning "-37"]

let slot_offset (loc : Reg.stack_location) ~stack_class ~stack_offset
      ~fun_contains_calls ~fun_num_stack_slots =
  match loc with
    Incoming n ->
      assert (n >= 0);
      let frame_size =
        frame_size ~stack_offset ~contains_calls:fun_contains_calls
          ~num_stack_slots:fun_num_stack_slots
      in
      Bytes_relative_to_stack_pointer (frame_size + n)
  | Local n ->
      let offset =
        stack_offset +
        Stack_class.Tbl.offset_in_bytes fun_num_stack_slots ~stack_class ~slot:n
      in
      Bytes_relative_to_stack_pointer offset
  | Outgoing n ->
      assert (n >= 0);
      Bytes_relative_to_stack_pointer n
  | Domainstate n ->
      Bytes_relative_to_domainstate_pointer (
        n + Domainstate.(idx_of_field Domain_extra_params) * 8)

(* Calling the assembler *)

let assemble_file infile outfile =
  let dwarf_flag =
    if !Clflags.native_code && !Clflags.debug then
      Dwarf_flags.get_dwarf_as_toolchain_flag ()
    else
      ""
  in
  Ccomp.command (Config.asm ^ " " ^
                 (String.concat " " (Misc.debug_prefix_map_flags ())) ^
                 dwarf_flag ^
                 " -o " ^ Filename.quote outfile ^ " " ^ Filename.quote infile)

let has_three_operand_float_ops () = false

let operation_supported : Cmm.operation -> bool = function
  | Caddi128 | Csubi128 | Cmuli64 _ ->
    (* CR mslater: restore after the arm DSL is merged *)
    false
  | Cprefetch _ | Catomic _
  | Creinterpret_cast (V128_of_vec (Vec256 | Vec512) |
                       V256_of_vec _ | V512_of_vec _)
  | Cstatic_cast (V256_of_scalar _ | Scalar_of_v256 _ |
                  V512_of_scalar _ | Scalar_of_v512 _) ->
    false
  | Cpopcnt
  | Cnegf Float32 | Cabsf Float32 | Caddf Float32
  | Csubf Float32 | Cmulf Float32 | Cdivf Float32
  | Cpackf32
  | Cclz _ | Cctz _ | Cbswap _
  | Capply _ | Cextcall _ | Cload _ | Calloc _ | Cstore _
  | Caddi | Csubi | Cmuli | Cmulhi _ | Cdivi | Cmodi
  | Cand | Cor | Cxor | Clsl | Clsr | Casr
  | Ccmpi _ | Caddv | Cadda
  | Cnegf Float64 | Cabsf Float64 | Caddf Float64
  | Csubf Float64 | Cmulf Float64 | Cdivf Float64
  | Ccmpf _
  | Ccsel _
  | Craise _
  | Cprobe _ | Cprobe_is_enabled _ | Copaque | Cpause
  | Cbeginregion | Cendregion | Ctuple_field _
  | Cdls_get
  | Ctls_get
  | Cdomain_index
  | Cpoll
  | Creinterpret_cast (Int_of_value | Value_of_int |
                       Int64_of_float | Float_of_int64 |
                       Float32_of_float | Float_of_float32 |
                       Float32_of_int32 | Int32_of_float32 |
                       V128_of_vec Vec128)
  | Cstatic_cast (Float_of_float32 | Float32_of_float |
                  Int_of_float Float32 | Float_of_int Float32 |
                  Float_of_int Float64 | Int_of_float Float64 |
                  V128_of_scalar _ | Scalar_of_v128 _) ->
    true

let expression_supported : Cmm.expression -> bool = function
  | Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
  | Cconst_vec128 _ | Cconst_symbol _  | Cvar _ | Clet _ | Cphantom_let _
  | Ctuple _ | Cop _ | Csequence _ | Cifthenelse _ | Cswitch _ | Ccatch _
  | Cexit _ | Cinvalid _ -> true
  | Cconst_vec256 _ | Cconst_vec512 _ -> false


let trap_size_in_bytes () =
  if !Clflags.llvm_backend
  then
    Misc.fatal_error
      "Proc.trap_size_in_bytes: LLVM backend not supported for ARM"
  else 16
