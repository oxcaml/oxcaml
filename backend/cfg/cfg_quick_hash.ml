[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare
module DLL = Oxcaml_utils.Doubly_linked_list

let[@inline] hash_combine h1 h2 = (h1 * 65599) + h2

let operation : Operation.t -> int =
 fun op ->
  match op with
  | Move -> 0
  | Spill -> 1
  | Reload -> 2
  | Opaque -> 3
  | Begin_region -> 4
  | End_region -> 5
  | Dls_get -> 6
  | Tls_get -> 7
  | Domain_index -> 8
  | Poll -> 9
  | Pause -> 10
  | Const_int _ -> 11
  | Const_float32 _ -> 12
  | Const_float _ -> 13
  | Const_symbol _ -> 14
  | Const_vec128 _ -> 15
  | Const_vec256 _ -> 16
  | Const_vec512 _ -> 17
  | Stackoffset _ -> 18
  | Load _ -> 19
  | Store _ -> 20
  | Intop _ -> 21
  | Int128op _ -> 22
  | Intop_imm _ -> 23
  | Intop_atomic _ -> 24
  | Floatop _ -> 25
  | Csel _ -> 26
  | Reinterpret_cast _ -> 27
  | Static_cast _ -> 28
  | Probe_is_enabled _ -> 29
  | Specific _ -> 30
  | Name_for_debugger _ -> 31
  | Alloc _ -> 32

let basic : Cfg.basic -> int = function
  | Op op -> operation op
  | Reloadretaddr -> 1
  | Pushtrap { lbl_handler = _ } -> 2
  | Poptrap { lbl_handler = _ } -> 3
  | Prologue -> 4
  | Epilogue -> 5
  | Stack_check { max_frame_size_bytes } -> hash_combine 6 max_frame_size_bytes

let terminator : Cfg.terminator -> int =
 fun term ->
  match term with
  | Never -> 0
  | Return -> 1
  | Always _ -> 2
  | Parity_test _ -> 3
  | Truth_test _ -> 4
  | Float_test _ -> 5
  | Int_test _ -> 6
  | Switch _ -> 7
  | Raise _ -> 8
  | Tailcall_self _ -> 9
  | Tailcall_func _ -> 10
  | Call_no_return _ -> 11
  | Invalid _ -> 12
  | Call _ -> 13
  | Prim _ -> 14

let rec basic_instruction_cell :
    ignore_name_for_debugger:bool ->
    Cfg.basic Cfg.instruction DLL.cell option ->
    fuel:int ->
    acc:int ->
    int =
 fun ~ignore_name_for_debugger cell ~fuel ~acc ->
  if fuel <= 0
  then acc
  else
    match cell with
    | None -> acc
    | Some cell ->
      let instr = DLL.value cell in
      if ignore_name_for_debugger && Cfg.is_name_for_debugger instr
      then
        basic_instruction_cell ~ignore_name_for_debugger (DLL.next cell) ~fuel
          ~acc
      else
        let instr_hash = basic instr.desc in
        basic_instruction_cell ~ignore_name_for_debugger (DLL.next cell)
          ~fuel:(pred fuel)
          ~acc:(hash_combine acc instr_hash)

let basic_instruction_list :
    ignore_name_for_debugger:bool -> Cfg.basic Cfg.instruction DLL.t -> int =
 fun ~ignore_name_for_debugger l ->
  (* CR-someday xclerc for xclerc: also use the list length? *)
  basic_instruction_cell ~ignore_name_for_debugger (DLL.hd_cell l) ~fuel:4
    ~acc:0

let basic_block : ignore_name_for_debugger:bool -> Cfg.basic_block -> int =
 fun ~ignore_name_for_debugger block ->
  match block with
  | { start = _;
      body;
      terminator =
        { desc = terminator_desc;
          id = _;
          arg = _;
          res = _;
          dbg = _;
          fdo = _;
          live = _;
          stack_offset = _;
          available_before = _;
          available_across = _
        };
      predecessors = _;
      stack_offset = _;
      exn = _;
      can_raise = _;
      is_trap_handler = _;
      cold = _
    } ->
    let body_hash = basic_instruction_list ~ignore_name_for_debugger body in
    let term_hash = terminator terminator_desc in
    hash_combine body_hash term_hash

(* The hash walks at most [fuel] blocks of the layout: enough to discriminate
   most functions while keeping the cost bounded for very large CFGs. *)
let cfg_with_layout ~ignore_name_for_debugger cfg_with_layout =
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let layout = Cfg_with_layout.layout cfg_with_layout in
  let rec hash label_cell ~fuel ~acc =
    match label_cell with
    | None -> acc
    | Some cell ->
      if fuel <= 0
      then acc
      else
        let label = DLL.value cell in
        let block = Cfg.get_block_exn cfg label in
        hash (DLL.next cell) ~fuel:(pred fuel)
          ~acc:(hash_combine acc (basic_block ~ignore_name_for_debugger block))
  in
  hash (DLL.hd_cell layout) ~fuel:4 ~acc:0
