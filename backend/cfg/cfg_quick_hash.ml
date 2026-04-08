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
  | Dummy_use -> 3
  | Opaque -> 4
  | Begin_region -> 5
  | End_region -> 6
  | Dls_get -> 7
  | Tls_get -> 8
  | Domain_index -> 9
  | Poll -> 10
  | Pause -> 11
  | Const_int _ -> 12
  | Const_float32 _ -> 13
  | Const_float _ -> 14
  | Const_symbol _ -> 15
  | Const_vec128 _ -> 16
  | Const_vec256 _ -> 17
  | Const_vec512 _ -> 18
  | Stackoffset _ -> 19
  | Load _ -> 20
  | Store _ -> 21
  | Intop _ -> 22
  | Int128op _ -> 23
  | Intop_imm _ -> 24
  | Intop_atomic _ -> 25
  | Floatop _ -> 26
  | Csel _ -> 27
  | Reinterpret_cast _ -> 28
  | Static_cast _ -> 29
  | Probe_is_enabled _ -> 30
  | Specific _ -> 31
  | Name_for_debugger _ -> 32
  | Alloc _ -> 33

let basic : Cfg.basic -> int = function
  | Op op -> operation op
  | Reloadretaddr -> 1
  | Pushtrap { lbl_handler = _ } -> 2
  | Poptrap { lbl_handler = _ } -> 3
  | Prologue -> 4
  | Epilogue -> 5
  | Stack_check { max_frame_size_bytes = _ } -> 6

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
    Cfg.basic Cfg.instruction DLL.cell option -> fuel:int -> acc:int -> int =
 fun cell ~fuel ~acc ->
  if fuel <= 0
  then acc
  else
    match cell with
    | None -> acc
    | Some cell ->
      let instr_hash = basic (DLL.value cell).desc in
      basic_instruction_cell (DLL.next cell) ~fuel:(pred fuel)
        ~acc:(hash_combine acc instr_hash)

let basic_instruction_list : Cfg.basic Cfg.instruction DLL.t -> int =
 fun l ->
  (* CR-someday xclerc for xclerc: also use the list length? *)
  basic_instruction_cell (DLL.hd_cell l) ~fuel:4 ~acc:0

let basic_block : Cfg.basic_block -> int =
 fun block ->
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
    let body_hash = basic_instruction_list body in
    let term_hash = terminator terminator_desc in
    hash_combine body_hash term_hash
