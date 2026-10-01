[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
module Array = ArrayLabels
module DLL = Doubly_linked_list

(* Dead store elimination.

   A store is deleted when the location it writes (same access size, same
   addressing mode, same address registers) is entirely overwritten by a later
   store in the same basic block, with no intervening instruction that could
   observe the location's contents: no memory read, no allocation or poll (the
   GC, and any code it triggers such as finalizers or signal handlers, may read
   memory), no atomic operation or fence, and nothing that could raise
   (including via a hardware trap). Intervening writes, even to potentially
   aliasing locations, are harmless: since nothing reads between the two stores,
   the intermediate memory states cannot be observed, and the final contents of
   every location are unchanged by deleting the earlier store. As in [Cfg_cse],
   no attempt is made to preserve the values observed by racing accesses from
   other threads: plain (non-atomic) racing accesses provide no such guarantee,
   and atomic operations and fences act as barriers here.

   The scan over a block body is backward, maintaining a set of location keys: a
   key being in the set means that the corresponding location is overwritten
   later in the block, before anything can observe it. *)

module Key : sig
  (* A memory location written by a store, identified by the access size, the
     addressing mode and the address registers. *)
  type t

  (* The location written by a store of [memory_chunk] through
     [addressing_mode], where [args] are the arguments of the store instruction:
     the stored value followed by the address registers. *)
  val of_store :
    memory_chunk:Cmm.memory_chunk ->
    addressing_mode:Arch.addressing_mode ->
    args:Reg.t array ->
    t

  val compare : t -> t -> int

  (* [mentions_reg t reg] is [true] iff [reg] is one of the address registers of
     [t]. *)
  val mentions_reg : t -> Reg.t -> bool
end = struct
  (* The access size (rather than the memory chunk) is what determines whether a
     later store entirely overwrites an earlier one: chunks are used
     asymmetrically (e.g. [Word_int] stores can be reloaded as [Word_val]). *)
  type t =
    { size_in_bytes : int;
      addressing_mode : Arch.addressing_mode;
      addr_regs : Reg.Stamp.t array
          (* Stamps rather than [Reg.t]s compared with their [typ] (as
             [Reg.same] would): a stamp identifies a machine register, and
             physical registers aliased at different types share stamps.
             Identifying them is what we want here -- same stamp means same
             address value, and invalidation must match a redefinition even
             under a different [typ] view of the same physical register. *)
    }

  let of_store ~memory_chunk ~addressing_mode ~(args : Reg.t array) =
    let addr_regs =
      Array.map
        (Array.sub args ~pos:1 ~len:(Array.length args - 1))
        ~f:(fun (reg : Reg.t) -> reg.Reg.stamp)
    in
    { size_in_bytes = Cmm.size_of_memory_chunk memory_chunk;
      addressing_mode;
      addr_regs
    }

  let compare
      { size_in_bytes = size1; addressing_mode = mode1; addr_regs = regs1 }
      { size_in_bytes = size2; addressing_mode = mode2; addr_regs = regs2 } =
    let c = Int.compare size1 size2 in
    if c <> 0
    then c
    else
      let c = Arch.compare_addressing_mode mode1 mode2 in
      if c <> 0
      then c
      else Misc.Stdlib.Array.compare Reg.Stamp.compare regs1 regs2

  let mentions_reg { addr_regs; _ } (reg : Reg.t) =
    Array.exists addr_regs ~f:(fun stamp -> Reg.Stamp.equal reg.Reg.stamp stamp)
end

module Key_set = Set.Make (Key)

(* Instructions that are pure except possibly for storing to memory: guaranteed
   not to read from memory, not to raise, and not to trigger the execution of
   arbitrary code. Dead store elimination steps over them after forgetting the
   candidates whose address registers they write. Generic [Store]s are the
   subject of the analysis and are handled by [process_instr] before this
   predicate is consulted. *)
let is_pure_except_stores (instr : Cfg.basic Cfg.instruction) : bool =
  match instr.desc with
  | Op op -> (
    match op with
    | Move | Const_int _ | Const_float32 _ | Const_float _ | Const_symbol _
    | Const_vec128 _ | Const_vec256 _ | Const_vec512 _ | Const_mask _
    | Int128op _ | Floatop _ | Csel _ | Reinterpret_cast _ | Static_cast _
    | Name_for_debugger _ | Intop _ | Intop_imm _ | Store _ ->
      true
    | Specific specific -> Arch.operation_is_pure_except_stores specific
    | Spill | Reload | Load _ | Intop_atomic _ | Opaque | Stackoffset _
    | Probe_is_enabled _ | Begin_region | End_region | Dls_get | Tls_get
    | Domain_index | Poll | Pause | Alloc _ ->
      false)
  | Reloadretaddr | Pushtrap _ | Poptrap _ | Prologue | Epilogue | Stack_check _
    ->
    false

(* Forget the candidate dead stores whose address registers are among [regs]:
   the values of these registers before the instruction writing them may differ
   from the ones the later stores used. *)
let invalidate_regs (candidate_dead_stores : Key_set.t ref) (regs : Reg.t array)
    : unit =
  if Array.length regs > 0 && not (Key_set.is_empty !candidate_dead_stores)
  then
    candidate_dead_stores
      := Key_set.filter
           (fun key -> not (Array.exists regs ~f:(Key.mentions_reg key)))
           !candidate_dead_stores

(* Step backward over [instr]: forget the candidates whose address registers
   [instr] defines or destroys. *)
let invalidate_regs_written_by (candidate_dead_stores : Key_set.t ref)
    (instr : Cfg.basic Cfg.instruction) : unit =
  invalidate_regs candidate_dead_stores instr.res;
  invalidate_regs candidate_dead_stores (Proc.destroyed_at_basic instr.desc)

let process_instr (candidate_dead_stores : Key_set.t ref)
    (cell : Cfg.basic Cfg.instruction DLL.cell) : unit =
  let instr = DLL.value cell in
  match instr.desc with
  | Op (Store (memory_chunk, addressing_mode, _)) ->
    let key = Key.of_store ~memory_chunk ~addressing_mode ~args:instr.arg in
    if Key_set.mem key !candidate_dead_stores
    then DLL.delete_curr cell
    else (
      invalidate_regs_written_by candidate_dead_stores instr;
      (* A store reads its address registers before it writes any register, so
         [key] describes the location written at the program point just before
         [instr], whatever [instr] defines or destroys: an earlier store to
         [key], with nothing observing the location in between, is dead. *)
      candidate_dead_stores := Key_set.add key !candidate_dead_stores)
  | Op
      ( Move | Spill | Reload | Const_int _ | Const_float32 _ | Const_float _
      | Const_symbol _ | Const_vec128 _ | Const_vec256 _ | Const_vec512 _
      | Const_mask _ | Stackoffset _ | Load _ | Intop _ | Int128op _
      | Intop_imm _ | Intop_atomic _ | Floatop _ | Csel _ | Reinterpret_cast _
      | Static_cast _ | Probe_is_enabled _ | Opaque | Begin_region | End_region
      | Specific _ | Name_for_debugger _ | Dls_get | Tls_get | Domain_index
      | Poll | Pause | Alloc _ )
  | Reloadretaddr | Pushtrap _ | Poptrap _ | Prologue | Epilogue | Stack_check _
    ->
    if not (is_pure_except_stores instr)
    then candidate_dead_stores := Key_set.empty
    else invalidate_regs_written_by candidate_dead_stores instr

let process_body (body : Cfg.basic Cfg.instruction DLL.t) : unit =
  let candidate_dead_stores = ref Key_set.empty in
  DLL.iter_right_cell body ~f:(process_instr candidate_dead_stores)

let run : Cfg_with_layout.t -> Cfg_with_layout.t =
 fun cfg_with_layout ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  if !Oxcaml_flags.cfg_dse
  then Cfg.iter_blocks cfg ~f:(fun _label block -> process_body block.body);
  cfg_with_layout
