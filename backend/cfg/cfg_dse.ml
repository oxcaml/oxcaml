[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
module Array = ArrayLabels
module DLL = Doubly_linked_list
module List = ListLabels

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

module Key = struct
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
end

module Key_set = Set.Make (Key)

let key_mentions_reg { Key.addr_regs; _ } (regs : Reg.t array) : bool =
  Array.exists addr_regs ~f:(fun stamp ->
      Array.exists regs ~f:(fun (reg : Reg.t) ->
          Reg.Stamp.equal reg.Reg.stamp stamp))

(* Instructions over which dead store elimination may step: guaranteed not to
   read from memory, not to raise, and not to trigger the execution of arbitrary
   code. They may write to memory (e.g. target-specific stores of immediate
   values). *)
let is_transparent (instr : Cfg.basic Cfg.instruction) : bool =
  match instr.desc with
  | Op op -> (
    match op with
    | Move | Const_int _ | Const_float32 _ | Const_float _ | Const_symbol _
    | Const_vec128 _ | Const_vec256 _ | Const_vec512 _ | Const_mask _
    | Int128op _ | Floatop _ | Csel _ | Reinterpret_cast _ | Static_cast _
    | Name_for_debugger _ | Intop _ | Intop_imm _ ->
      true
    | Specific specific -> Arch.operation_is_pure_except_memory_writes specific
    | Spill | Reload | Load _ | Store _ | Intop_atomic _ | Opaque
    | Stackoffset _ | Probe_is_enabled _ | Begin_region | End_region | Dls_get
    | Tls_get | Domain_index | Poll | Pause | Alloc _ ->
      false)
  | Reloadretaddr | Pushtrap _ | Poptrap _ | Prologue | Epilogue | Stack_check _
    ->
    false

let process_body (body : Cfg.basic Cfg.instruction DLL.t) : unit =
  let candidates = ref Key_set.empty in
  let invalidate_regs (regs : Reg.t array) =
    if Array.length regs > 0 && not (Key_set.is_empty !candidates)
    then
      candidates
        := Key_set.filter
             (fun key -> not (key_mentions_reg key regs))
             !candidates
  in
  DLL.iter_right_cell body ~f:(fun cell ->
      let instr : Cfg.basic Cfg.instruction = DLL.value cell in
      match instr.desc with
      | Op (Store (memory_chunk, addressing_mode, _)) ->
        let addr_regs =
          Array.map
            (Array.sub instr.arg ~pos:1 ~len:(Array.length instr.arg - 1))
            ~f:(fun (reg : Reg.t) -> reg.Reg.stamp)
        in
        let key : Key.t =
          { size_in_bytes = Cmm.size_of_memory_chunk memory_chunk;
            addressing_mode;
            addr_regs
          }
        in
        if Key_set.mem key !candidates
        then DLL.delete_curr cell
        else
          let destroyed = Proc.destroyed_at_basic instr.desc in
          invalidate_regs destroyed;
          if not (key_mentions_reg key destroyed)
          then candidates := Key_set.add key !candidates
      | Op
          ( Move | Spill | Reload | Const_int _ | Const_float32 _
          | Const_float _ | Const_symbol _ | Const_vec128 _ | Const_vec256 _
          | Const_vec512 _ | Const_mask _ | Stackoffset _ | Load _ | Intop _
          | Int128op _ | Intop_imm _ | Intop_atomic _ | Floatop _ | Csel _
          | Reinterpret_cast _ | Static_cast _ | Probe_is_enabled _ | Opaque
          | Begin_region | End_region | Specific _ | Name_for_debugger _
          | Dls_get | Tls_get | Domain_index | Poll | Pause | Alloc _ )
      | Reloadretaddr | Pushtrap _ | Poptrap _ | Prologue | Epilogue
      | Stack_check _ ->
        if is_transparent instr
        then (
          invalidate_regs instr.res;
          invalidate_regs (Proc.destroyed_at_basic instr.desc))
        else candidates := Key_set.empty)

let run : Cfg_with_layout.t -> Cfg_with_layout.t =
 fun cfg_with_layout ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  (* The pass is opt-in ([-cfg-dse]). [No_CSE] is currently only used to skip
     the (potentially expensive) backend CSE of toplevel entry functions; there
     is little point spending time optimizing stores there either, so honor it
     here too. *)
  if
    !Oxcaml_flags.cfg_dse
    && not (List.mem ~set:cfg.fun_codegen_options Cfg.No_CSE)
  then Cfg.iter_blocks cfg ~f:(fun _label block -> process_body block.body);
  cfg_with_layout
