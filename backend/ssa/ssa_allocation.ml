open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-40-41-42"]

open Ssa.Export

type t =
  { allocation : finished Instruction.op_data;
    initialization : finished Instruction.op_data list
  }

(* Whether [value] is result 0 of [op]. Used to check that store addresses and
   address-reset operands come from the expected defining instructions. *)
let is_result (op : finished Instruction.op_data) (value : finished Value.t) =
  match value with
  | Res (other, 0) -> Instruction.Id.equal op.id other.id
  | Res (_, _) | Block_param _ -> false

(* Match the store forms emitted by [Ssa_of_cmm.emit_stores], including the
   target-specific immediate stores. The size is that of the field slot, not
   necessarily that of the write: float32 fields occupy a full word. *)
let initializing_store_size ~base ~addressing ~is_header
    (instr : finished Instruction.op_data) : int option =
  if Array.length instr.typ <> 0
  then None
  else
    (* note: it is safe to fail to recognize an initializing store (we leave
       unrecognized allocation in place, and they act as barrier for others) *)
    match instr.op, instr.args with
    | Store (chunk, addr, false), [| value; address |]
      when is_result base address && Arch.equal_addressing_mode addr addressing
      -> (
      match Value.typ value with
      | Valx2 -> None
      | (Val | Addr | Int | Float | Float32 | Vec128 | Vec256 | Vec512 | Mask)
        as typ ->
        if
          ((not is_header) || Cmm.equal_machtype_component typ Int)
          && Cmm.equal_memory_chunk chunk
               (Select_utils.chunk_of_machtype_component typ)
        then Some (Select_utils.size_component typ)
        else None)
    | Specific op, [| address |]
      when is_result base address
           && Arch.is_immediate_initializing_store op ~addressing ->
      Some Arch.size_int
    | ( ( Move | Spill | Reload | Const_int _ | Const_float32 _ | Const_float _
        | Const_symbol _ | Const_vec128 _ | Const_vec256 _ | Const_vec512 _
        | Const_mask _ | Stackoffset _ | Load _ | Store _ | Intop _ | Int128op _
        | Intop_imm _ | Intop_atomic _ | Floatop _ | Csel _ | Reinterpret_cast _
        | Static_cast _ | Probe_is_enabled _ | Opaque | Begin_region
        | End_region | Specific _ | Name_for_debugger _ | Dls_get | Tls_get
        | Domain_index | Poll | Pause | Alloc _ ),
        _ ) ->
      None

(* Match the constant/addition pair that computes [base + offset]. *)
let is_address_reset ~base ~offset (constant : finished Instruction.op_data)
    (address : finished Instruction.op_data) : bool =
  match[@warning "-fragile-match"]
    constant.op, constant.args, address.op, address.args
  with
  | Const_int n, [||], Intop Iadd, [| base_arg; offset_arg |] ->
    Nativeint.equal n (Nativeint.of_int offset)
    && is_result base base_arg
    && is_result constant offset_arg
    && Array.length constant.typ = 1
    && Cmm.equal_machtype_component constant.typ.(0) Int
    && Array.length address.typ = 1
    && Cmm.equal_machtype_component address.typ.(0) Addr
  | _ -> false

(* Collect the stores and address resets that fully initialize [allocation],
   scanning [body] from [instr_index + 1]. Skip independent pure operations and
   debugger markers; return [None] for unsupported or incomplete sequences. *)
let recognize (body : finished Instruction.t array) ~instr_index
    ~(allocation : finished Instruction.op_data) ~bytes : t option =
  let members = Instruction.Id.Tbl.create 8 in
  Instruction.Id.Tbl.replace members allocation.id ();
  let uses_member (value : finished Value.t) =
    match value with
    | Res (op, _) -> Option.is_some (Instruction.Id.Tbl.find_opt members op.id)
    | Block_param _ -> false
  in
  let rec scan i ~base ~offset ~remaining ~initialization =
    if remaining = 0
    then Some { allocation; initialization = List.rev initialization }
    else if i >= Array.length body
    then None
    else
      match body.(i) with
      | Push_trap _ | Pop_trap _ -> None
      | Op instr -> (
        let addressing =
          Arch.offset_addressing Arch.identity_addressing offset
        in
        match
          initializing_store_size ~base ~addressing
            ~is_header:(remaining = bytes) instr
        with
        | Some size when size <= remaining ->
          (* Self-referential initialization is not a canonical construction:
             only the address operand may refer to the construction. *)
          let stores_member =
            match instr.args with
            | [| value; _address |] -> uses_member value
            | _ -> false
          in
          if stores_member
          then None
          else
            advance (i + 1) ~base ~offset:(offset + size)
              ~remaining:(remaining - size)
              ~initialization:(instr :: initialization)
        | Some _ -> None
        | None -> (
          match[@warning "-fragile-match"] instr.op with
          | Name_for_debugger _ ->
            scan (i + 1) ~base ~offset ~remaining ~initialization
          | _
            when Operation.is_pure instr.op
                 && not (Array.exists uses_member instr.args) ->
            scan (i + 1) ~base ~offset ~remaining ~initialization
          | _ -> None))
  and advance i ~base ~offset ~remaining ~initialization =
    if remaining = 0
    then Some { allocation; initialization = List.rev initialization }
    else
      (* [emit_stores] resets the base with a constant and an addition when the
         next offset no longer fits the target's addressing mode. *)
      match Cfg_selection.is_offset_out_of_range offset with
      | Within_range -> scan i ~base ~offset ~remaining ~initialization
      | Out_of_range -> (
        if i + 1 >= Array.length body
        then None
        else
          match body.(i), body.(i + 1) with
          | Op constant, Op address
            when is_address_reset ~base ~offset constant address ->
            Instruction.Id.Tbl.replace members constant.id ();
            Instruction.Id.Tbl.replace members address.id ();
            scan (i + 2) ~base:address ~offset:0 ~remaining
              ~initialization:(address :: constant :: initialization)
          | Op _, Op _
          | (Push_trap _ | Pop_trap _), _
          | _, (Push_trap _ | Pop_trap _) ->
            None)
  in
  scan (instr_index + 1) ~base:allocation ~offset:(-Arch.size_int)
    ~remaining:bytes ~initialization:[]

let find_in_block (block : finished Block.t) : t list =
  let body = Block.body block in
  let constructions = ref [] in
  Array.iteri
    (fun instr_index (instr : finished Instruction.t) ->
      match[@warning "-fragile-match"] instr with
      (* [bytes] includes the header, so require at least one word. Otherwise
         [scan] would accept a zero-sized allocation without any stores. *)
      | Op ({ op = Alloc { mode = Heap; bytes; _ }; _ } as allocation)
        when bytes >= Arch.size_int && bytes mod Arch.size_addr = 0 ->
        recognize body ~instr_index ~allocation ~bytes
        |> Option.iter (fun construction ->
            constructions := construction :: !constructions)
      (* CR-soon xclerc for xclerc: Extend construction recognition to local
         allocations. *)
      | _ -> ())
    body;
  List.rev !constructions
