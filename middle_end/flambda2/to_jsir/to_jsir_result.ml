(** Blocks with the continuation potentially not yet defined.

    For efficiency reasons, [body] will be inserted head-first,
    so the instructions are in reverse. [archive_block] will
    therefore reverse this before archiving. *)
type partial_block =
  { params : Jsir.Var.t list;
    body : Jsir.instr list;
    addr : Jsir.Addr.t
  }

type t =
  { archived_blocks : Jsir.block Jsir.Addr.Map.t;
    current_blocks : partial_block list;
    next_addr : Jsir.Addr.t;
    reserved_addrs : Jsir.Addr.Set.t
  }

let create () =
  { archived_blocks = Jsir.Addr.Map.empty;
    current_blocks = [];
    next_addr = Jsir.Addr.zero;
    reserved_addrs = Jsir.Addr.Set.empty
  }

let add_instr_exn t instr =
  let top_current_block, rest_current_blocks =
    match t.current_blocks with
    | [] ->
      Misc.fatal_error
        "To_jsir_result.add_instr_exn: expected nonempty current_blocks"
    | hd :: tl -> hd, tl
  in
  let top_current_block =
    { top_current_block with body = instr :: top_current_block.body }
  in
  { t with current_blocks = top_current_block :: rest_current_blocks }

let new_block { archived_blocks; current_blocks; next_addr; reserved_addrs }
    ~params =
  let new_block = { params; body = []; addr = next_addr } in
  ( { archived_blocks;
      current_blocks = new_block :: current_blocks;
      next_addr = Jsir.Addr.succ next_addr;
      reserved_addrs
    },
    next_addr )

let reserve_address
    { archived_blocks; current_blocks; next_addr; reserved_addrs } =
  ( { archived_blocks;
      current_blocks;
      next_addr = Jsir.Addr.succ next_addr;
      reserved_addrs = Jsir.Addr.Set.add next_addr reserved_addrs
    },
    next_addr )

let new_block_with_addr_exn
    { archived_blocks; current_blocks; next_addr; reserved_addrs } ~params ~addr
    =
  if not (Jsir.Addr.Set.mem addr reserved_addrs)
  then
    Misc.fatal_errorf
      "To_jsir_result.new_block_with_addr_exn: expected provided address %d to \
       be reserved"
      addr;
  let new_block = { params; body = []; addr } in
  { archived_blocks;
    current_blocks = new_block :: current_blocks;
    next_addr;
    reserved_addrs = Jsir.Addr.Set.remove addr reserved_addrs
  }

let end_block_with_last_exn
    { archived_blocks; current_blocks; next_addr; reserved_addrs } last =
  let { params; body; addr }, rest_current_blocks =
    match current_blocks with
    | [] ->
      Misc.fatal_error
        "To_jsir_result.end_block_with_last_exn: expected nonempty \
         current_blocks"
    | hd :: tl -> hd, tl
  in
  let new_block : Jsir.block =
    { params; body = List.rev body; branch = last }
  in
  let archived_blocks = Jsir.Addr.Map.add addr new_block archived_blocks in
  { archived_blocks;
    current_blocks = rest_current_blocks;
    next_addr;
    reserved_addrs
  }

let to_program_exn
    { archived_blocks; current_blocks; next_addr = _; reserved_addrs } =
  if List.length current_blocks <> 0
  then
    Misc.fatal_error
      "To_jsir_result.to_program_exn: expected current_blocks to be empty";
  if not (Jsir.Addr.Set.is_empty reserved_addrs)
  then
    Misc.fatal_error
      "To_jsir_result.to_program_exn: expected all reserved addresses to be \
       used";
  let free_pc = (Jsir.Addr.Map.max_binding archived_blocks |> fst) + 1 in
  { Jsir.start = Jsir.Addr.zero; blocks = archived_blocks; free_pc }
