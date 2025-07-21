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
    next_addr : Jsir.Addr.t
  }

let create () =
  { archived_blocks = Jsir.Addr.Map.empty;
    current_blocks = [];
    next_addr = Jsir.Addr.zero
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

let new_block { archived_blocks; current_blocks; next_addr } ~params =
  (* CR selee: review PC *)
  let new_block = { params; body = []; addr = next_addr } in
  ( { archived_blocks;
      current_blocks = new_block :: current_blocks;
      next_addr = Jsir.Addr.succ next_addr
    },
    next_addr )

let end_block_with_last_exn { archived_blocks; current_blocks; next_addr } last
    =
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
  { archived_blocks; current_blocks = rest_current_blocks; next_addr }

let to_program_exn { archived_blocks; current_blocks; next_addr = _ } =
  if List.length current_blocks <> 0
  then
    Misc.fatal_error
      "To_jsir_result.to_program_exn: expected current_blocks to be empty";
  let free_pc = (Jsir.Addr.Map.max_binding archived_blocks |> fst) + 1 in
  { Jsir.start = Jsir.Addr.zero; blocks = archived_blocks; free_pc }
