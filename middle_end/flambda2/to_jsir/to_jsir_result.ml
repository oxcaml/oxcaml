(** Blocks with the continuation potentially not yet defined.

    For efficiency reasons, [body] will be inserted head-first,
    so the instructions are in reverse. [archive_block] will
    therefore reverse this before archiving. *)
type partial_block =
  { params : Jsir.Var.t list;
    body : Jsir.instr list;
    branch : Jsir.last option
  }

type t =
  { archived_blocks : Jsir.block Jsir.Addr.Map.t;
    current_block : partial_block;
    current_block_addr : Jsir.Addr.t
  }

let create () =
  let current_block = { params = []; body = []; branch = None } in
  { archived_blocks = Jsir.Addr.Map.empty;
    current_block;
    current_block_addr = Jsir.Addr.zero
  }

let add_instr t instr =
  let current_block =
    { t.current_block with body = instr :: t.current_block.body }
  in
  { t with current_block }

let set_branch t last =
  let current_block = { t.current_block with branch = Some last } in
  { t with current_block }

let archive_block_exn ~archived_blocks ~current_block ~current_block_addr =
  let branch =
    match current_block.branch with
    | None ->
      Misc.fatal_errorf
        "To_jsir_result: tried to archive a block with an unknown branch"
    | Some b -> b
  in
  (* Reverse the list of instructions, since they were inserted head-first *)
  (* CR selee: check *)
  let[@warning "-18"] (current_block : Jsir.block) =
    { params = current_block.params;
      body = List.rev current_block.body;
      branch
    }
  in
  Jsir.Addr.Map.add current_block_addr current_block archived_blocks

let new_block_exn { archived_blocks; current_block; current_block_addr } params
    =
  (* CR selee: review PC *)
  let archived_blocks =
    archive_block_exn ~archived_blocks ~current_block ~current_block_addr
  in
  let current_block_addr =
    current_block_addr + List.length current_block.body
  in
  let current_block = { params; body = []; branch = None } in
  { archived_blocks; current_block_addr; current_block }

let to_program_exn { archived_blocks; current_block; current_block_addr } =
  let archived_blocks =
    archive_block_exn ~archived_blocks ~current_block ~current_block_addr
  in
  let free_pc = (Jsir.Addr.Map.max_binding archived_blocks |> fst) + 1 in
  { Jsir.start = Jsir.Addr.zero; blocks = archived_blocks; free_pc }
