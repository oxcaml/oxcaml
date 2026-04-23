[@@@ocaml.warning "+a-40-41-42"]

module B = Ssa.Builder

module type S = sig
  val emit_instruction :
    Ssa.instruction ->
    [> `Unchanged | `Replaced | `ReplacedWith of Ssa.instruction]

  val emit_terminator :
    dbg:Debuginfo.t -> Ssa.terminator -> [> `Unchanged | `Replaced]
end

module type Assembler = sig
  val emit_instruction : Ssa.instruction -> unit

  val emit_terminator : dbg:Debuginfo.t -> Ssa.terminator -> unit

  val new_block : ?params:Cmm.machtype -> Ssa.block_desc -> Ssa.block

  val change_block : Ssa.block -> unit

  val current_block : unit -> Ssa.block
end

module type Reducer = functor (_ : Assembler) -> S

(* Glue a list of reducers into a single reducer. Each child sees the same
   Assembler, so emissions a child makes re-enter the combined reducer at the
   top. [emit_instruction] and [emit_terminator] try each child in order,
   returning the first non-[`Unchanged] result (or [`Unchanged] if all children
   are). *)
module Combine
    (R : sig
      val reducers : (module Reducer) list
    end)
    (A : Assembler) : S = struct
  let reducers : (module S) list =
    List.map
      (fun m ->
        let module Red = (val m : Reducer) in
        let module Inst = Red (A) in
        (module Inst : S))
      R.reducers

  let emit_instruction i =
    let rec loop = function
      | [] -> `Unchanged
      | (module Red : S) :: rest -> (
        match Red.emit_instruction i with
        | `Unchanged -> loop rest
        | `Replaced -> `Replaced
        | `ReplacedWith i' -> `ReplacedWith i')
    in
    loop reducers

  let emit_terminator ~dbg t =
    let rec loop = function
      | [] -> `Unchanged
      | (module Red : S) :: rest -> (
        match Red.emit_terminator ~dbg t with
        | `Unchanged -> loop rest
        | `Replaced -> `Replaced)
    in
    loop reducers
end

(* Run a single reducer over [ssa]. A reducer returning [`Unchanged] lets [Copy]
   emit the instruction as-is; [`Replaced] means the reducer already emitted via
   the Assembler; [`ReplacedWith i] re-runs the reducer on [i]. Terminators
   behave the same way, minus the [`ReplacedWith] case. The Assembler handed to
   the reducer re-enters the same reducer, so emissions originating from the
   reducer are themselves subject to reduction. To run several reducers at once,
   use [Combine] to glue them into a single reducer. *)
let run (module Red : Reducer) (ssa : Ssa.t) : Ssa.t =
  (* Builder handle for each new block we can emit into. Populated in step 1
     with the structurally-copied blocks, and extended by [Assembler.new_block]
     whenever a reducer creates a fresh block. *)
  let block_to_builder : B.t Ssa.Block.Tbl.t = Ssa.Block.Tbl.create 64 in
  (* Map from old block to its structurally-copied counterpart. *)
  let old_to_new : Ssa.block Ssa.Block.Tbl.t = Ssa.Block.Tbl.create 64 in
  let op_map : Ssa.instruction Ssa.InstructionId.Tbl.t =
    Ssa.InstructionId.Tbl.create 256
  in
  (* Step 1: create a fresh builder for each block, in list order (the builder
     guarantees this is a topological order modulo back edges, and only
     emits reachable blocks). *)
  let entry_builder = B.make ssa.entry.params in
  let entry_new = B.current_block entry_builder in
  Ssa.set_label_hint entry_new ssa.entry.label_hint;
  Ssa.Block.Tbl.replace block_to_builder entry_new entry_builder;
  Ssa.Block.Tbl.replace old_to_new ssa.entry entry_new;
  List.iter
    (fun (blk : Ssa.block) ->
      if not (Ssa.block_equal blk ssa.entry)
      then (
        (match[@warning "-fragile-match"] blk.desc with
        | Function_start ->
          Misc.fatal_errorf "Ssa_reducer.Copy: multiple Function_start"
        | Merge | Branch_target | Call_continuation | Trap_handler -> ());
        let new_builder =
          B.new_block entry_builder ~params:blk.params blk.desc
        in
        let new_blk = B.current_block new_builder in
        (* Carry the [label_hint] forward so the downstream [cfg_of_ssa] reuses
           the aligned labels. *)
        Ssa.set_label_hint new_blk blk.label_hint;
        Ssa.Block.Tbl.replace block_to_builder new_blk new_builder;
        Ssa.Block.Tbl.replace old_to_new blk new_blk))
    ssa.blocks;
  let map_block (old : Ssa.block) : Ssa.block =
    Ssa.Block.Tbl.find old_to_new old
  in
  (* Look up a handler reference for [Push_trap]/[Pop_trap]. If the old handler
     block was dropped (e.g., an unreachable trap handler that [keep] excluded),
     the reference becomes [None], to be resolved at CFG conversion time as the
     shared invalid handler. *)
  let map_handler (old : Ssa.block option) : Ssa.block option =
    match old with
    | None -> None
    | Some blk -> Ssa.Block.Tbl.find_opt old_to_new blk
  in
  let rec rewrite_arg (i : Ssa.instruction) : Ssa.instruction =
    match i with
    | Op { id; _ } -> Ssa.InstructionId.Tbl.find op_map id
    | Block_param { block; index; typ } ->
      Block_param { block = map_block block; index; typ }
    | Proj { index; src } -> Proj { index; src = rewrite_arg src }
    | Push_trap _ | Pop_trap _ | Stack_check _ | Name_for_debugger _ ->
      assert false
  in
  let rewrite_args = Array.map rewrite_arg in
  let rewrite_terminator (t : Ssa.terminator) : Ssa.terminator =
    match t with
    | Pending_construction -> Pending_construction
    | Goto { goto; args } ->
      Goto { goto = map_block goto; args = rewrite_args args }
    | Branch { cond; ifso; ifnot } ->
      Branch
        { cond = rewrite_arg cond;
          ifso = map_block ifso;
          ifnot = map_block ifnot
        }
    | Switch (targets, args) ->
      Switch (Array.map map_block targets, rewrite_args args)
    | Return args -> Return (rewrite_args args)
    | Raise (k, args, h) -> Raise (k, rewrite_args args, Option.map map_block h)
    | Tailcall_self { destination; args } ->
      Tailcall_self
        { destination = map_block destination; args = rewrite_args args }
    | Tailcall_func (op, args) -> Tailcall_func (op, rewrite_args args)
    | Call { op; args; continuation; exn_continuation } ->
      Call
        { op;
          args = rewrite_args args;
          continuation = map_block continuation;
          exn_continuation = Option.map map_block exn_continuation
        }
    | Prim { op; args; continuation; exn_continuation } ->
      Prim
        { op;
          args = rewrite_args args;
          continuation = map_block continuation;
          exn_continuation = Option.map map_block exn_continuation
        }
    | Invalid { message; args; continuation } ->
      Invalid
        { message;
          args = rewrite_args args;
          continuation = Option.map map_block continuation
        }
  in
  let current_builder = ref entry_builder in
  (* [Assembler] and [Chain] are mutually recursive: the assembler handed to the
     reducer re-enters the chain, and the chain applies the reducer to the
     assembler. [module rec] ties the knot; the cycle is safe because both
     bodies only capture the other side in closures. *)
  let module M = struct
    module rec Assembler : Assembler = struct
      let emit_instruction i =
        ignore (Chain.emit_instruction i : Ssa.instruction option)

      let emit_terminator ~dbg t = Chain.emit_terminator ~dbg t

      let new_block ?params desc =
        let nb = B.new_block !current_builder ?params desc in
        let blk = B.current_block nb in
        Ssa.Block.Tbl.replace block_to_builder blk nb;
        current_builder := nb;
        blk

      let change_block blk =
        current_builder := Ssa.Block.Tbl.find block_to_builder blk

      let current_block () = B.current_block !current_builder
    end

    and Chain : sig
      (* Returns [Some i] when the caller should remap the original Op id to
         [i]: either [`Unchanged] (and [i] was emitted verbatim) or
         [`ReplacedWith i] (and the reducer already emitted the replacement
         via the Assembler). [`Replaced] returns [None]: the reducer emitted
         a sequence and doesn't nominate a single representative. *)
      val emit_instruction : Ssa.instruction -> Ssa.instruction option

      val emit_terminator : dbg:Debuginfo.t -> Ssa.terminator -> unit
    end = struct
      module R = Red (Assembler)

      let emit_instruction (i : Ssa.instruction) =
        match R.emit_instruction i with
        | `Unchanged ->
          B.emit_instruction !current_builder i;
          Some i
        | `Replaced -> None
        | `ReplacedWith i' -> Some i'

      let emit_terminator ~dbg (t : Ssa.terminator) =
        match R.emit_terminator ~dbg t with
        | `Unchanged -> B.finish_block !current_builder ~dbg t
        | `Replaced -> ()
    end
  end in
  let emit_chain = M.Chain.emit_instruction in
  let emit_terminator_chain = M.Chain.emit_terminator in
  (* Step 2: emit body and terminator for each block. *)
  List.iter
    (fun (blk : Ssa.block) ->
      let new_blk = Ssa.Block.Tbl.find old_to_new blk in
      current_builder := Ssa.Block.Tbl.find block_to_builder new_blk;
      try
        Array.iter
          (fun (i : Ssa.instruction) ->
            let rewritten : Ssa.instruction =
              match i with
              | Op { op; typ; args; dbg; _ } ->
                Ssa.make_op ~op ~typ ~args:(rewrite_args args) ~dbg
              | Push_trap { handler } ->
                Push_trap { handler = map_handler handler }
              | Pop_trap { handler } ->
                Pop_trap { handler = map_handler handler }
              | Stack_check _ -> i
              | Name_for_debugger { ident; provenance; which_parameter; regs }
                ->
                Name_for_debugger
                  { ident;
                    provenance;
                    which_parameter;
                    regs = rewrite_args regs
                  }
              | Block_param _ | Proj _ -> assert false
            in
            match[@warning "-fragile-match"] emit_chain rewritten, i with
            | Some new_i, Op { id; _ } ->
              Ssa.InstructionId.Tbl.replace op_map id new_i
            | (Some _ | None), _ -> ())
          blk.body;
        emit_terminator_chain ~dbg:blk.terminator_dbg
          (rewrite_terminator blk.terminator)
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        Format.eprintf
          "*** Ssa_reducer.Copy error for %s while processing block %a: %s@."
          ssa.fun_name Ssa_print.print_block_id blk (Printexc.to_string exn);
        Format.eprintf "*** Old SSA:@.%a@." Ssa_print.print ssa;
        let finished_so_far = B.finish entry_builder in
        Format.eprintf "*** New SSA so far (%d finished block(s)):@.%a@."
          (List.length finished_so_far)
          (Format.pp_print_list ~pp_sep:(fun _ () -> ()) Ssa_print.print_block)
          finished_so_far;
        Format.eprintf
          "*** Under-construction block (body not yet flushed):\n%a"
          Ssa_print.print_block
          (B.current_block !current_builder);
        Format.pp_print_flush Format.err_formatter ();
        Printexc.raise_with_backtrace exn bt)
    ssa.blocks;
  { blocks = B.finish entry_builder;
    fun_name = ssa.fun_name;
    fun_args = ssa.fun_args;
    fun_args_names = ssa.fun_args_names;
    fun_codegen_options = ssa.fun_codegen_options;
    fun_dbg = ssa.fun_dbg;
    entry = map_block ssa.entry;
    fun_poll = ssa.fun_poll;
    fun_ret_type = ssa.fun_ret_type
  }
