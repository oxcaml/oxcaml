[@@@ocaml.warning "+a-40-41-42"]

(** Framework for SSA-to-SSA transformations.

    A reducer is a functor over a [Context] (input + output graphs and the
    builder ops to translate between them). {!run} drives the framework: it
    allocates a fresh output graph (so input and output have distinct [Block.t]
    / [Instruction.t] types and can't be accidentally mixed), creates an output
    block per input block, then walks the input in the Finished_graph default
    order (which is guaranteed to have dominators first) and translates each
    block, skipping unused operations and block parameters.

    Two layered hooks:
    - [visit_block] / [visit_instruction] / [visit_terminator]: intercept the
      walk over the input. Return [Replaced] to take over (e.g. emit something
      else), or [Unchanged] to let the framework apply its default translation.
    - [rewrite_instruction] / [rewrite_terminator]: intercept emissions into the
      output. Fire on every emission — both the framework's default translation
      and reducer-driven ones go through here.

    Default translation, applied when [visit_*] returns [Unchanged]:
    - Args (op args, block-param uses, terminator args) and blocks are mapped
      from the input to the output via [map_arg] and [map_block].
    - Block params with [usage_count = 0] are dropped from both the block
      parameter list and from [Goto]s.
    - [Push_trap] / [Pop_trap] handlers are replaced by [None] if the handler
      block has become unreachable. CFG lowering routes those through a shared
      invalid handler.
    - Other terminators are reconstructed with their args mapped through.

    [keep_unused_ops] disables the dead-Op skip and the dead-param drop, so the
    output is structurally faithful to the input. Used before [Cfg_compare]. *)

module type Context = sig
  module In : Ssa.Finished_graph

  include Ssa.Graph_builder

  val emit_instruction : cursor -> Instruction.t -> Instruction.t

  val emit_op :
    cursor ->
    op:op ->
    dbg:Debuginfo.t ->
    typ:Cmm.machtype ->
    args:Instruction.t array ->
    Instruction.t

  val map_arg : In.Instruction.t -> Instruction.t

  val map_block : In.Block.t -> Block.t

  val finish : unit
end

type 'a result =
  | Unchanged
  | Replaced of 'a

module type Reducer = functor (C : Context) -> sig
  val analyze : unit -> unit

  val visit_block : C.In.Block.t -> C.cursor -> unit result

  val visit_instruction :
    C.In.Block.t -> instr_index:int -> C.cursor -> unit result

  val visit_terminator : C.In.Block.t -> C.cursor -> unit result

  val rewrite_instruction :
    C.cursor -> C.Instruction.t -> C.Instruction.t result

  val rewrite_terminator :
    C.cursor -> dbg:Debuginfo.t -> C.Terminator.t -> unit result
end

module Default : Reducer =
functor
  (C : Context)
  ->
  struct
    let analyze () = ()

    let visit_block (_ : C.In.Block.t) (_ : C.cursor) = Unchanged

    let visit_instruction (_ : C.In.Block.t) ~instr_index:(_ : int)
        (_ : C.cursor) =
      Unchanged

    let visit_terminator (_ : C.In.Block.t) (_ : C.cursor) = Unchanged

    let rewrite_instruction (_ : C.cursor) (_ : C.Instruction.t) = Unchanged

    let rewrite_terminator (_ : C.cursor) ~dbg:(_ : Debuginfo.t)
        (_ : C.Terminator.t) =
      Unchanged
  end

let combine (rs : (module Reducer) list) : (module Reducer) =
  let module Combined =
  functor
    (C : Context)
    ->
    struct
      module type S = sig
        val analyze : unit -> unit

        val visit_block : C.In.Block.t -> C.cursor -> unit result

        val visit_instruction :
          C.In.Block.t -> instr_index:int -> C.cursor -> unit result

        val visit_terminator : C.In.Block.t -> C.cursor -> unit result

        val rewrite_instruction :
          C.cursor -> C.Instruction.t -> C.Instruction.t result

        val rewrite_terminator :
          C.cursor -> dbg:Debuginfo.t -> C.Terminator.t -> unit result
      end

      let children : (module S) list =
        List.map
          (fun m ->
            let module Red = (val m : Reducer) in
            let module Inst = Red (C) in
            (module Inst : S))
          rs

      let analyze () =
        List.iter (fun (module Red : S) -> Red.analyze ()) children

      let visit_block blk b =
        let rec loop = function
          | [] -> Unchanged
          | (module Red : S) :: rest -> (
            match Red.visit_block blk b with
            | Unchanged -> loop rest
            | Replaced () -> Replaced ())
        in
        loop children

      let visit_instruction blk ~instr_index b =
        let rec loop = function
          | [] -> Unchanged
          | (module Red : S) :: rest -> (
            match Red.visit_instruction blk ~instr_index b with
            | Unchanged -> loop rest
            | Replaced () -> Replaced ())
        in
        loop children

      let visit_terminator blk b =
        let rec loop = function
          | [] -> Unchanged
          | (module Red : S) :: rest -> (
            match Red.visit_terminator blk b with
            | Unchanged -> loop rest
            | Replaced () -> Replaced ())
        in
        loop children

      let rewrite_instruction b i =
        let rec loop = function
          | [] -> Unchanged
          | (module Red : S) :: rest -> (
            match Red.rewrite_instruction b i with
            | Unchanged -> loop rest
            | Replaced i' -> Replaced i')
        in
        loop children

      let rewrite_terminator b ~dbg t =
        let rec loop = function
          | [] -> Unchanged
          | (module Red : S) :: rest -> (
            match Red.rewrite_terminator b ~dbg t with
            | Unchanged -> loop rest
            | Replaced () -> Replaced ())
        in
        loop children
    end in
  (module Combined : Reducer)

let run ?(keep_unused_ops = false) (module Red_ctor : Reducer)
    (input : (module Ssa.Finished_graph)) : (module Ssa.Finished_graph) =
  let module In = (val input : Ssa.Finished_graph) in
  let module Out = (val Ssa.make_builder In.function_info : Ssa.Graph_builder)
  in
  (* Map each input block to its output counterpart. *)
  let block_map : Out.Block.t In.Block.Tbl.t = In.Block.Tbl.create 64 in
  (* For each input block, the array of input param indices kept in the output
     block (in order). See [compute_kept] for the dropping criterion. *)
  let kept_params : int array In.Block.Tbl.t = In.Block.Tbl.create 64 in
  let op_map : Out.Instruction.t In.Instruction_id.Tbl.t =
    In.Instruction_id.Tbl.create 256
  in
  (* A param can be dropped only if every incoming edge passes its arg through a
     [Goto] (the only terminator that has positional per-param args). A non-Goto
     predecessor supplies the param's value through a runtime-fixed mechanism,
     so its position must be preserved — we model this as "every param is used"
     for that target. The function entry is handled separately below: its params
     come from the function ABI and are never droppable. *)
  let compute_kept (blk : In.Block.t) : int array =
    let n = Array.length blk.params in
    let any_non_goto_pred =
      In.Block.Set.exists
        (fun (pred : In.Block.t) ->
          match[@warning "-fragile-match"] pred.terminator with
          | Goto _ -> false
          | _ -> true)
        blk.predecessors
    in
    if any_non_goto_pred || keep_unused_ops
    then Array.init n Fun.id
    else
      let counts = blk.param_usage_counts in
      let buf = ref [] in
      for i = n - 1 downto 0 do
        if counts.(i) > 0 then buf := i :: !buf
      done;
      Array.of_list !buf
  in
  (* Step 1: create an output block for each input block. The entry's params
     come from the function ABI and are kept verbatim; other blocks may drop
     unused params per [compute_kept]. *)
  let entry_out = Out.entry in
  In.Block.Tbl.replace block_map In.entry entry_out;
  In.Block.Tbl.replace kept_params In.entry
    (Array.init (Array.length In.entry.params) Fun.id);
  List.iter
    (fun (blk : In.Block.t) ->
      if not (In.Block.equal blk In.entry)
      then begin
        let kept = compute_kept blk in
        In.Block.Tbl.replace kept_params blk kept;
        let params =
          Array.map (fun i -> (blk.params.(i) : Ssa_intf.block_param).typ) kept
        in
        let { Out.block = new_out; _ } = Out.new_block ~params in
        let out_params = Out.Block.params new_out in
        Array.iteri
          (fun i k ->
            (out_params.(i) : Ssa_intf.block_param).name
              <- (blk.params.(k) : Ssa_intf.block_param).name)
          kept;
        In.Block.Tbl.replace block_map blk new_out
      end)
    In.blocks;
  let map_block_impl (old : In.Block.t) : Out.Block.t =
    In.Block.Tbl.find block_map old
  in
  (* Select from [args] the entries at positions whose target params were kept.
     Filtering before [map_arg] avoids touching args that feed dropped params:
     those args may be defined by dead [Op]s skipped by [visit_instruction], so
     they are absent from [op_map]. *)
  let filter_in_args_for (old : In.Block.t) (args : In.Instruction.t array) :
      In.Instruction.t array =
    let kept = In.Block.Tbl.find kept_params old in
    if Array.length kept = Array.length args
    then args
    else Array.map (fun i -> args.(i)) kept
  in
  (* Old index → new index for [block]'s params (post-filtering). *)
  let remap_param_index (block : In.Block.t) (old_index : int) : int =
    let kept = In.Block.Tbl.find kept_params block in
    if Array.length kept = Array.length block.params
    then old_index
    else
      let found = ref (-1) in
      Array.iteri (fun j k -> if k = old_index then found := j) kept;
      assert (!found >= 0);
      !found
  in
  (* Look up a handler reference for [Push_trap]/[Pop_trap]. If the old handler
     block was dropped (e.g., an unreachable trap handler filtered by
     [Builder.finish]), the reference becomes [None], to be resolved at CFG
     conversion time as the shared invalid handler. *)
  let map_handler (old : In.Block.t option) : Out.Block.t option =
    match old with
    | None -> None
    | Some blk -> In.Block.Tbl.find_opt block_map blk
  in
  let rec map_arg_impl (i : In.Instruction.t) : Out.Instruction.t =
    match i with
    | Op { id; _ } -> In.Instruction_id.Tbl.find op_map id
    | Block_param { block; index } ->
      Out.Instruction.make_block_param (map_block_impl block)
        (remap_param_index block index)
    | Proj { index; src } -> Out.Instruction.make_proj ~index (map_arg_impl src)
    | Tuple _ | Push_trap _ | Pop_trap _ | Stack_check _ | Name_for_debugger _
      ->
      Misc.fatal_error "Unexpected instruction in Ssa_reducer map_arg_impl"
  in
  let map_args (args : In.Instruction.t array) : Out.Instruction.t array =
    Array.map map_arg_impl args
  in
  let rewrite_terminator_impl (t : In.Terminator.t) : Out.Terminator.t =
    match t with
    | Goto { goto; args } ->
      let mapped_args = Array.map map_arg_impl (filter_in_args_for goto args) in
      Goto { goto = map_block_impl goto; args = mapped_args }
    | Branch { cond; ifso; ifnot } ->
      Branch
        { cond = map_arg_impl cond;
          ifso = map_block_impl ifso;
          ifnot = map_block_impl ifnot
        }
    | Switch { index; targets } ->
      Switch
        { index = map_arg_impl index;
          targets = Array.map map_block_impl targets
        }
    | Return { args } -> Return { args = map_args args }
    | Raise { raise_kind; args } -> Raise { raise_kind; args = map_args args }
    | Tailcall_self { destination; args } ->
      let destination = map_block_impl destination in
      assert (Array.length args = Array.length (Out.Block.params destination));
      Tailcall_self { destination; args = map_args args }
    | Tailcall_func { op; args } -> Tailcall_func { op; args = map_args args }
    | Call { op; args; continuation; may_raise; nontail } ->
      Call
        { op;
          args = map_args args;
          continuation = map_block_impl continuation;
          may_raise;
          nontail
        }
    | Invalid { message; args; continuation } ->
      Invalid
        { message;
          args = map_args args;
          continuation = Option.map map_block_impl continuation
        }
  in
  let module M = struct
    module rec Ctx : sig
      include Context with module In = In and type cursor = Out.cursor

      val visit_instruction : In.Block.t -> instr_index:int -> cursor -> unit

      val visit_terminator : In.Block.t -> cursor -> unit
    end = struct
      module In = In
      include Out

      let emit_instruction c i =
        match Red.rewrite_instruction c i with
        | Unchanged ->
          Out.emit_instruction c i;
          i
        | Replaced i' -> i'

      let emit_op c ~op ~dbg ~typ ~args =
        emit_instruction c (Out.Instruction.make_op ~op ~typ ~args ~dbg)

      let finish_block c ~dbg term =
        match Red.rewrite_terminator c ~dbg term with
        | Unchanged -> Out.finish_block c ~dbg term
        | Replaced () -> ()

      let map_arg = map_arg_impl

      let map_block = map_block_impl

      let visit_instruction (blk : In.Block.t) ~instr_index b =
        match Red.visit_instruction blk ~instr_index b with
        | Replaced () -> ()
        | Unchanged -> (
          let i = Array.get blk.body instr_index in
          match[@warning "-fragile-match"] i with
          (* Skip dead Ops: their args may reference [Block_param]s that we've
             dropped (see [compute_kept]), and emitting them would propagate
             those dangling references into the new graph. An Op with
             [usage_count = 0] is by definition not referenced by any live
             consumer. [keep_unused_ops] disables this filtering so the output
             is structurally faithful to the input — used before [Cfg_compare]
             so it can match the baseline pipeline. *)
          | Op { usage_count = 0; _ } when not keep_unused_ops -> ()
          | _ -> (
            let rewritten : Out.Instruction.t =
              match i with
              | Op { op; typ; args; dbg; name; _ } ->
                let new_op =
                  Out.Instruction.make_op ~op ~typ ~args:(map_args args) ~dbg
                in
                Option.iter (Out.Instruction.set_name new_op) name;
                new_op
              | Push_trap { handler } ->
                Push_trap { handler = map_handler handler }
              | Pop_trap { handler } ->
                Pop_trap { handler = map_handler handler }
              | Stack_check { max_frame_size_bytes } ->
                Stack_check { max_frame_size_bytes }
              | Name_for_debugger { ident; provenance; which_parameter; regs }
                ->
                Name_for_debugger
                  { ident; provenance; which_parameter; regs = map_args regs }
              | Block_param _ | Proj _ | Tuple _ -> Misc.fatal_error "reducer found imp"
            in
            let new_i = emit_instruction b rewritten in
            match[@warning "-fragile-match"] i with
            | Op { id; _ } -> In.Instruction_id.Tbl.replace op_map id new_i
            | _ -> ()))

      let visit_terminator (blk : In.Block.t) b =
        match Red.visit_terminator blk b with
        | Replaced () -> ()
        | Unchanged ->
          let term = rewrite_terminator_impl blk.terminator in
          finish_block b ~dbg:blk.terminator_dbg term

      let finish = ()
    end

    and Red : sig
      val analyze : unit -> unit

      val visit_block : In.Block.t -> Out.cursor -> unit result

      val visit_instruction :
        In.Block.t -> instr_index:int -> Out.cursor -> unit result

      val visit_terminator : In.Block.t -> Out.cursor -> unit result

      val rewrite_instruction :
        Out.cursor -> Out.Instruction.t -> Out.Instruction.t result

      val rewrite_terminator :
        Out.cursor -> dbg:Debuginfo.t -> Out.Terminator.t -> unit result
    end =
      Red_ctor (Ctx)
  end in
  let module Red = M.Red in
  let module Ctx = M.Ctx in
  Red.analyze ();
  (* Step 2: walk the input graph in [In.blocks] order, which already guarantees
     each block's dominators come before it. *)
  List.iter
    (fun (blk : In.Block.t) ->
      let out_blk = In.Block.Tbl.find block_map blk in
      let c = Out.start_block out_blk in
      try
        match Red.visit_block blk c with
        | Replaced () -> ()
        | Unchanged ->
          Array.iteri
            (fun instr_index _ -> Ctx.visit_instruction blk ~instr_index c)
            blk.body;
          Ctx.visit_terminator blk c
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        let module In_print = Ssa_print.Make (In) in
        Format.eprintf
          "*** Ssa_reducer.run error for %s while processing block %a: %s@.*** \
           Input SSA:@.%a@."
          In.function_info.fun_name In_print.print_block_id blk
          (Printexc.to_string exn) Ssa_print.print
          (module In : Ssa.Finished_graph);
        Format.pp_print_flush Format.err_formatter ();
        Printexc.raise_with_backtrace exn bt)
    In.blocks;
  let result = Out.finish () in
  Ssa_validate.validate result;
  result
