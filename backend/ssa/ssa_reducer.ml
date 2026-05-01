[@@@ocaml.warning "+a-40-41-42"]

module type Context = sig
  module In : Ssa.Finished_graph

  module Out : Ssa.Graph_builder

  val emit_instruction :
    Out.unfinished_block ->
    Out.Instruction.t ->
    Out.unfinished_block * Out.Instruction.t

  val emit_op :
    Out.unfinished_block ->
    op:Out.op ->
    dbg:Debuginfo.t ->
    typ:Cmm.machtype ->
    args:Out.Instruction.t array ->
    Out.unfinished_block * Out.Instruction.t

  val finish_block :
    Out.unfinished_block -> dbg:Debuginfo.t -> Out.Terminator.t -> unit

  val new_block : params:Cmm.machtype -> Out.new_block_result

  val map_arg : In.Instruction.t -> Out.Instruction.t

  val map_block : In.Block.t -> Out.Block.t

  val inline_block :
    In.Block.t ->
    block_args:Out.Instruction.t array ->
    Out.unfinished_block ->
    unit

  val inline_instruction :
    In.Block.t ->
    instr_index:int ->
    Out.unfinished_block ->
    Out.unfinished_block

  val inline_terminator : In.Block.t -> Out.unfinished_block -> unit
end

module type Reducer = functor (C : Context) -> sig
  val analyze : unit -> unit

  val visit_block :
    C.In.Block.t -> C.Out.unfinished_block -> [> `Unchanged | `Replaced]

  val visit_instruction :
    C.In.Block.t ->
    instr_index:int ->
    C.Out.unfinished_block ->
    [> `Unchanged | `Replaced]

  val visit_terminator :
    C.In.Block.t -> C.Out.unfinished_block -> [> `Unchanged | `Replaced]

  val rewrite_instruction :
    C.Out.unfinished_block ->
    C.Out.Instruction.t ->
    [> `Unchanged | `Replaced of C.Out.unfinished_block * C.Out.Instruction.t]

  val rewrite_terminator :
    C.Out.unfinished_block ->
    dbg:Debuginfo.t ->
    C.Out.Terminator.t ->
    [> `Unchanged | `Replaced]
end

module Default : Reducer =
functor
  (C : Context)
  ->
  struct
    let analyze () = ()

    let visit_block (_ : C.In.Block.t) (_ : C.Out.unfinished_block) = `Unchanged

    let visit_instruction (_ : C.In.Block.t) ~instr_index:(_ : int)
        (_ : C.Out.unfinished_block) =
      `Unchanged

    let visit_terminator (_ : C.In.Block.t) (_ : C.Out.unfinished_block) =
      `Unchanged

    let rewrite_instruction (_ : C.Out.unfinished_block)
        (_ : C.Out.Instruction.t) =
      `Unchanged

    let rewrite_terminator (_ : C.Out.unfinished_block) ~dbg:(_ : Debuginfo.t)
        (_ : C.Out.Terminator.t) =
      `Unchanged
  end

let combine (rs : (module Reducer) list) : (module Reducer) =
  let module Combined =
  functor
    (C : Context)
    ->
    struct
      module type S = sig
        val analyze : unit -> unit

        val visit_block :
          C.In.Block.t -> C.Out.unfinished_block -> [`Unchanged | `Replaced]

        val visit_instruction :
          C.In.Block.t ->
          instr_index:int ->
          C.Out.unfinished_block ->
          [`Unchanged | `Replaced]

        val visit_terminator :
          C.In.Block.t -> C.Out.unfinished_block -> [`Unchanged | `Replaced]

        val rewrite_instruction :
          C.Out.unfinished_block ->
          C.Out.Instruction.t ->
          [ `Unchanged
          | `Replaced of C.Out.unfinished_block * C.Out.Instruction.t ]

        val rewrite_terminator :
          C.Out.unfinished_block ->
          dbg:Debuginfo.t ->
          C.Out.Terminator.t ->
          [`Unchanged | `Replaced]
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
          | [] -> `Unchanged
          | (module Red : S) :: rest -> (
            match Red.visit_block blk b with
            | `Unchanged -> loop rest
            | `Replaced -> `Replaced)
        in
        loop children

      let visit_instruction blk ~instr_index b =
        let rec loop = function
          | [] -> `Unchanged
          | (module Red : S) :: rest -> (
            match Red.visit_instruction blk ~instr_index b with
            | `Unchanged -> loop rest
            | `Replaced -> `Replaced)
        in
        loop children

      let visit_terminator blk b =
        let rec loop = function
          | [] -> `Unchanged
          | (module Red : S) :: rest -> (
            match Red.visit_terminator blk b with
            | `Unchanged -> loop rest
            | `Replaced -> `Replaced)
        in
        loop children

      let rewrite_instruction b i =
        let rec loop = function
          | [] -> `Unchanged
          | (module Red : S) :: rest -> (
            match Red.rewrite_instruction b i with
            | `Unchanged -> loop rest
            | `Replaced (b', i') -> `Replaced (b', i'))
        in
        loop children

      let rewrite_terminator b ~dbg t =
        let rec loop = function
          | [] -> `Unchanged
          | (module Red : S) :: rest -> (
            match Red.rewrite_terminator b ~dbg t with
            | `Unchanged -> loop rest
            | `Replaced -> `Replaced)
        in
        loop children
    end in
  (module Combined : Reducer)

let run (module Red_ctor : Reducer) (input : (module Ssa.Finished_graph)) :
    (module Ssa.Finished_graph) =
  let module In = (val input : Ssa.Finished_graph) in
  let module Out =
    (val Ssa.make_builder In.function_info : Ssa.Standalone_graph_builder)
  in
  (* Map each input block to its output counterpart. *)
  let block_map : Out.Block.t In.Block.Tbl.t = In.Block.Tbl.create 64 in
  (* Global [Block_param] substitution table, populated by [visit_block]: for a
     block [blk] in this table, references to [Block_param { block = blk; index
     = i; _ }] resolve through [map_arg] to [subst.(i)]. *)
  let block_param_subst : Out.Instruction.t array In.Block.Tbl.t =
    In.Block.Tbl.create 16
  in
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
      In.Block_set.exists
        (fun (pred : In.Block.t) ->
          match[@warning "-fragile-match"] pred.terminator with
          | Goto _ -> false
          | _ -> true)
        blk.predecessors
    in
    if any_non_goto_pred
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
  Out.Block.set_label_hint entry_out In.entry.label_hint;
  In.Block.Tbl.replace block_map In.entry entry_out;
  In.Block.Tbl.replace kept_params In.entry
    (Array.init (Array.length In.entry.params) Fun.id);
  List.iter
    (fun (blk : In.Block.t) ->
      if not (In.Block.equal blk In.entry)
      then begin
        let kept = compute_kept blk in
        In.Block.Tbl.replace kept_params blk kept;
        let params = Array.map (fun i -> blk.params.(i)) kept in
        let { Out.block = new_out; _ } = Out.new_block ~params in
        (* Carry the [label_hint] forward so the downstream [cfg_of_ssa] reuses
           the aligned labels. *)
        Out.Block.set_label_hint new_out blk.label_hint;
        In.Block.Tbl.replace block_map blk new_out
      end)
    In.blocks;
  let map_block_impl (old : In.Block.t) : Out.Block.t =
    In.Block.Tbl.find block_map old
  in
  (* Select from [args] the entries at positions whose target params were
     kept. *)
  let filter_args_for (old : In.Block.t) (args : Out.Instruction.t array) :
      Out.Instruction.t array =
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
    | Block_param { block; index } -> (
      match In.Block.Tbl.find_opt block_param_subst block with
      | Some subst -> subst.(index)
      | None ->
        Out.Instruction.make_block_param (map_block_impl block)
          (remap_param_index block index))
    | Proj { index; src } -> Out.Instruction.make_proj ~index (map_arg_impl src)
    | Tuple _ | Push_trap _ | Pop_trap _ | Stack_check _ | Name_for_debugger _
      ->
      assert false
  in
  let map_args (args : In.Instruction.t array) : Out.Instruction.t array =
    Array.map map_arg_impl args
  in
  let rewrite_terminator_impl (t : In.Terminator.t) : Out.Terminator.t =
    match t with
    | Goto { goto; args } ->
      let mapped_args = filter_args_for goto (Array.map map_arg_impl args) in
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
    | Raise { raise_kind; args; handler } ->
      let handler = Option.map map_block_impl handler in
      Option.iter
        (fun h ->
          assert (Array.length args = Array.length (Out.Block.params h)))
        handler;
      Raise { raise_kind; args = map_args args; handler }
    | Tailcall_self { destination; args } ->
      let destination = map_block_impl destination in
      assert (Array.length args = Array.length (Out.Block.params destination));
      Tailcall_self { destination; args = map_args args }
    | Tailcall_func { op; args } -> Tailcall_func { op; args = map_args args }
    | Call { op; args; continuation; exn_continuation } ->
      Call
        { op;
          args = map_args args;
          continuation = map_block_impl continuation;
          exn_continuation = Option.map map_block_impl exn_continuation
        }
    | Invalid { message; args; continuation } ->
      Invalid
        { message;
          args = map_args args;
          continuation = Option.map map_block_impl continuation
        }
  in
  (* [Ctx] and [Red] are mutually recursive: [Ctx]'s emissions route through
     [Red]'s rewrite hooks, and [Red]'s visit hooks default to [Ctx]'s
     translation helpers. [Ctx]'s [inline_*] family (exposed via [Context]) is
     used by reducers to splice another block's content into the current
     builder; the matching [visit_*] helpers are the same translation logic
     exposed via [Ctx]'s richer signature so the main loop can drive them
     directly. *)
  let module M = struct
    module rec Ctx : sig
      include Context with module In = In and module Out = Out

      val visit_instruction :
        In.Block.t ->
        instr_index:int ->
        Out.unfinished_block ->
        Out.unfinished_block

      val visit_terminator : In.Block.t -> Out.unfinished_block -> unit
    end = struct
      module In = In
      module Out = Out

      let emit_instruction b i =
        match Red.rewrite_instruction b i with
        | `Unchanged -> Out.emit_instruction b i
        | `Replaced (b', i') -> b', i'

      let emit_op b ~op ~dbg ~typ ~args =
        emit_instruction b (Out.Instruction.make_op ~op ~typ ~args ~dbg)

      let finish_block b ~dbg term =
        match Red.rewrite_terminator b ~dbg term with
        | `Unchanged -> Out.finish_block b ~dbg term
        | `Replaced -> ()

      let new_block = Out.new_block

      let map_arg = map_arg_impl

      let map_block = map_block_impl

      let visit_instruction (blk : In.Block.t) ~instr_index b =
        match Red.visit_instruction blk ~instr_index b with
        | `Replaced -> b
        | `Unchanged -> (
          let i = Array.get blk.body instr_index in
          match[@warning "-fragile-match"] i with
          (* Skip dead Ops: their args may reference [Block_param]s that we've
             dropped (see [compute_kept]), and emitting them would propagate
             those dangling references into the new graph. An Op with
             [usage_count = 0] is by definition not referenced by any live
             consumer. *)
          | Op { usage_count = 0; _ } -> b
          | _ ->
            let rewritten : Out.Instruction.t =
              match i with
              | Op { op; typ; args; dbg; _ } ->
                Out.Instruction.make_op ~op ~typ ~args:(map_args args) ~dbg
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
              | Block_param _ | Proj _ | Tuple _ -> assert false
            in
            let b, new_i = emit_instruction b rewritten in
            (match[@warning "-fragile-match"] i with
            | Op { id; _ } -> In.Instruction_id.Tbl.replace op_map id new_i
            | _ -> ());
            b)

      let visit_terminator (blk : In.Block.t) b =
        match Red.visit_terminator blk b with
        | `Replaced -> ()
        | `Unchanged ->
          let term = rewrite_terminator_impl blk.terminator in
          finish_block b ~dbg:blk.terminator_dbg term

      let inline_instruction = visit_instruction

      let inline_terminator = visit_terminator

      let inline_block (blk : In.Block.t) ~block_args b =
        match Red.visit_block blk b with
        | `Replaced -> ()
        | `Unchanged ->
          In.Block.Tbl.replace block_param_subst blk block_args;
          let b = ref b in
          Array.iteri
            (fun instr_index _ -> b := inline_instruction blk ~instr_index !b)
            blk.body;
          inline_terminator blk !b
    end

    and Red : sig
      val analyze : unit -> unit

      val visit_block :
        In.Block.t -> Out.unfinished_block -> [`Unchanged | `Replaced]

      val visit_instruction :
        In.Block.t ->
        instr_index:int ->
        Out.unfinished_block ->
        [`Unchanged | `Replaced]

      val visit_terminator :
        In.Block.t -> Out.unfinished_block -> [`Unchanged | `Replaced]

      val rewrite_instruction :
        Out.unfinished_block ->
        Out.Instruction.t ->
        [`Unchanged | `Replaced of Out.unfinished_block * Out.Instruction.t]

      val rewrite_terminator :
        Out.unfinished_block ->
        dbg:Debuginfo.t ->
        Out.Terminator.t ->
        [`Unchanged | `Replaced]
    end =
      Red_ctor (Ctx)
  end in
  let module Red = M.Red in
  let module Ctx = M.Ctx in
  Red.analyze ();
  (* Step 2: walk the input graph in [In.blocks] order, which already
     guarantees each block's dominators come before it. *)
  List.iter
    (fun (blk : In.Block.t) ->
      let out_blk = In.Block.Tbl.find block_map blk in
      let b = Out.start_block out_blk in
      try
        match Red.visit_block blk b with
        | `Replaced -> ()
        | `Unchanged ->
          let b = ref b in
          Array.iteri
            (fun instr_index _ ->
              b := Ctx.visit_instruction blk ~instr_index !b)
            blk.body;
          Ctx.visit_terminator blk !b
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        let module InPrint = Ssa_print.Make (In) in
        Format.eprintf
          "*** Ssa_reducer.run error for %s while processing block %a: %s@."
          In.function_info.fun_name InPrint.print_block_id blk
          (Printexc.to_string exn);
        Printexc.raise_with_backtrace exn bt)
    In.blocks;
  let result = Out.finish () in
  (try Ssa_validate.validate result
   with exn ->
     let bt = Printexc.get_raw_backtrace () in
     Format.eprintf "*** Ssa_reducer.run: validation failed for %s: %s@."
       In.function_info.fun_name (Printexc.to_string exn);
     Format.pp_print_flush Format.err_formatter ();
     Printexc.raise_with_backtrace exn bt);
  result
