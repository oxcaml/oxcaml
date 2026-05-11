open! Int_replace_polymorphic_compare

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
    - [Push_trap] / [Pop_trap] whose handler block has become unreachable are
      dropped entirely. The matching push/pop pair both reference the same
      handler, so they are dropped together and trap-stack balance is preserved.
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
    C.In.Block.t -> instr_index:int -> C.cursor -> C.Instruction.t result

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
          C.In.Block.t -> instr_index:int -> C.cursor -> C.Instruction.t result

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

      let visit_block block c =
        let rec loop = function
          | [] -> Unchanged
          | (module Red : S) :: rest -> (
            match Red.visit_block block c with
            | Unchanged -> loop rest
            | Replaced () -> Replaced ())
        in
        loop children

      let visit_instruction block ~instr_index c =
        let rec loop = function
          | [] -> Unchanged
          | (module Red : S) :: rest -> (
            match Red.visit_instruction block ~instr_index c with
            | Unchanged -> loop rest
            | Replaced instr -> Replaced instr)
        in
        loop children

      let visit_terminator block c =
        let rec loop = function
          | [] -> Unchanged
          | (module Red : S) :: rest -> (
            match Red.visit_terminator block c with
            | Unchanged -> loop rest
            | Replaced () -> Replaced ())
        in
        loop children

      let rewrite_instruction c instr =
        let rec loop = function
          | [] -> Unchanged
          | (module Red : S) :: rest -> (
            match Red.rewrite_instruction c instr with
            | Unchanged -> loop rest
            | Replaced instr' -> Replaced instr')
        in
        loop children

      let rewrite_terminator c ~dbg t =
        let rec loop = function
          | [] -> Unchanged
          | (module Red : S) :: rest -> (
            match Red.rewrite_terminator c ~dbg t with
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
  (* For input blocks where some params were dropped, bidirectional index maps
     between input and output param positions: - [to_old.(new_idx) = old_idx] -
     [to_new.(old_idx) = new_idx], or [-1] if the param was dropped. Blocks not
     in this table keep all params in order (identity mapping); see
     [compute_kept_block_params] for the dropping criterion. *)
  let kept_block_params : (int array * int array) In.Block.Tbl.t =
    In.Block.Tbl.create 64
  in
  let op_map : Out.Instruction.t In.Instruction_id.Tbl.t =
    In.Instruction_id.Tbl.create 256
  in
  (* A param can be dropped only if every incoming edge passes its arg through a
     [Goto] (the only terminator that has positional per-param args). A non-Goto
     predecessor supplies the param's value through a runtime-fixed mechanism,
     so its position must be preserved — we model this as "every param is used"
     for that target. The function entry is handled separately below: its params
     come from the function ABI and are never droppable.

     Returns [None] for the identity mapping (all params kept in order), in
     which case the caller does not record an entry in [kept_block_params]. *)
  let compute_kept_block_params (block : In.Block.t) :
      (int array * int array) option =
    let n = Array.length block.params in
    let any_non_goto_pred =
      In.Block.Set.exists
        (fun (pred : In.Block.t) ->
          match[@warning "-fragile-match"] pred.terminator with
          | Goto _ -> false
          | _ -> true)
        block.predecessors
    in
    if
      any_non_goto_pred || keep_unused_ops
      || Array.for_all
           (fun (param : In.Block.param) -> param.usage_count > 0)
           block.params
    then None
    else
      let to_old = Array.make n 0 in
      let to_new = Array.make n (-1) in
      let j = ref 0 in
      for i = 0 to n - 1 do
        if block.params.(i).usage_count > 0
        then begin
          to_old.(!j) <- i;
          to_new.(i) <- !j;
          incr j
        end
      done;
      let to_old = Array.sub to_old 0 !j in
      Some (to_old, to_new)
  in
  (* Step 1: create an output block for each input block. The entry's params
     come from the function ABI and are kept verbatim; other blocks may drop
     unused params per [compute_kept_block_params]. *)
  let entry_out = Out.entry in
  In.Block.Tbl.replace block_map In.entry entry_out;
  List.iter
    (fun (block : In.Block.t) ->
      if not (In.Block.equal block In.entry)
      then begin
        let kept_opt = compute_kept_block_params block in
        Option.iter (In.Block.Tbl.replace kept_block_params block) kept_opt;
        let params =
          match kept_opt with
          | None -> Array.map (fun (p : In.Block.param) -> p.typ) block.params
          | Some (to_old, _) -> Array.map (fun i -> block.params.(i).typ) to_old
        in
        let { Out.block = new_out; _ } = Out.new_block ~params in
        let out_params = Out.Block.params new_out in
        let copy_name new_idx old_idx =
          Option.iter
            (Out.Block.set_param_name out_params.(new_idx))
            block.params.(old_idx).name
        in
        (match kept_opt with
        | None -> Array.iteri (fun i _ -> copy_name i i) block.params
        | Some (to_old, _) -> Array.iteri copy_name to_old);
        In.Block.Tbl.replace block_map block new_out
      end)
    In.blocks;
  let map_block (old : In.Block.t) : Out.Block.t =
    In.Block.Tbl.find block_map old
  in
  (* Select from [args] the entries at positions whose target params were kept.
     Filtering before [map_arg] avoids touching args that feed dropped params:
     those args may be defined by dead [Op]s skipped by [visit_instruction], so
     they are absent from [op_map]. *)
  let filter_in_args_for (old : In.Block.t) (args : In.Instruction.t array) :
      In.Instruction.t array =
    match In.Block.Tbl.find_opt kept_block_params old with
    | None -> args
    | Some (to_old, _) -> Array.map (fun i -> args.(i)) to_old
  in
  (* Old index → new index for [block]'s params (post-filtering). *)
  let remap_param_index (block : In.Block.t) (old_index : int) : int =
    match In.Block.Tbl.find_opt kept_block_params block with
    | None -> old_index
    | Some (_, to_new) ->
      let new_index = to_new.(old_index) in
      assert (new_index >= 0);
      new_index
  in
  let rec map_arg (instr : In.Instruction.t) : Out.Instruction.t =
    match instr with
    | Op { id; _ } -> In.Instruction_id.Tbl.find op_map id
    | Block_param { block; index } ->
      Out.Instruction.make_block_param (map_block block)
        (remap_param_index block index)
    | Proj { index; src } -> Out.Instruction.make_proj ~index (map_arg src)
    | Tuple _ | Push_trap _ | Pop_trap _ | Stack_check _ | Name_for_debugger _
      ->
      Misc.fatal_error "Unexpected instruction in Ssa_reducer map_arg_impl"
  in
  let map_args (args : In.Instruction.t array) : Out.Instruction.t array =
    Array.map map_arg args
  in
  let map_instruction (instr : In.Instruction.t) : Out.Instruction.t =
    match instr with
    | Op { op; typ; args; dbg; name; _ } ->
      let new_op =
        Out.Instruction.make_op ~op ~typ ~args:(map_args args) ~dbg
      in
      Option.iter (Out.Instruction.set_name new_op) name;
      new_op
    | Push_trap { handler } -> Push_trap { handler = map_block handler }
    | Pop_trap { handler } -> Pop_trap { handler = map_block handler }
    | Stack_check { max_frame_size_bytes } ->
      Stack_check { max_frame_size_bytes }
    | Name_for_debugger { ident; provenance; which_parameter; regs } ->
      Name_for_debugger
        { ident; provenance; which_parameter; regs = map_args regs }
    | Block_param _ | Proj _ | Tuple _ -> assert false
  in
  let map_terminator (t : In.Terminator.t) : Out.Terminator.t =
    match t with
    | Goto { goto; args } ->
      let mapped_args = Array.map map_arg (filter_in_args_for goto args) in
      Goto { goto = map_block goto; args = mapped_args }
    | Branch { cond; ifso; ifnot } ->
      Branch
        { cond = map_arg cond; ifso = map_block ifso; ifnot = map_block ifnot }
    | Switch { index; targets } ->
      Switch { index = map_arg index; targets = Array.map map_block targets }
    | Return { args } -> Return { args = map_args args }
    | Raise { raise_kind; args } -> Raise { raise_kind; args = map_args args }
    | Tailcall_self { destination; args } ->
      Tailcall_self
        { destination = map_block destination; args = map_args args }
    | Tailcall_func { op; args } -> Tailcall_func { op; args = map_args args }
    | Call { op; args; continuation; may_raise; nontail } ->
      Call
        { op;
          args = map_args args;
          continuation = map_block continuation;
          may_raise;
          nontail
        }
    | Invalid { message; args; continuation } ->
      Invalid
        { message;
          args = map_args args;
          continuation = Option.map map_block continuation
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

      let emit_instruction c instr =
        match Red.rewrite_instruction c instr with
        | Unchanged ->
          Out.emit_instruction c instr;
          instr
        | Replaced instr' -> instr'

      let emit_op c ~op ~dbg ~typ ~args =
        emit_instruction c (Out.Instruction.make_op ~op ~typ ~args ~dbg)

      let finish_block c ~dbg term =
        match Red.rewrite_terminator c ~dbg term with
        | Unchanged -> Out.finish_block c ~dbg term
        | Replaced () -> ()

      let map_arg = map_arg

      let map_block = map_block

      let visit_instruction (block : In.Block.t) ~instr_index c =
        let instr = Array.get block.body instr_index in
        let should_drop =
          match[@warning "-fragile-match"] instr with
          (* Skip dead Ops: their args may reference [Block_param]s that we've
             dropped (see [compute_kept_block_params]), and emitting them would
             propagate those dangling references into the new graph. An Op with
             [usage_count = 0] is by definition not referenced by any live
             consumer. [keep_unused_ops] disables this filtering so the output
             is structurally faithful to the input — used before [Cfg_compare]
             so it can match the baseline pipeline. *)
          | Op { usage_count = 0; _ } when not keep_unused_ops -> true
          (* We drop trap handlers as well as their [Push_trap] / [Pop_trap]
             when they become unreachable. *)
          | (Push_trap { handler } | Pop_trap { handler })
            when In.Block.Set.is_empty handler.predecessors ->
            true
          | _ -> false
        in
        if not should_drop
        then begin
          let replacement =
            match Red.visit_instruction block ~instr_index c with
            | Replaced replacement -> replacement
            | Unchanged -> emit_instruction c (map_instruction instr)
          in
          if
            In.Instruction.result_arity instr
            <> Instruction.result_arity replacement
          then
            Misc.fatal_errorf
              "Ssa_reducer: replacement arity %d does not match input arity \
               %d.@ Input: %a@ Replacement: %a"
              (Instruction.result_arity replacement)
              (In.Instruction.result_arity instr)
              In.print_instruction instr Out.print_instruction replacement;
          match[@warning "-fragile-match"] instr with
          | Op { id; _ } -> In.Instruction_id.Tbl.replace op_map id replacement
          | _ -> ()
        end

      let visit_terminator (block : In.Block.t) c =
        match Red.visit_terminator block c with
        | Replaced () ->
          if not (Out.is_finished c)
          then
            Misc.fatal_error
              "The reducer promised to have replaced the block terminator, but \
               did not actually finish the block."
        | Unchanged ->
          let term = map_terminator block.terminator in
          finish_block c ~dbg:block.terminator_dbg term

      let finish = ()
    end

    and Red : sig
      val analyze : unit -> unit

      val visit_block : In.Block.t -> Out.cursor -> unit result

      val visit_instruction :
        In.Block.t -> instr_index:int -> Out.cursor -> Out.Instruction.t result

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
    (fun (block : In.Block.t) ->
      let out_block = In.Block.Tbl.find block_map block in
      let c = Out.start_block out_block in
      try
        match Red.visit_block block c with
        | Replaced () -> ()
        | Unchanged ->
          Array.iteri
            (fun instr_index _ -> Ctx.visit_instruction block ~instr_index c)
            block.body;
          Ctx.visit_terminator block c
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        Format.eprintf
          "*** Ssa_reducer.run error for %s while processing block %a: %s@.*** \
           Input SSA:@.%a@."
          In.function_info.fun_name In.print_block_id block
          (Printexc.to_string exn) Ssa_print.print
          (module In : Ssa.Finished_graph);
        Format.pp_print_flush Format.err_formatter ();
        Printexc.raise_with_backtrace exn bt)
    In.blocks;
  let result = Out.finish () in
  Ssa_validate.validate result;
  result
