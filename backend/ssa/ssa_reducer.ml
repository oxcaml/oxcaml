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
    - Other terminators are reconstructed with their args mapped through.

    Dead-Op and unreachable-handler cleanup (dropping [Op]s with
    [usage_count = 0] and [Push_trap]/[Pop_trap] whose handler has no
    predecessors) happens in [Ssa.finalize_blocks] inside [Out.finish], not
    here.

    [keep_unused_ops] disables removing unused operations and parameters, used
    before [Cfg_compare]. *)

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
  let module Out =
    (val Ssa.make_builder In.function_info ~keep_unused_ops : Ssa.Graph_builder)
  in
  (* Map each input block to its output counterpart. *)
  let block_map : Out.Block.t In.Block.Tbl.t = In.Block.Tbl.create 64 in
  let op_map : Out.Instruction.t In.Instruction_id.Tbl.t =
    In.Instruction_id.Tbl.create 256
  in
  (* Maps each old param index to its new index in [Out], or
     [dropped_param_sentinel] if the param is dropped. A param can be dropped
     only if every incoming edge passes its arg through a [Goto] (the only
     terminator that has positional per-param args). A non-Goto predecessor
     supplies the parameter's value through a runtime-fixed mechanism, so the
     parameter's position must be preserved. *)
  let block_param_map : int array In.Block.Tbl.t = In.Block.Tbl.create 64 in
  let dropped_param_sentinel = -1 in
  let compute_block_param_map (block : In.Block.t) : int array =
    let n = Array.length block.params in
    let any_non_goto_pred () =
      In.Block.Set.exists
        (fun (pred : In.Block.t) ->
          match[@warning "-fragile-match"] pred.terminator with
          | Goto _ -> false
          | _ -> true)
        block.predecessors
    in
    if
      keep_unused_ops || any_non_goto_pred ()
      || Array.for_all
           (fun (param : In.Block.param) -> param.usage_count > 0)
           block.params
    then Array.init n Fun.id
    else
      let to_new = Array.make n dropped_param_sentinel in
      let j = ref 0 in
      for i = 0 to n - 1 do
        if block.params.(i).usage_count > 0
        then begin
          to_new.(i) <- !j;
          incr j
        end
      done;
      to_new
  in
  (* Step 1: create an output block for each input block. The entry's params
     come from the function ABI and are kept verbatim; other blocks may drop
     unused params per [compute_block_param_map]. *)
  let entry_out = Out.entry in
  In.Block.Tbl.replace block_map In.entry entry_out;
  In.Block.Tbl.replace block_param_map In.entry
    (Array.init (Array.length In.entry.params) Fun.id);
  List.iter
    (fun (block : In.Block.t) ->
      if not (In.Block.equal block In.entry)
      then begin
        let param_map = compute_block_param_map block in
        In.Block.Tbl.replace block_param_map block param_map;
        let params =
          block.params |> Array.to_list
          |> List.filteri (fun i _ -> param_map.(i) <> dropped_param_sentinel)
          |> List.map (fun (param : In.Block.param) -> param.typ, param.name)
          |> Array.of_list
        in
        In.Block.Tbl.replace block_map block (Out.new_block_with_names ~params)
      end)
    In.blocks;
  let map_block (old : In.Block.t) : Out.Block.t =
    In.Block.Tbl.find block_map old
  in
  let rec map_arg (instr : In.Instruction.t) : Out.Instruction.t =
    match instr with
    | Op { id; _ } -> In.Instruction_id.Tbl.find op_map id
    | Block_param { block; index } ->
      let new_index = (In.Block.Tbl.find block_param_map block).(index) in
      assert (new_index <> dropped_param_sentinel);
      Out.Instruction.make_block_param (map_block block) new_index
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
    | Name_for_debugger { ident; provenance; which_parameter; args } ->
      Name_for_debugger
        { ident; provenance; which_parameter; args = map_args args }
    | Block_param _ | Proj _ | Tuple _ -> assert false
  in
  let map_terminator (t : In.Terminator.t) : Out.Terminator.t =
    match t with
    | Goto { goto; args } ->
      let param_map = In.Block.Tbl.find block_param_map goto in
      let mapped_args =
        args
        |> Misc.Stdlib.Array.filteri (fun i _ ->
            param_map.(i) <> dropped_param_sentinel)
        |> Array.map (Option.map map_arg)
      in
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
            "Ssa_reducer: replacement arity %d does not match input arity %d.@ \
             Input: %a@ Replacement: %a"
            (Instruction.result_arity replacement)
            (In.Instruction.result_arity instr)
            In.print_instruction instr Out.print_instruction replacement;
        match[@warning "-fragile-match"] instr with
        | Op { id; _ } -> In.Instruction_id.Tbl.replace op_map id replacement
        | _ -> ()

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
