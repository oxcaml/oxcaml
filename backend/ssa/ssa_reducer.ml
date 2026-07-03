(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-40-41-42"]

(** Framework for SSA-to-SSA transformations.

    A reducer is a module matching {!Reducer}. {!Make_run} turns one into a
    pass: [run] rebuilds the input graph into a fresh output graph, visiting
    blocks in dominators-first order (one output block per input block) and
    dropping block parameters that turned out unused. At each instruction and
    terminator the reducer's hooks may take over; where they defer, the
    framework applies its default translation (below).

    The per-run state (the two graphs and the input->output reference maps)
    lives in [Context.t]. The context is built once and passed to the reducer's
    [create], which produces the reducer's own state — holding whatever the
    reducer needs, such as some analysis result and the context itself; that
    state is then threaded to every hook. [Context] is a plain (non-recursive)
    module so that a reducer's calls into it ([Cursor.emit_op] /
    [Cursor.finish_block] / [map_value] / ...) inline. To route emissions back
    through the reducer's [emit_op] and [finish_block] hooks without a recursive
    module, [Context.t] stores those hooks (which reach the reducer state
    through a forward reference, since it is built from the context); the only
    non-inlined step is then the indirect call back into the reducer.

    Two layered hooks:
    - [visit_instruction] / [visit_terminator]: intercept the walk over the
      input. Return [Emitted_replacement] after take over (that is, having
      emitted something else), or [For_next_reducer] to let the framework apply
      its default translation.
    - [emit_op] / [finish_block]: intercept emissions into the output. Fire on
      every emission — both the framework's default translation and
      reducer-driven ones go through here.

    Default translation, applied when [visit_*] returns [For_next_reducer]:
    - Op args and block-param uses are mapped from the input to the output via
      [map_value]; block references via [map_block].
    - The terminator is translated by [map_terminator] (exposed on the
      [Context]): its values and blocks are mapped, and a [Continue (Goto _)]'s
      args feeding params that were dropped from the target block's parameter
      list are dropped to match.

    Unused instruction and unreachable block or exception handler cleanup
    happens in [Ssa.finish_graph], not here.

    [keep_unused_ops] disables removing unused operations and parameters, used
    before [Cfg_compare]. *)

open! Ssa.Export

(** [Context] methods to emit into the output graph are mutually recursive with
    the reducer's hooks. Since recursive modules currently stop inlining, we
    instead cut the cycle at the point of the reducer calling into the context,
    so that the actual implementation of these functions can be inlined into the
    main loop in [run].

    [out] is really [under_construction]; in the interface it is keept abstract
    so a reducer can only extend the output through the [Cursor] operations
    (which route through the [emit_*] hooks), not via the raw [Ssa] builder. *)
module Context = struct
  type out = under_construction

  type t =
    { in_graph : finished Ssa.graph;
      out_graph : under_construction Ssa.graph;
      block_map : under_construction Block.t Block.Tbl.t;
      (* Output value(s) for each input [Op], indexed by its id. *)
      op_map :
        (finished, under_construction Value.t array) Instruction.Id.Tbl.t;
      (* Per input block, indexed by the original param index: the output value
         of a kept param, or [Value.undefined] for a param dropped from the
         output block. *)
      block_param_values : under_construction Value.t array Block.Tbl.t;
      emit_op :
        Ssa.Cursor.t ->
        op:Ssa.op ->
        dbg:Debuginfo.t ->
        typ:Cmm.machtype ->
        args:under_construction Value.t array ->
        under_construction Value.t array;
      finish_block :
        Ssa.Cursor.t ->
        dbg:Debuginfo.t ->
        under_construction Terminator.t ->
        unit
    }

  type context = t

  module Cursor = struct
    include Ssa.Cursor

    let emit_op (ctx : context) c op dbg typ args =
      ctx.emit_op c ~op ~dbg ~typ ~args

    let finish_block (ctx : context) c ~dbg term = ctx.finish_block c ~dbg term
  end

  let in_graph t = t.in_graph

  let out_graph t = t.out_graph

  let map_block t (old : finished Block.t) : under_construction Block.t =
    Block.Tbl.find t.block_map old

  let map_value t (value : finished Value.t) : under_construction Value.t =
    match value with
    | Res ({ id; _ }, i) -> (Instruction.Id.Tbl.find t.op_map id).(i)
    | Block_param (block, i) -> (Block.Tbl.find t.block_param_values block).(i)
    | Undefined -> Value.undefined

  let map_values t (args : finished Value.t array) : out Value.t array =
    Array.map (map_value t) args

  let map_continuation t (cont : finished Ssa.continuation) :
      out Ssa.continuation =
    match cont with
    | Goto b -> Goto (map_block t b)
    | Return -> Return
    | Raise k -> Raise k
    | Unreachable -> Unreachable

  let map_terminator t (term : finished Terminator.t) : out Terminator.t =
    match term with
    | Continue { continuation = Goto goto; args } ->
      (* Drop the args going to dropped target params. *)
      let param_values = Block.Tbl.find t.block_param_values goto in
      let mapped_args =
        args
        |> Misc.Stdlib.Array.filteri (fun i _ ->
            not (Value.equal param_values.(i) Value.undefined))
        |> Array.map (map_value t)
      in
      Continue { continuation = Goto (map_block t goto); args = mapped_args }
    | Continue
        { continuation = (Return | Raise _ | Unreachable) as continuation;
          args
        } ->
      Continue
        { continuation = map_continuation t continuation;
          args = map_values t args
        }
    | Switch { index; targets } ->
      Switch
        { index = map_value t index; targets = Array.map (map_block t) targets }
    | Call { op; args; continuation; may_raise; nontail } ->
      Call
        { op;
          args = map_values t args;
          continuation = map_continuation t continuation;
          may_raise;
          nontail
        }
    | Invalid { message; args; continuation } ->
      Invalid
        { message;
          args = map_values t args;
          continuation = Option.map (map_block t) continuation
        }
end

type 'a reduction =
  | Unchanged
  | Reduce of (Context.Cursor.t -> 'a)

let reduce_output_terminator (ctx : Context.t) ~dbg
    (terminator : Context.out Terminator.t) =
  Reduce (fun c -> Context.Cursor.finish_block ctx c ~dbg terminator)

let reduce_input_terminator (ctx : Context.t) ~dbg
    (terminator : finished Terminator.t) =
  reduce_output_terminator ctx (Context.map_terminator ctx terminator) ~dbg

module type Reducer = sig
  type t

  val create : Context.t -> t

  val visit_instruction :
    t ->
    finished Block.t ->
    instr_index:int ->
    Context.out Value.t array reduction

  val visit_terminator : t -> finished Block.t -> unit reduction

  val emit_op :
    t ->
    op:Ssa.op ->
    dbg:Debuginfo.t ->
    typ:Cmm.machtype ->
    args:Context.out Value.t array ->
    Context.out Value.t array reduction

  val finish_block :
    t -> dbg:Debuginfo.t -> Context.out Terminator.t -> unit reduction
end

module Default_reducer = struct
  type t = Context.t

  let create ctx = ctx

  let visit_instruction _ (_ : finished Block.t) ~instr_index:(_ : int) =
    Unchanged

  let visit_terminator _ (_ : finished Block.t) = Unchanged

  let emit_op _ ~op:(_ : Ssa.op) ~dbg:(_ : Debuginfo.t) ~typ:(_ : Cmm.machtype)
      ~args:(_ : Context.out Value.t array) =
    Unchanged

  let finish_block _ ~dbg:(_ : Debuginfo.t) (_ : Context.out Terminator.t) =
    Unchanged
end

module Combine (A : Reducer) (B : Reducer) : Reducer = struct
  (* Each sub-reducer keeps its own state; both are built from the same context,
     so emissions from either still route through the combined chain. *)
  type t = A.t * B.t

  let create ctx = A.create ctx, B.create ctx

  let visit_instruction (a, b) block ~instr_index =
    match A.visit_instruction a block ~instr_index with
    | Unchanged -> B.visit_instruction b block ~instr_index
    | Reduce _ as r -> r

  let visit_terminator (a, b) block =
    match A.visit_terminator a block with
    | Unchanged -> B.visit_terminator b block
    | Reduce _ as r -> r

  let emit_op (a, b) ~op ~dbg ~typ ~args =
    match A.emit_op a ~op ~dbg ~typ ~args with
    | Unchanged -> B.emit_op b ~op ~dbg ~typ ~args
    | Reduce _ as r -> r

  let finish_block (a, b) ~dbg t =
    match A.finish_block a ~dbg t with
    | Unchanged -> B.finish_block b ~dbg t
    | Reduce _ as r -> r
end

module Make_run (R : Reducer) = struct
  let run ?(keep_unused_ops = false) (in_graph : finished Ssa.graph) :
      finished Ssa.graph =
    let out_graph =
      Ssa.create_graph (Ssa.function_info in_graph) ~keep_unused_ops
    in
    (* Map each input block to its output counterpart. *)
    let block_map : under_construction Block.t Block.Tbl.t =
      Block.Tbl.create 64
    in
    let op_map :
        (finished, under_construction Value.t array) Instruction.Id.Tbl.t =
      Instruction.Id.Tbl.create 256
    in
    let block_param_values : under_construction Value.t array Block.Tbl.t =
      Block.Tbl.create 64
    in
    (* Step 1: create an output block for each input block. The entry's params
       come from the function ABI and are kept verbatim; other blocks drop the
       params [Ssa] scheduled for removal (unused, and droppable given their
       predecessors). *)
    List.iter
      (fun (block : finished Block.t) ->
        let in_params = Block.params block in
        let params =
          in_params |> Array.to_list
          |> List.filter (fun param -> not (Value.scheduled_for_removal param))
          |> List.map (fun param -> Value.typ param, Value.name param)
          |> Array.of_list
        in
        let out_block =
          if Block.equal block (Ssa.entry in_graph)
          then Ssa.entry out_graph
          else Block.create_with_names out_graph ~params
        in
        Block.Tbl.replace block_map block out_block;
        (* Index the output values by the original param index, with
           [Value.undefined] in the dropped slots; the kept output params line
           up with the kept input params in order. *)
        let out_params = Block.params out_block in
        let next = ref 0 in
        let param_values =
          Array.map
            (fun param ->
              if Value.scheduled_for_removal param
              then Value.undefined
              else
                let v = out_params.(!next) in
                incr next;
                v)
            in_params
        in
        Block.Tbl.replace block_param_values block param_values)
      (Ssa.blocks in_graph);
    (* The whole emission step for an op / a terminator: the reducer's hook, and
       when it defers, the default emission into the output graph. These are the
       closures stored in [Context.t]; the main walk calls them directly, while
       [Context.Cursor.*] are thin wrappers onto them for the reducer. *)
    let rec emit_op c ~op ~dbg ~typ ~args =
      match R.emit_op (Lazy.force reducer) ~op ~dbg ~typ ~args with
      | Unchanged -> Ssa.Cursor.emit_op out_graph c op dbg typ args
      | Reduce f -> f c
    and finish_block c ~dbg term =
      match R.finish_block (Lazy.force reducer) ~dbg term with
      | Unchanged -> Ssa.Cursor.finish_block out_graph c ~dbg term
      | Reduce f -> f c
    and ctx : Context.t =
      { in_graph;
        out_graph;
        block_map;
        op_map;
        block_param_values;
        emit_op;
        finish_block
      }
    and reducer : R.t Lazy.t = lazy (R.create ctx) in
    let reducer = Lazy.force reducer in
    (* Default translation of one body instruction, returning the value(s) its
       results map to ([||] for a trap instruction). *)
    let default_translate_instruction (instr : finished Instruction.t)
        (c : Cursor.t) : under_construction Value.t array =
      match instr with
      | Op { op; typ; args; dbg; name; _ } ->
        let vs = emit_op c ~op ~dbg ~typ ~args:(Context.map_values ctx args) in
        (match name with
        | Some name when Array.length vs > 0 -> Value.set_name vs.(0) name
        | Some _ | None -> ());
        vs
      | Push_trap { handler } ->
        Cursor.emit_push_trap c ~handler:(Context.map_block ctx handler);
        [||]
      | Pop_trap { handler } ->
        Cursor.emit_pop_trap c ~handler:(Context.map_block ctx handler);
        [||]
    in
    let visit_instruction (block : finished Block.t) ~instr_index c =
      let instr = Array.get (Block.body block) instr_index in
      let vs =
        match R.visit_instruction reducer block ~instr_index with
        | Reduce f -> f c
        | Unchanged -> default_translate_instruction instr c
      in
      if Instruction.result_arity instr <> Array.length vs
      then
        Misc.fatal_errorf
          "Ssa_reducer: replacement arity %d does not match input arity %d.@ \
           Input: %a"
          (Array.length vs)
          (Instruction.result_arity instr)
          Instruction.print instr;
      match instr with
      | Op { id; _ } -> Instruction.Id.Tbl.replace op_map id vs
      | Push_trap _ | Pop_trap _ -> ()
    in
    let visit_terminator (block : finished Block.t) c =
      match R.visit_terminator reducer block with
      | Reduce f ->
        f c;
        if not (Cursor.is_finished c)
        then
          Misc.fatal_error
            "The reducer promised to have replaced the block terminator, but \
             did not actually finish the block."
      | Unchanged ->
        let term = Context.map_terminator ctx (Block.terminator block) in
        finish_block c ~dbg:(Block.terminator_dbg block) term
    in
    (* Step 2: walk the input graph in [blocks] order, which already guarantees
       each block's dominators come before it. *)
    List.iter
      (fun (block : finished Block.t) ->
        let out_block = Block.Tbl.find block_map block in
        let c = Cursor.start out_block in
        try
          Array.iteri
            (fun instr_index _ -> visit_instruction block ~instr_index c)
            (Block.body block);
          visit_terminator block c
        with exn ->
          let bt = Printexc.get_raw_backtrace () in
          Format.eprintf
            "*** Ssa_reducer.run error for %s while processing block %a: \
             %s@.*** Input SSA:@.%a@."
            (Ssa.function_info in_graph).sym_name Block.print_id block
            (Printexc.to_string exn) Ssa_print.print in_graph;
          Format.pp_print_flush Format.err_formatter ();
          Printexc.raise_with_backtrace exn bt)
      (Ssa.blocks in_graph);
    Ssa.finish_graph out_graph
end
