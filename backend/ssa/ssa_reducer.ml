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

    A reducer is a functor over a [Context]; {!Make_run} instantiates it once
    and drives the framework. [Make_run.run] allocates a fresh output graph,
    creates an output block per input block, then walks the input in the
    finished-graph default order (which is guaranteed to have dominators first)
    and translates each block while removing unused block parameters.

    The per-run state (the two graphs and the input->output reference maps)
    lives in [Context.t]; it is threaded to every hook and on to the [Context]
    operations. [Context] is a plain (non-recursive) module so that a reducer's
    calls into it ([Cursor.emit_op] / [Cursor.finish_block] / [map_value] / ...)
    inline. To route emissions back through the reducer's [emit_op] and
    [finish_block] hooks without a recursive module, [Context.t] also stores
    those hooks (with the run's analysis already captured); the only non-inlined
    step is then the indirect call back into the reducer.

    Two layered hooks:
    - [visit_instruction] / [visit_terminator]: intercept the walk over the
      input. Return [Emitted_replacement] after take over (that is, having
      emitted something else), or [For_next_reducer] to let the framework apply
      its default translation.
    - [emit_op] / [finish_block]: intercept emissions into the output. Fire on
      every emission — both the framework's default translation and
      reducer-driven ones go through here.

    Default translation, applied when [visit_*] returns [For_next_reducer]:
    - Args (op args, block-param uses, terminator args) and blocks are mapped
      from the input to the output via [map_value] and [map_block].
    - Block params with [usage_count = 0] are dropped from both the block
      parameter list and from [Continue]s.
    - Other terminators are reconstructed with their args mapped through.

    Unused instruction and unreachable block or exception handler cleanup
    happens in [Ssa.finish_graph], not here.

    [keep_unused_ops] disables removing unused operations and parameters, used
    before [Cfg_compare]. *)

open! Ssa.Export

type 'a did_emit =
  | For_next_reducer
  | Emitted_replacement of 'a

(* A param can be dropped only if every incoming edge passes its arg through a
   [Continue (Goto _)] (the only terminator with positional per-param args). Any
   other predecessor supplies the parameter's value through a runtime-fixed
   mechanism, so the parameter's position must be preserved. *)
let dropped_param_sentinel = -1

module type Context = sig
  type t

  type out

  (** Mirrors [Ssa.Cursor], with the context taking the graph's place; [emit_op]
      and [finish_block] route through the reducer's [emit_op] / [finish_block]
      hooks. *)
  module Cursor : sig
    type context := t

    type t

    val start : out Block.t -> t

    val move : t -> new_pos:out Block.t -> unit

    val is_finished : t -> bool

    val emit_op :
      context ->
      t ->
      Ssa.op ->
      Debuginfo.t ->
      Cmm.machtype ->
      out Value.t array ->
      out Value.t array

    val emit_push_trap : t -> handler:out Block.t -> unit

    val emit_pop_trap : t -> handler:out Block.t -> unit

    val finish_block :
      context -> t -> dbg:Debuginfo.t -> out Terminator.t -> unit
  end

  val in_graph : t -> finished Ssa.graph

  val out_graph : t -> out Ssa.graph

  val map_value : t -> finished Value.t -> out Value.t

  val map_block : t -> finished Block.t -> out Block.t
end

module type S = sig
  type analysis_result

  type context

  type cursor

  type out

  val analyze : finished Ssa.graph -> analysis_result

  val visit_instruction :
    analysis_result ->
    context ->
    finished Block.t ->
    instr_index:int ->
    cursor ->
    out Value.t array did_emit

  val visit_terminator :
    analysis_result -> context -> finished Block.t -> cursor -> unit did_emit

  val emit_op :
    analysis_result ->
    context ->
    cursor ->
    op:Ssa.op ->
    dbg:Debuginfo.t ->
    typ:Cmm.machtype ->
    args:out Value.t array ->
    out Value.t array did_emit

  val finish_block :
    analysis_result ->
    context ->
    cursor ->
    dbg:Debuginfo.t ->
    out Terminator.t ->
    unit did_emit
end

module type Reducer = functor (C : Context) ->
  S with type context := C.t and type cursor := C.Cursor.t and type out := C.out

module Default (C : Context) = struct
  type analysis_result = unit

  let analyze (_ : finished Ssa.graph) = ()

  let visit_instruction (_ : analysis_result) (_ : C.t) (_ : finished Block.t)
      ~instr_index:(_ : int) (_ : C.Cursor.t) =
    For_next_reducer

  let visit_terminator (_ : analysis_result) (_ : C.t) (_ : finished Block.t)
      (_ : C.Cursor.t) =
    For_next_reducer

  let emit_op (_ : analysis_result) (_ : C.t) (_ : C.Cursor.t) ~op:(_ : Ssa.op)
      ~dbg:(_ : Debuginfo.t) ~typ:(_ : Cmm.machtype)
      ~args:(_ : C.out Value.t array) =
    For_next_reducer

  let finish_block (_ : analysis_result) (_ : C.t) (_ : C.Cursor.t)
      ~dbg:(_ : Debuginfo.t) (_ : C.out Terminator.t) =
    For_next_reducer
end

module Combine (Reducer_a : Reducer) (Reducer_b : Reducer) : Reducer =
functor
  (C : Context)
  ->
  struct
    module A = Reducer_a (C)
    module B = Reducer_b (C)

    type analysis_result = A.analysis_result * B.analysis_result

    let analyze g = A.analyze g, B.analyze g

    let visit_instruction (ra, rb) ctx block ~instr_index c =
      match A.visit_instruction ra ctx block ~instr_index c with
      | For_next_reducer -> B.visit_instruction rb ctx block ~instr_index c
      | Emitted_replacement vs -> Emitted_replacement vs

    let visit_terminator (ra, rb) ctx block c =
      match A.visit_terminator ra ctx block c with
      | For_next_reducer -> B.visit_terminator rb ctx block c
      | Emitted_replacement () -> Emitted_replacement ()

    let emit_op (ra, rb) ctx c ~op ~dbg ~typ ~args =
      match A.emit_op ra ctx c ~op ~dbg ~typ ~args with
      | For_next_reducer -> B.emit_op rb ctx c ~op ~dbg ~typ ~args
      | Emitted_replacement vs -> Emitted_replacement vs

    let finish_block (ra, rb) ctx c ~dbg t =
      match A.finish_block ra ctx c ~dbg t with
      | For_next_reducer -> B.finish_block rb ctx c ~dbg t
      | Emitted_replacement () -> Emitted_replacement ()
  end

module Make_run (R : Reducer) = struct
  (** [Context] is a plain (non-recursive) module so that the reducer's calls
      into it are inlined. The reducer's [emit_op] and [finish_block] hooks
      (with the run's analysis already captured) live in [t], so that
      [Cursor.emit_op] / [Cursor.finish_block] route emissions back through the
      reducer; only that (uncommon) indirect call is left un-inlined. *)
  module Context = struct
    type out = under_construction

    type t =
      { in_graph : finished Ssa.graph;
        out_graph : under_construction Ssa.graph;
        block_map : under_construction Block.t Block.Tbl.t;
        (* Output value(s) for each input [Op], indexed by its id. *)
        op_map : under_construction Value.t array Instruction.Id.Tbl.t;
        (* Old param index -> new param index (or [dropped_param_sentinel]). *)
        block_param_map : int array Block.Tbl.t;
        (* Output [Block_param] values, in the compressed (post-drop) order. *)
        block_param_values : under_construction Value.t array Block.Tbl.t;
        emit_op :
          t ->
          Ssa.Cursor.t ->
          op:Ssa.op ->
          dbg:Debuginfo.t ->
          typ:Cmm.machtype ->
          args:under_construction Value.t array ->
          under_construction Value.t array did_emit;
        finish_block :
          t ->
          Ssa.Cursor.t ->
          dbg:Debuginfo.t ->
          under_construction Terminator.t ->
          unit did_emit
      }

    type context = t

    module Cursor = struct
      include Ssa.Cursor

      let emit_op (ctx : context) c op dbg typ args =
        match ctx.emit_op ctx c ~op ~dbg ~typ ~args with
        | For_next_reducer -> Ssa.Cursor.emit_op ctx.out_graph c op dbg typ args
        | Emitted_replacement vs -> vs

      let finish_block (ctx : context) c ~dbg term =
        match ctx.finish_block ctx c ~dbg term with
        | For_next_reducer -> Ssa.Cursor.finish_block ctx.out_graph c ~dbg term
        | Emitted_replacement () -> ()
    end

    let create ~in_graph ~out_graph ~block_map ~op_map ~block_param_map
        ~block_param_values ~emit_op ~finish_block =
      { in_graph;
        out_graph;
        block_map;
        op_map;
        block_param_map;
        block_param_values;
        emit_op;
        finish_block
      }

    let in_graph t = t.in_graph

    let out_graph t = t.out_graph

    let map_block t (old : finished Block.t) : under_construction Block.t =
      Block.Tbl.find t.block_map old

    let map_value t (value : finished Value.t) : under_construction Value.t =
      match value with
      | Res ({ id; _ }, i) -> (Instruction.Id.Tbl.find t.op_map id).(i)
      | Block_param (block, i) ->
        let new_index = (Block.Tbl.find t.block_param_map block).(i) in
        assert (new_index <> dropped_param_sentinel);
        (Block.Tbl.find t.block_param_values block).(new_index)
      | Undefined -> Value.undefined
  end

  module Red = R (Context)

  let run ?(keep_unused_ops = false) (in_graph : finished Ssa.graph) :
      finished Ssa.graph =
    let analysis = Red.analyze in_graph in
    let out_graph =
      Ssa.create_graph (Ssa.function_info in_graph) ~keep_unused_ops
    in
    (* Map each input block to its output counterpart. *)
    let block_map : under_construction Block.t Block.Tbl.t =
      Block.Tbl.create 64
    in
    let op_map : under_construction Value.t array Instruction.Id.Tbl.t =
      Instruction.Id.Tbl.create 256
    in
    let block_param_map : int array Block.Tbl.t = Block.Tbl.create 64 in
    let block_param_values : under_construction Value.t array Block.Tbl.t =
      Block.Tbl.create 64
    in
    let compute_block_param_map (block : finished Block.t) : int array =
      let params = Block.params block in
      let n = Array.length params in
      (* A param can only be dropped if every predecessor feeds it via a
         positional [Continue (Goto _)] arg, which we can then drop too. Any
         other edge (call results, exception bucket) supplies it at a
         runtime-fixed position. *)
      let any_non_continue_pred () =
        List.exists
          (fun (pred : finished Block.t) ->
            match Block.terminator pred with
            | Continue { continuation = Goto _; _ } -> false
            | Continue { continuation = Return | Raise _; _ }
            | Call _ | Switch _ | Invalid _ ->
              true)
          (Block.predecessors block)
      in
      if
        keep_unused_ops || any_non_continue_pred ()
        || Array.for_all (fun param -> Value.usage_count param > 0) params
      then Array.init n Fun.id
      else
        let to_new = Array.make n dropped_param_sentinel in
        let j = ref 0 in
        for i = 0 to n - 1 do
          if Value.usage_count params.(i) > 0
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
    let in_entry = Ssa.entry in_graph in
    Block.Tbl.replace block_map in_entry (Ssa.entry out_graph);
    Block.Tbl.replace block_param_map in_entry
      (Array.init (Array.length (Block.params in_entry)) Fun.id);
    Block.Tbl.replace block_param_values in_entry
      (Block.params (Ssa.entry out_graph));
    List.iter
      (fun (block : finished Block.t) ->
        if not (Block.equal block in_entry)
        then begin
          let param_map = compute_block_param_map block in
          Block.Tbl.replace block_param_map block param_map;
          let params =
            Block.params block |> Array.to_list
            |> List.filteri (fun i _ -> param_map.(i) <> dropped_param_sentinel)
            |> List.map (fun param -> Value.typ param, Value.name param)
            |> Array.of_list
          in
          let out_block = Block.create_with_names out_graph ~params in
          Block.Tbl.replace block_map block out_block;
          Block.Tbl.replace block_param_values block (Block.params out_block)
        end)
      (Ssa.blocks in_graph);
    let ctx =
      Context.create ~in_graph ~out_graph ~block_map ~op_map ~block_param_map
        ~block_param_values ~emit_op:(Red.emit_op analysis)
        ~finish_block:(Red.finish_block analysis)
    in
    let map_values (args : finished Value.t array) :
        under_construction Value.t array =
      Array.map (Context.map_value ctx) args
    in
    (* Default translation of one body instruction, returning the value(s) its
       results map to ([||] for a trap instruction). *)
    let default_translate_instruction (instr : finished Instruction.t)
        (c : Cursor.t) : under_construction Value.t array =
      match instr with
      | Op { op; typ; args; dbg; name; _ } ->
        let vs = Context.Cursor.emit_op ctx c op dbg typ (map_values args) in
        (match name with
        | Some name when Array.length vs > 0 -> Value.set_name vs.(0) name
        | Some _ | None -> ());
        vs
      | Push_trap { handler } ->
        Context.Cursor.emit_push_trap c ~handler:(Context.map_block ctx handler);
        [||]
      | Pop_trap { handler } ->
        Context.Cursor.emit_pop_trap c ~handler:(Context.map_block ctx handler);
        [||]
    in
    let map_continuation (cont : finished Ssa.continuation) :
        under_construction Ssa.continuation =
      match cont with
      | Goto b -> Goto (Context.map_block ctx b)
      | Return -> Return
      | Raise k -> Raise k
    in
    let map_terminator (t : finished Terminator.t) :
        under_construction Terminator.t =
      match t with
      | Continue { continuation = Goto goto; args } ->
        (* Drop the args going to dropped target params. *)
        let param_map = Block.Tbl.find block_param_map goto in
        let mapped_args =
          args
          |> Misc.Stdlib.Array.filteri (fun i _ ->
              param_map.(i) <> dropped_param_sentinel)
          |> Array.map (Context.map_value ctx)
        in
        Continue
          { continuation = Goto (Context.map_block ctx goto);
            args = mapped_args
          }
      | Continue { continuation = (Return | Raise _) as continuation; args } ->
        Continue
          { continuation = map_continuation continuation;
            args = map_values args
          }
      | Switch { index; targets } ->
        Switch
          { index = Context.map_value ctx index;
            targets = Array.map (Context.map_block ctx) targets
          }
      | Call { op; args; continuation; may_raise; nontail } ->
        Call
          { op;
            args = map_values args;
            continuation = map_continuation continuation;
            may_raise;
            nontail
          }
      | Invalid { message; args; continuation } ->
        Invalid
          { message;
            args = map_values args;
            continuation = Option.map (Context.map_block ctx) continuation
          }
    in
    let visit_instruction (block : finished Block.t) ~instr_index c =
      let instr = Array.get (Block.body block) instr_index in
      let vs =
        match Red.visit_instruction analysis ctx block ~instr_index c with
        | Emitted_replacement vs -> vs
        | For_next_reducer -> default_translate_instruction instr c
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
      match Red.visit_terminator analysis ctx block c with
      | Emitted_replacement () ->
        if not (Cursor.is_finished c)
        then
          Misc.fatal_error
            "The reducer promised to have replaced the block terminator, but \
             did not actually finish the block."
      | For_next_reducer ->
        let term = map_terminator (Block.terminator block) in
        Context.Cursor.finish_block ctx c ~dbg:(Block.terminator_dbg block) term
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
    let result = Ssa.finish_graph out_graph in
    Ssa_invariants.validate result;
    result
end
