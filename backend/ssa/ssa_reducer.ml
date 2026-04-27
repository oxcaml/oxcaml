[@@@ocaml.warning "+a-40-41-42"]

module B = Ssa.Builder

module type Context = sig
  include Ssa.BuilderS

  val map_arg : Ssa.instruction -> Ssa.instruction

  val map_block : Ssa.block -> Ssa.block

  val visit_block : Ssa.block -> block_args:Ssa.instruction array -> t -> unit

  val visit_instruction : Ssa.block -> instr_index:int -> t -> unit

  val visit_terminator : Ssa.block -> t -> unit
end

module type S = sig
  type t

  val analyze : Ssa.t -> unit

  val visit_block : Ssa.block -> t -> [> `Unchanged | `Replaced]

  val visit_instruction :
    Ssa.block -> instr_index:int -> t -> [> `Unchanged | `Replaced]

  val visit_terminator : Ssa.block -> t -> [> `Unchanged | `Replaced]

  val rewrite_instruction :
    t -> Ssa.instruction -> [> `Unchanged | `Replaced of t * Ssa.instruction]

  val rewrite_terminator :
    t -> dbg:Debuginfo.t -> Ssa.terminator -> [> `Unchanged | `Replaced]
end

module type Reducer = functor (C : Context) -> S with type t = C.t

module Default : Reducer =
functor
  (C : Context)
  ->
  struct
    type t = C.t

    let analyze (_ : Ssa.t) = ()

    let visit_block (_ : Ssa.block) (_ : t) = `Unchanged

    let visit_instruction (_ : Ssa.block) ~instr_index:(_ : int) (_ : t) =
      `Unchanged

    let visit_terminator (_ : Ssa.block) (_ : t) = `Unchanged

    let rewrite_instruction (_ : t) (_ : Ssa.instruction) = `Unchanged

    let rewrite_terminator (_ : t) ~dbg:(_ : Debuginfo.t) (_ : Ssa.terminator) =
      `Unchanged
  end

let combine (rs : (module Reducer) list) : (module Reducer) =
  let module Combined =
  functor
    (C : Context)
    ->
    struct
      type t = C.t

      let children : (module S with type t = t) list =
        List.map
          (fun m ->
            let module Red = (val m : Reducer) in
            let module Inst = Red (C) in
            (module Inst : S with type t = t))
          rs

      let analyze ssa =
        List.iter
          (fun (module Red : S with type t = t) -> Red.analyze ssa)
          children

      let visit_block blk b =
        let rec loop = function
          | [] -> `Unchanged
          | (module Red : S with type t = t) :: rest -> (
            match Red.visit_block blk b with
            | `Unchanged -> loop rest
            | `Replaced -> `Replaced)
        in
        loop children

      let visit_instruction blk ~instr_index b =
        let rec loop = function
          | [] -> `Unchanged
          | (module Red : S with type t = t) :: rest -> (
            match Red.visit_instruction blk ~instr_index b with
            | `Unchanged -> loop rest
            | `Replaced -> `Replaced)
        in
        loop children

      let visit_terminator blk b =
        let rec loop = function
          | [] -> `Unchanged
          | (module Red : S with type t = t) :: rest -> (
            match Red.visit_terminator blk b with
            | `Unchanged -> loop rest
            | `Replaced -> `Replaced)
        in
        loop children

      let rewrite_instruction b i =
        let rec loop = function
          | [] -> `Unchanged
          | (module Red : S with type t = t) :: rest -> (
            match Red.rewrite_instruction b i with
            | `Unchanged -> loop rest
            | `Replaced (b', i') -> `Replaced (b', i'))
        in
        loop children

      let rewrite_terminator b ~dbg t =
        let rec loop = function
          | [] -> `Unchanged
          | (module Red : S with type t = t) :: rest -> (
            match Red.rewrite_terminator b ~dbg t with
            | `Unchanged -> loop rest
            | `Replaced -> `Replaced)
        in
        loop children
    end
  in
  (module Combined : Reducer)

let run (module Red_ctor : Reducer) (ssa : Ssa.t) : Ssa.t =
  (* Map each old block to the builder for its structurally-copied
     counterpart. *)
  let block_map : B.t Ssa.Block.Tbl.t = Ssa.Block.Tbl.create 64 in
  (* Global [Block_param] substitution table, populated by [visit_block]: for a
     block [blk] in this table, references to [Block_param { block = blk; index
     = i; _ }] resolve through [map_arg] to [subst.(i)]. *)
  let block_param_subst : Ssa.instruction array Ssa.Block.Tbl.t =
    Ssa.Block.Tbl.create 16
  in
  (* For each old block, the array of old param indices kept in the new block
     (in order). [Merge] blocks drop any param with zero usage count ("unused
     phi node"); other block descriptors keep all params since their values come
     from runtime mechanisms (call return, exn bucket, function ABI) or carry no
     params at all. *)
  let kept_params : int array Ssa.Block.Tbl.t = Ssa.Block.Tbl.create 64 in
  let op_map : Ssa.instruction Ssa.InstructionId.Tbl.t =
    Ssa.InstructionId.Tbl.create 256
  in
  let compute_kept (blk : Ssa.block) : int array =
    let n = Array.length blk.params in
    match[@warning "-fragile-match"] blk.desc with
    | Merge ->
      let buf = ref [] in
      for i = n - 1 downto 0 do
        if blk.param_usage_counts.(i) > 0 then buf := i :: !buf
      done;
      Array.of_list !buf
    | Function_start | Branch_target | Call_continuation | Trap_handler ->
      Array.init n Fun.id
  in
  (* Step 1: create a fresh builder for each block. *)
  let entry_builder = B.make ssa.entry.params in
  Ssa.set_label_hint (B.current_block entry_builder) ssa.entry.label_hint;
  Ssa.Block.Tbl.replace block_map ssa.entry entry_builder;
  Ssa.Block.Tbl.replace kept_params ssa.entry (compute_kept ssa.entry);
  List.iter
    (fun (blk : Ssa.block) ->
      if not (Ssa.block_equal blk ssa.entry)
      then (
        (match[@warning "-fragile-match"] blk.desc with
        | Function_start ->
          Misc.fatal_errorf "Ssa_reducer.run: multiple Function_start"
        | Merge | Branch_target | Call_continuation | Trap_handler -> ());
        let kept = compute_kept blk in
        Ssa.Block.Tbl.replace kept_params blk kept;
        let params = Array.map (fun i -> blk.params.(i)) kept in
        let new_builder = B.new_block entry_builder ~params blk.desc in
        (* Carry the [label_hint] forward so the downstream [cfg_of_ssa] reuses
           the aligned labels. *)
        Ssa.set_label_hint (B.current_block new_builder) blk.label_hint;
        Ssa.Block.Tbl.replace block_map blk new_builder))
    ssa.blocks;
  let map_block_impl (old : Ssa.block) : Ssa.block =
    B.current_block (Ssa.Block.Tbl.find block_map old)
  in
  (* Select from [args] the entries at positions whose target params were
     kept. *)
  let filter_args_for (old : Ssa.block) (args : Ssa.instruction array) :
      Ssa.instruction array =
    let kept = Ssa.Block.Tbl.find kept_params old in
    if Array.length kept = Array.length args
    then args
    else Array.map (fun i -> args.(i)) kept
  in
  (* Old index → new index for [block]'s params (post-filtering). *)
  let remap_param_index (block : Ssa.block) (old_index : int) : int =
    let kept = Ssa.Block.Tbl.find kept_params block in
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
  let map_handler (old : Ssa.block option) : Ssa.block option =
    match old with
    | None -> None
    | Some blk -> (
      match Ssa.Block.Tbl.find_opt block_map blk with
      | Some b -> Some (B.current_block b)
      | None -> None)
  in
  let rec map_arg_impl (i : Ssa.instruction) : Ssa.instruction =
    match i with
    | Op { id; _ } -> Ssa.InstructionId.Tbl.find op_map id
    | Block_param { block; index; typ } -> (
      match Ssa.Block.Tbl.find_opt block_param_subst block with
      | Some subst -> map_arg_impl subst.(index)
      | None ->
        Block_param
          { block = map_block_impl block;
            index = remap_param_index block index;
            typ
          })
    | Proj { index; src } -> Ssa.make_proj ~index (map_arg_impl src)
    | Tuple _ | Push_trap _ | Pop_trap _ | Stack_check _ | Name_for_debugger _
      ->
      assert false
  in
  let map_args = Array.map map_arg_impl in
  let rewrite_terminator_impl (t : Ssa.terminator) : Ssa.terminator =
    match t with
    | Pending_construction -> Pending_construction
    | Goto { goto; args } ->
      Goto
        { goto = map_block_impl goto;
          args = map_args (filter_args_for goto args)
        }
    | Branch { cond; ifso; ifnot } ->
      Branch
        { cond = map_arg_impl cond;
          ifso = map_block_impl ifso;
          ifnot = map_block_impl ifnot
        }
    | Switch (targets, args) ->
      Switch (Array.map map_block_impl targets, map_args args)
    | Return args -> Return (map_args args)
    | Raise (k, args, h) ->
      let h = Option.map map_block_impl h in
      Option.iter
        (fun (h : Ssa.block) ->
          assert (Array.length args = Array.length h.params))
        h;
      Raise (k, map_args args, h)
    | Tailcall_self { destination; args } ->
      let destination = map_block_impl destination in
      assert (Array.length args = Array.length destination.params);
      Tailcall_self { destination; args = map_args args }
    | Tailcall_func (op, args) -> Tailcall_func (op, map_args args)
    | Call { op; args; continuation; exn_continuation } ->
      Call
        { op;
          args = map_args args;
          continuation = map_block_impl continuation;
          exn_continuation = Option.map map_block_impl exn_continuation
        }
    | Prim { op; args; continuation; exn_continuation } ->
      Prim
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
     translation helpers. *)
  let module M = struct
    module rec Ctx : (Context with type t = B.t) = struct
      type t = B.t

      let emit_instruction b i =
        match Red.rewrite_instruction b i with
        | `Unchanged -> B.emit_instruction b i
        | `Replaced (_, i') -> i'

      let emit_op b ~op ~dbg ~typ ~args =
        emit_instruction b (Ssa.make_op ~op ~typ ~args ~dbg)

      let finish_block b ~dbg term =
        match Red.rewrite_terminator b ~dbg term with
        | `Unchanged -> B.finish_block b ~dbg term
        | `Replaced -> ()

      let current_block = B.current_block

      let new_block = B.new_block

      let block_params = B.block_params

      let map_arg i = map_arg_impl i

      let map_block blk = map_block_impl blk

      let visit_instruction (blk : Ssa.block) ~instr_index b =
        match Red.visit_instruction blk ~instr_index b with
        | `Replaced -> ()
        | `Unchanged -> (
          let i = blk.body.(instr_index) in
          match[@warning "-fragile-match"] i with
          (* Skip dead Ops: their args may reference [Block_param]s that we've
             dropped (see [compute_kept]), and emitting them would propagate
             those dangling references into the new graph. An Op with
             [usage_count = 0] is by definition not referenced by any live
             consumer. *)
          | Op { usage_count = 0; _ } -> ()
          | _ -> (
            let rewritten : Ssa.instruction =
              match i with
              | Op { op; typ; args; dbg; _ } ->
                Ssa.make_op ~op ~typ ~args:(map_args args) ~dbg
              | Push_trap { handler } ->
                Push_trap { handler = map_handler handler }
              | Pop_trap { handler } ->
                Pop_trap { handler = map_handler handler }
              | Stack_check _ -> i
              | Name_for_debugger { ident; provenance; which_parameter; regs }
                ->
                Name_for_debugger
                  { ident; provenance; which_parameter; regs = map_args regs }
              | Block_param _ | Proj _ | Tuple _ -> assert false
            in
            let new_i = emit_instruction b rewritten in
            match[@warning "-fragile-match"] i with
            | Op { id; _ } -> Ssa.InstructionId.Tbl.replace op_map id new_i
            | _ -> ()))

      let visit_terminator (blk : Ssa.block) b =
        match Red.visit_terminator blk b with
        | `Replaced -> ()
        | `Unchanged ->
          let term = rewrite_terminator_impl blk.terminator in
          finish_block b ~dbg:blk.terminator_dbg term

      let visit_block (blk : Ssa.block) ~block_args b =
        match Red.visit_block blk b with
        | `Replaced -> ()
        | `Unchanged ->
          Ssa.Block.Tbl.replace block_param_subst blk block_args;
          Array.iteri
            (fun instr_index _ -> visit_instruction blk ~instr_index b)
            blk.body;
          visit_terminator blk b
    end

    and Red : (S with type t = B.t) = Red_ctor (Ctx)
  end in
  let module Red = M.Red in
  let module Ctx = M.Ctx in
  Red.analyze ssa;
  (* Step 2: walk the input graph in a priority order over unvisited blocks
     [(priority, input_position)] (smaller is better):
       priority 0 — all predecessors already visited (canonical topological);
       priority 1 — block is a transitive predecessor of an already-visited
                   block (closes an existing "open" structure);
       priority 2 — neither.
     [input_position] breaks ties by input list order.

     The two state pieces are maintained incrementally:
     - [unvisited_pred_count] starts at the predecessor count and is decremented
       when a predecessor is popped from the queue (whether or not it ends up
       being emitted standalone). The entry's count starts at 0 even if it has
       back-edge predecessors, so it's always priority 0 initially.
     - [reaches_visited] is set when, after visiting a block [v] that itself
       had unvisited predecessors, we walk backward from [v]'s preds, marking
       each previously-unmarked-and-unvisited ancestor and stopping at visited
       or already-marked ones. (When [v] is visited with all preds already
       visited, no propagation is needed: every ancestor reaches some visited
       pred of [v] through a path whose first visited node was already
       processed with unvisited preds and thus already triggered the
       propagation.)

     Before emitting, we still consult [should_process]: a block whose output
     has been spliced in by an inlining [visit_block] (e.g., [Inline_merge])
     must not be emitted standalone, or we'd create a second copy of its body
     that pollutes [op_map] and ends up filtered as unreachable. We always
     decrement successor counts on pop, even when skipping, so blocks
     downstream of an inlined block can still reach priority 0. *)
  let position : int Ssa.Block.Tbl.t = Ssa.Block.Tbl.create 64 in
  List.iteri
    (fun pos blk -> Ssa.Block.Tbl.replace position blk pos)
    ssa.blocks;
  let unvisited_pred_count : int Ssa.Block.Tbl.t = Ssa.Block.Tbl.create 64 in
  List.iter
    (fun (blk : Ssa.block) ->
      let n =
        if Ssa.block_equal blk ssa.entry
        then 0
        else List.length (Ssa.predecessors blk)
      in
      Ssa.Block.Tbl.replace unvisited_pred_count blk n)
    ssa.blocks;
  let reaches_visited : unit Ssa.Block.Tbl.t = Ssa.Block.Tbl.create 64 in
  let priority_of (blk : Ssa.block) =
    if Ssa.Block.Tbl.find unvisited_pred_count blk = 0
    then 0
    else if Ssa.Block.Tbl.mem reaches_visited blk
    then 1
    else 2
  in
  let module Q = Set.Make (struct
    type t = int * int * Ssa.block
    let compare (p1, i1, b1) (p2, i2, b2) =
      match Int.compare p1 p2 with
      | 0 -> (
        match Int.compare i1 i2 with 0 -> Ssa.Block.compare b1 b2 | n -> n)
      | n -> n
  end) in
  let queue = ref Q.empty in
  let entry_of (blk : Ssa.block) =
    priority_of blk, Ssa.Block.Tbl.find position blk, blk
  in
  List.iter (fun blk -> queue := Q.add (entry_of blk) !queue) ssa.blocks;
  let bump_priority (blk : Ssa.block) ~old_priority =
    let pos = Ssa.Block.Tbl.find position blk in
    let old_key = old_priority, pos, blk in
    if Q.mem old_key !queue
    then begin
      let new_prio = priority_of blk in
      if new_prio < old_priority
      then begin
        queue := Q.remove old_key !queue;
        queue := Q.add (new_prio, pos, blk) !queue
      end
    end
  in
  let should_process (blk : Ssa.block) =
    let out = B.current_block (Ssa.Block.Tbl.find block_map blk) in
    (not (Ssa.block_finished out))
    && (Ssa.block_equal blk ssa.entry || Ssa.predecessors out <> [])
  in
  while not (Q.is_empty !queue) do
    let ((_, _, blk) as item) = Q.min_elt !queue in
    queue := Q.remove item !queue;
    let visited_with_unvisited_preds =
      if should_process blk
      then begin
        let had_unvisited_preds =
          Ssa.Block.Tbl.find unvisited_pred_count blk > 0
        in
        let b = Ssa.Block.Tbl.find block_map blk in
        (try
           match Red.visit_block blk b with
           | `Replaced -> ()
           | `Unchanged ->
             Array.iteri
               (fun instr_index _ ->
                 Ctx.visit_instruction blk ~instr_index b)
               blk.body;
             Ctx.visit_terminator blk b
         with exn ->
           let bt = Printexc.get_raw_backtrace () in
           Format.eprintf
             "*** Ssa_reducer.run error for %s while processing block %a: %s@."
             ssa.fun_name Ssa_print.print_block_id blk
             (Printexc.to_string exn);
           Format.eprintf "*** Old SSA:@.%a@." Ssa_print.print ssa;
           let finished_so_far = B.finish entry_builder in
           Format.eprintf "*** New SSA so far (%d finished block(s)):@.%a@."
             (List.length finished_so_far)
             (Format.pp_print_list
                ~pp_sep:(fun _ () -> ())
                Ssa_print.print_block)
             finished_so_far;
           Format.eprintf
             "*** Under-construction block (body not yet flushed):\n%a"
             Ssa_print.print_block (B.current_block b);
           Format.pp_print_flush Format.err_formatter ();
           Printexc.raise_with_backtrace exn bt);
        had_unvisited_preds
      end
      else false
    in
    List.iter
      (fun (succ : Ssa.block) ->
        match Ssa.Block.Tbl.find_opt unvisited_pred_count succ with
        | None -> ()
        | Some cnt ->
          let old_prio = priority_of succ in
          Ssa.Block.Tbl.replace unvisited_pred_count succ (cnt - 1);
          bump_priority succ ~old_priority:old_prio)
      (Ssa.successors blk);
    if visited_with_unvisited_preds
    then begin
      let rec walk (b : Ssa.block) =
        List.iter
          (fun (pred : Ssa.block) ->
            if (not (Ssa.Block.Tbl.mem reaches_visited pred))
               && Ssa.Block.Tbl.mem position pred
               && Q.mem
                    ( priority_of pred,
                      Ssa.Block.Tbl.find position pred,
                      pred )
                    !queue
            then begin
              let old_prio = priority_of pred in
              Ssa.Block.Tbl.add reaches_visited pred ();
              bump_priority pred ~old_priority:old_prio;
              walk pred
            end)
          (Ssa.predecessors b)
      in
      walk blk
    end
  done;
  let result =
    { Ssa.blocks = B.finish entry_builder;
      fun_name = ssa.fun_name;
      fun_args = ssa.fun_args;
      fun_args_names = ssa.fun_args_names;
      fun_codegen_options = ssa.fun_codegen_options;
      fun_dbg = ssa.fun_dbg;
      entry = map_block_impl ssa.entry;
      fun_poll = ssa.fun_poll;
      fun_ret_type = ssa.fun_ret_type
    }
  in
  (try Ssa_validate.validate result
   with exn ->
     let bt = Printexc.get_raw_backtrace () in
     Format.eprintf
       "*** Ssa_reducer.run: validation failed for %s: %s@.*** Old SSA:@.%a@.*** New SSA:@.%a@."
       ssa.fun_name (Printexc.to_string exn) Ssa_print.print ssa Ssa_print.print
       result;
     Format.pp_print_flush Format.err_formatter ();
     Printexc.raise_with_backtrace exn bt);
  result
