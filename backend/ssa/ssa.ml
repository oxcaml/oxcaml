open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-30-40-41-42-67"]

(** SSA graph implementation; signatures are in {!Ssa_intf}.

    A single underlying module satisfies both [Graph_builder] and
    [Finished_graph]. During construction the abstract types in [Graph_builder]
    hide use counts, predecessors and dominator info; at [finish] time the same
    module is repackaged with the [Finished_graph] view that exposes them.

    Construction is mostly functional: each [emit_*] returns a new
    [unfinished_block] cursor that carries the appended instruction list; blocks
    themselves are mutated only by [finish_block] (which seals the body and
    terminator) and by [finish] (which fills in metadata).

    [finish] performs three passes over the finished blocks, in order:
    - [compute_reachability_and_trap_stacks]: forward DFS from [entry] following
      structural and exception successors, threading the trap stack so each
      reachable block gets its [block_end_trap_stack] populated; unreachable
      blocks are pruned.
    - [compute_dominators]: iterative meet-over-predecessors fixpoint that walks
      reachable blocks until no [dominator_info] changes.
    - [compute_use_counts]: refcount over op args and block params; the latter
      propagate to predecessors' [Goto] args, so a transitively-unused arg keeps
      its defining op count at zero.

    Invariants enforced here:
    - Every reachable block is [finish_block]ed.
    - [Tuple] never appears in a finished body, only transiently as an
      [Ssa_reducer] instruction representative.
    - [Block_param], [Proj] never appear in a block body.
    - [Block_param.index] is bounds-checked by [make_block_param]. *)

include Ssa_intf

let make_builder (function_info : function_info) : (module Graph_builder) =
  let module M = struct
    module Instruction_id = Oxcaml_utils.Id_counter.Make ()
    module Block_id = Oxcaml_utils.Id_counter.Make ()

    type op = Operation.t

    type usage_count = int

    module rec Block : sig
      type dominator_info =
        { depth : int;
          dominator : Block.t
        }

      and t =
        { id : Block_id.t;
          is_function_start : bool;
          params : block_param array;
          mutable predecessors : Block.Set.t;
          mutable body : Instruction.t array;
          mutable terminator : Terminator.t;
          mutable terminator_dbg : Debuginfo.t;
          mutable dominator_info : dominator_info;
          mutable param_usage_counts : usage_count array;
          mutable block_end_trap_stack : Block.t list
        }

      val id : Block.t -> Block_id.t

      val is_function_start : Block.t -> bool

      val params : Block.t -> block_param array

      val equal : Block.t -> Block.t -> bool

      val compare : Block.t -> Block.t -> int

      val hash : Block.t -> int

      module Map : Map.S with type key = Block.t

      module Set : Set.S with type elt = Block.t

      module Tbl : Hashtbl.S with type key = Block.t
    end = struct
      type dominator_info =
        { depth : int;
          dominator : Block.t
        }

      and t =
        { id : Block_id.t;
          is_function_start : bool;
          params : block_param array;
          mutable predecessors : Block.Set.t;
          mutable body : Instruction.t array;
          mutable terminator : Terminator.t;
          mutable terminator_dbg : Debuginfo.t;
          mutable dominator_info : dominator_info;
          mutable param_usage_counts : usage_count array;
          mutable block_end_trap_stack : Block.t list
        }

      let id b = b.id

      let is_function_start b = b.is_function_start

      let params b = b.params

      let equal a b = Block_id.equal a.id b.id

      let compare a b = Block_id.compare a.id b.id

      let hash b = Block_id.hash b.id

      module Self = struct
        type t = Block.t

        let equal = equal

        let compare = compare

        let hash = hash
      end

      module Map = Map.Make (Self)
      module Set = Set.Make (Self)
      module Tbl = Hashtbl.Make (Self)
    end

    and Instruction : sig
      type t =
        | Op of op_data
        | Block_param of block_param_data
        | Proj of proj_data
        | Tuple of Instruction.t array
        | Push_trap of { handler : Block.t }
        | Pop_trap of { handler : Block.t }
        | Stack_check of { max_frame_size_bytes : int }
        | Name_for_debugger of
            { ident : Ident.t;
              provenance : Backend_var.Provenance.t option;
              which_parameter : int option;
              regs : Instruction.t array
            }

      and op_data =
        { id : Instruction_id.t;
          op : op;
          typ : Cmm.machtype;
          args : Instruction.t array;
          dbg : Debuginfo.t;
          mutable usage_count : usage_count;
          mutable name : string option
        }

      and block_param_data =
        { block : Block.t;
          index : int
        }

      and proj_data =
        { index : int;
          src : Instruction.t
        }

      val equal : Instruction.t -> Instruction.t -> bool

      val make_op :
        op:op ->
        typ:Cmm.machtype ->
        args:t array ->
        dbg:Debuginfo.t ->
        Instruction.t

      val set_name : Instruction.t -> string -> unit

      val make_block_param : Block.t -> int -> Instruction.t

      val make_proj : index:int -> Instruction.t -> Instruction.t

      val arg_type : Instruction.t -> Cmm.machtype_component
    end = struct
      type t =
        | Op of op_data
        | Block_param of block_param_data
        | Proj of proj_data
        | Tuple of Instruction.t array
        | Push_trap of { handler : Block.t }
        | Pop_trap of { handler : Block.t }
        | Stack_check of { max_frame_size_bytes : int }
        | Name_for_debugger of
            { ident : Ident.t;
              provenance : Backend_var.Provenance.t option;
              which_parameter : int option;
              regs : Instruction.t array
            }

      and op_data =
        { id : Instruction_id.t;
          op : op;
          typ : Cmm.machtype;
          args : Instruction.t array;
          dbg : Debuginfo.t;
          mutable usage_count : usage_count;
          mutable name : string option
        }

      and block_param_data =
        { block : Block.t;
          index : int
        }

      and proj_data =
        { index : int;
          src : Instruction.t
        }

      let equal (a : Instruction.t) (b : Instruction.t) =
        match[@warning "-4"] a, b with
        | Op a, Op b -> Instruction_id.equal a.id b.id
        | _ -> a == b

      let arg_type (instr : Instruction.t) : Cmm.machtype_component =
        match instr with
        | Op { typ = [| t |]; _ } -> t
        | Op { typ; _ } ->
          Misc.fatal_errorf
            "Ssa.Instruction.arg_type: Op has %d-component type; project first"
            (Array.length typ)
        | Block_param { block; index } -> block.params.(index).typ
        | Proj { index; src = Op { typ; _ } } -> typ.(index)
        | Proj _ | Tuple _ | Push_trap _ | Pop_trap _ | Stack_check _
        | Name_for_debugger _ ->
          Misc.fatal_error "Ssa.Instruction.arg_type: not a valid argument"

      let make_op ~op ~typ ~args ~dbg =
        Op
          { id = Instruction_id.create ();
            op;
            typ;
            args;
            dbg;
            usage_count = 0;
            name = None
          }

      let set_name (instr : Instruction.t) name =
        match[@warning "-4"] instr with
        | Op r -> r.name <- Some name
        | Block_param { block; index } -> block.params.(index).name <- Some name
        | _ -> ()

      let make_block_param (block : Block.t) (index : int) =
        if index < 0 || index >= Array.length block.params
        then
          Misc.fatal_errorf
            "Ssa.Instruction.make_block_param: index %d out of range for block \
             with %d params"
            index
            (Array.length block.params);
        Block_param { block; index }

      let make_proj ~index src =
        match src with
        | Tuple elems -> Array.get elems index
        | Op _ -> Proj { index; src }
        | Block_param _ | Proj _ | Push_trap _ | Pop_trap _ | Stack_check _
        | Name_for_debugger _ ->
          Misc.fatal_error
            "Ssa.Instruction.make_proj: cannot project from a \
             non-value-producing instruction"
    end

    and Terminator : sig
      type t =
        | Goto of
            { goto : Block.t;
              args : Instruction.t array
            }
        | Branch of
            { cond : Instruction.t;
              ifso : Block.t;
              ifnot : Block.t
            }
        | Switch of
            { index : Instruction.t;
              targets : Block.t array
            }
        | Return of { args : Instruction.t array }
        | Raise of
            { raise_kind : Lambda.raise_kind;
              args : Instruction.t array
            }
        | Tailcall_self of
            { destination : Block.t;
              args : Instruction.t array
            }
        | Tailcall_func of
            { op : Cfg_intf.S.func_call_operation;
              args : Instruction.t array
            }
        | Call of
            { op : call_op;
              args : Instruction.t array;
              continuation : Block.t;
              may_raise : bool;
              nontail : bool
            }
        | Invalid of
            { message : string;
              args : Instruction.t array;
              continuation : Block.t option
            }

      val non_trap_successors : Terminator.t -> Block.t list
    end = struct
      type t =
        | Goto of
            { goto : Block.t;
              args : Instruction.t array
            }
        | Branch of
            { cond : Instruction.t;
              ifso : Block.t;
              ifnot : Block.t
            }
        | Switch of
            { index : Instruction.t;
              targets : Block.t array
            }
        | Return of { args : Instruction.t array }
        | Raise of
            { raise_kind : Lambda.raise_kind;
              args : Instruction.t array
            }
        | Tailcall_self of
            { destination : Block.t;
              args : Instruction.t array
            }
        | Tailcall_func of
            { op : Cfg_intf.S.func_call_operation;
              args : Instruction.t array
            }
        | Call of
            { op : call_op;
              args : Instruction.t array;
              continuation : Block.t;
              may_raise : bool;
              nontail : bool
            }
        | Invalid of
            { message : string;
              args : Instruction.t array;
              continuation : Block.t option
            }

      (* The terminator is missing the trap successors, which are derived from
         [block_end_trap_stack]. *)
      let non_trap_successors (t : Terminator.t) : Block.t list =
        match t with
        | Return _ | Tailcall_func _ | Raise _ -> []
        | Goto { goto; _ } -> [goto]
        | Branch { ifso; ifnot; _ } -> [ifso; ifnot]
        | Switch { targets; _ } -> Array.to_list targets
        | Tailcall_self { destination; _ } -> [destination]
        | Call { continuation; _ } -> [continuation]
        | Invalid { continuation; _ } -> (
          match continuation with Some l -> [l] | None -> [])
    end

    (* === Builder state === *)

    type cursor =
      { mutable block : Block.t;
        mutable instrs_rev : Instruction.t list
      }

    type new_block_result =
      { block : Block.t;
        params : Instruction.t array
      }

    let function_info : function_info = function_info

    (* Finished blocks, in reverse [finish_block] order. Populated by
       [finish_block]; unfinished or never-finished blocks do not appear
       here. *)
    let finished_blocks : Block.t list ref = ref []

    (* Sentinel terminator used as the initial value of the [terminator] field.
       [finish_block] checks (via physical equality) that the field still holds
       this sentinel before sealing — i.e., that [finish_block] hasn't already
       been called for this block. *)
    let rec pending_terminator : Terminator.t =
      Terminator.Goto { goto = dummy_block; args = [||] }

    and dummy_block : Block.t =
      { id = Block_id.create ();
        is_function_start = false;
        params = [||];
        predecessors = Block.Set.empty;
        body = [||];
        terminator = pending_terminator;
        terminator_dbg = Debuginfo.none;
        dominator_info = { depth = -1; dominator = dummy_block };
        param_usage_counts = [||];
        block_end_trap_stack = []
      }

    let create_block ~is_function_start ~params : Block.t =
      { id = Block_id.create ();
        is_function_start;
        params;
        predecessors = Block.Set.empty;
        body = [||];
        terminator = pending_terminator;
        terminator_dbg = Debuginfo.none;
        dominator_info = { depth = -1; dominator = dummy_block };
        param_usage_counts = [||];
        block_end_trap_stack = []
      }

    let make_block_result ~is_function_start ~(params : Cmm.machtype) :
        new_block_result =
      let block_params : Ssa_intf.block_param array =
        Array.map
          (fun typ : Ssa_intf.block_param -> { typ; name = None })
          params
      in
      let block = create_block ~is_function_start ~params:block_params in
      let params_arr =
        Array.init (Array.length params) (fun i ->
            Instruction.make_block_param block i)
      in
      { block; params = params_arr }

    let new_block ~params = make_block_result ~is_function_start:false ~params

    let entry, entry_params =
      let { block; params } =
        make_block_result ~is_function_start:true ~params:function_info.fun_args
      in
      (* Initialize names from the function's argument names, when known. *)
      List.iteri
        (fun i (var, _ty) ->
          if i < Array.length block.params
          then
            block.params.(i).name
              <- Some (Backend_var.name (Backend_var.With_provenance.var var)))
        function_info.fun_args_names;
      block, params

    (* === Builder operations: mutate the cursor in place === *)

    let start_block (blk : Block.t) : cursor = { block = blk; instrs_rev = [] }

    let move_cursor (c : cursor) ~(new_pos : cursor) : unit =
      c.block <- new_pos.block;
      c.instrs_rev <- new_pos.instrs_rev

    let emit_instruction (c : cursor) (i : Instruction.t) =
      (match i with
      | Instruction.Block_param _ | Instruction.Proj _ | Instruction.Tuple _ ->
        Misc.fatal_errorf
          "Ssa.emit_instruction: %s is a virtual value and cannot be emitted \
           into a block body"
          (match[@warning "-fragile-match"] i with
          | Instruction.Block_param _ -> "Block_param"
          | Instruction.Proj _ -> "Proj"
          | Instruction.Tuple _ -> "Tuple"
          | _ -> assert false)
      | Instruction.Op _ | Instruction.Push_trap _ | Instruction.Pop_trap _
      | Instruction.Stack_check _ | Instruction.Name_for_debugger _ ->
        ());
      c.instrs_rev <- i :: c.instrs_rev

    let emit_op (c : cursor) ~op ~dbg ~typ ~args : Instruction.t =
      let instr = Instruction.make_op ~op ~typ ~args ~dbg in
      emit_instruction c instr;
      instr

    let check_args_arity ~term_name ~args ~expected =
      if Array.length args <> Array.length expected
      then
        Misc.fatal_errorf "Ssa.finish_block: %s passes %d args but expected %d"
          term_name (Array.length args) (Array.length expected)

    let check_target_has_no_params ~term_name (target : Block.t) =
      if Array.length target.params > 0
      then
        Misc.fatal_errorf "Ssa.finish_block: %s target must have no params"
          term_name

    let check_terminator (term : Terminator.t) : unit =
      match term with
      | Goto { goto; args } ->
        check_args_arity ~term_name:"Goto" ~args ~expected:goto.params
      | Tailcall_self { destination; args } ->
        check_args_arity ~term_name:"Tailcall_self" ~args
          ~expected:destination.params
      | Branch { ifso; ifnot; _ } ->
        check_target_has_no_params ~term_name:"Branch" ifso;
        check_target_has_no_params ~term_name:"Branch" ifnot
      | Switch { targets; _ } ->
        Array.iter (check_target_has_no_params ~term_name:"Switch") targets
      | Return _ | Raise _ | Tailcall_func _ | Call _ | Invalid _ -> ()

    let finish_block (c : cursor) ~(dbg : Debuginfo.t) (term : Terminator.t) :
        unit =
      let blk = c.block in
      if blk.terminator != pending_terminator
      then Misc.fatal_error "Ssa.finish_block: block already finished";
      check_terminator term;
      blk.body <- Array.of_list (List.rev c.instrs_rev);
      blk.terminator <- term;
      blk.terminator_dbg <- dbg;
      finished_blocks := blk :: !finished_blocks;
      c.instrs_rev <- []

    (* === Predecessors / dominators === *)

    let predecessors (blk : Block.t) : Block.Set.t = blk.predecessors

    let params_machtype (blk : Block.t) : Cmm.machtype =
      Array.map (fun (p : Ssa_intf.block_param) -> p.typ) blk.params

    (* The implicit trap successor of [blk]: the topmost handler in
       [block_end_trap_stack], if the terminator can raise. *)
    let trap_successor (blk : Block.t) : Block.t option =
      let raises =
        match blk.terminator with
        | Raise _ -> true
        | Call { may_raise; _ } -> may_raise
        | Goto _ | Branch _ | Switch _ | Return _ | Tailcall_self _
        | Tailcall_func _ | Invalid _ ->
          false
      in
      if raises
      then match blk.block_end_trap_stack with [] -> None | h :: _ -> Some h
      else None

    let successors (blk : Block.t) : Block.t list =
      let structural = Terminator.non_trap_successors blk.terminator in
      match trap_successor blk with
      | None -> structural
      | Some h -> structural @ [h]

    let rec dominates (a : Block.t) (b : Block.t) =
      Block.equal a b
      || b.dominator_info.depth > a.dominator_info.depth
         && dominates a b.dominator_info.dominator

    let rec common_dominator (a : Block.t) (b : Block.t) : Block.t =
      if Block.equal a b
      then a
      else if a.dominator_info.depth > b.dominator_info.depth
      then common_dominator a.dominator_info.dominator b
      else if b.dominator_info.depth > a.dominator_info.depth
      then common_dominator a b.dominator_info.dominator
      else
        common_dominator a.dominator_info.dominator b.dominator_info.dominator

    (* For a predecessor's terminator, return the argument at [index] that is
       passed through an unconditional jump to a successor whose params may be
       dropped. Only [Goto] targets such successors. *)
    let block_param_arg (term : Terminator.t) (index : int) :
        Instruction.t option =
      match term with
      | Goto { args; _ } -> Some (Array.get args index)
      | Branch _ | Switch _ | Return _ | Raise _ | Tailcall_self _
      | Tailcall_func _ | Call _ | Invalid _ ->
        None

    (* === Compute metadata: predecessors, traps, dominators, use counts === *)

    (* Apply the [Push_trap]/[Pop_trap] effects of [body] to [start_stack]. Each
       [Pop_trap { handler = h }] must find [h] at the top of the current
       stack. *)
    let apply_body_trap_effects ~(blk : Block.t) (start_stack : Block.t list)
        (body : Instruction.t array) : Block.t list =
      Array.fold_left
        (fun (stack : Block.t list) (instr : Instruction.t) ->
          match instr with
          | Push_trap { handler = h } -> h :: stack
          | Pop_trap { handler = h } -> (
            match stack with
            | [] ->
              Misc.fatal_errorf
                "Ssa.finish: block B%d pops handler B%d off an empty trap stack"
                (blk.id :> int)
                (h.id :> int)
            | top :: rest ->
              if not (Block.equal top h)
              then
                Misc.fatal_errorf
                  "Ssa.finish: block B%d pops handler B%d but top of trap \
                   stack is B%d"
                  (blk.id :> int)
                  (h.id :> int)
                  (top.id :> int);
              rest)
          | Op _ | Block_param _ | Proj _ | Tuple _ | Stack_check _
          | Name_for_debugger _ ->
            stack)
        start_stack body

    (* Forward search from [entry]: populates [predecessors] on each reachable
       block, computes [block_end_trap_stack] for each visited block (also
       needed to derive trap successors), and verifies that every reachable
       block has been finished. *)
    let compute_reachability_and_trap_stacks ~(entry : Block.t)
        ~(finished_blocks : Block.t list) : Block.t list =
      let visited = Block.Tbl.create 64 in
      let worklist = ref [entry, []] in
      while not (List.is_empty !worklist) do
        let block, start_stack = List.hd !worklist in
        worklist := List.tl !worklist;
        if not (Block.Tbl.mem visited block)
        then begin
          Block.Tbl.add visited block ();
          let end_stack =
            apply_body_trap_effects ~blk:block start_stack block.body
          in
          block.block_end_trap_stack <- end_stack;
          let add_pred start_stack (succ : Block.t) =
            succ.predecessors <- Block.Set.add block succ.predecessors;
            worklist := (succ, start_stack) :: !worklist
          in
          Terminator.non_trap_successors block.terminator
          |> List.iter (fun succ -> add_pred end_stack succ);
          (* On entry to a trap handler, the runtime has popped the topmost
             handler off the trap stack. *)
          trap_successor block
          |> Option.iter (fun succ -> add_pred (List.tl end_stack) succ)
        end
      done;
      let reachable_blocks =
        List.filter (fun blk -> Block.Tbl.mem visited blk) finished_blocks
      in
      let unfinished_blocks =
        Block.Set.diff
          (Block.Tbl.to_seq_keys visited |> Block.Set.of_seq)
          (Block.Set.of_list finished_blocks)
      in
      unfinished_blocks
      |> Block.Set.iter (fun unfinished ->
          Misc.fatal_errorf "Ssa.finish: reachable block B%d was never finished"
            (unfinished.id :> int));
      reachable_blocks

    let compute_dominators ~(entry : Block.t) ~(reachable_blocks : Block.t list)
        : unit =
      entry.dominator_info <- { depth = 0; dominator = entry };
      let has_idom (blk : Block.t) = blk.dominator_info.depth >= 0 in
      let changed = ref true in
      while !changed do
        changed := false;
        List.iter
          (fun (blk : Block.t) ->
            if not (Block.equal blk entry)
            then begin
              let new_idom =
                Block.Set.fold
                  (fun pred acc ->
                    if has_idom pred
                    then
                      match acc with
                      | None -> Some pred
                      | Some current -> Some (common_dominator pred current)
                    else acc)
                  blk.predecessors
                  (if has_idom blk
                   then Some blk.dominator_info.dominator
                   else None)
              in
              match new_idom with
              | None -> ()
              | Some idom ->
                let new_info : Block.dominator_info =
                  { depth = idom.dominator_info.depth + 1; dominator = idom }
                in
                if blk.dominator_info.depth != new_info.depth
                then begin
                  blk.dominator_info <- new_info;
                  changed := true
                end
                else
                  assert (
                    Block.equal blk.dominator_info.dominator new_info.dominator)
            end)
          reachable_blocks
      done

    (* Reference-counting walk over each reachable block's body and terminator.
       [Block_param] increments propagate to predecessors' [Goto] args via
       [block_param_arg]. Per-Op counts mutate [op_data.usage_count]; per-block
       param counts live in a side table and are written back to
       [param_usage_counts] at the end. *)
    let compute_use_counts ~(reachable_blocks : Block.t list) : unit =
      let param_counts : int array Block.Tbl.t = Block.Tbl.create 64 in
      List.iter
        (fun (blk : Block.t) ->
          Block.Tbl.replace param_counts blk
            (Array.make (Array.length blk.params) 0))
        reachable_blocks;
      let rec increment_use (i : Instruction.t) =
        match i with
        | Op r ->
          r.usage_count <- r.usage_count + 1;
          if r.usage_count = 1 then Array.iter increment_use r.args
        | Proj { src; _ } -> increment_use src
        | Block_param { block; index; _ } ->
          let counts = Block.Tbl.find param_counts block in
          let old = counts.(index) in
          counts.(index) <- old + 1;
          if old = 0
          then
            Block.Set.iter
              (fun (pred : Block.t) ->
                match block_param_arg pred.terminator index with
                | Some arg -> increment_use arg
                | None -> ())
              block.predecessors
        | Tuple _ ->
          Misc.fatal_error
            "Ssa.increment_use: Tuple should have been short-circuited by proj"
        | Push_trap _ | Pop_trap _ | Stack_check _ | Name_for_debugger _ -> ()
      in
      let increment_uses_in_terminator (term : Terminator.t) =
        let incr_all arr = Array.iter increment_use arr in
        match term with
        | Goto _ -> ()
        | Branch { cond; _ } -> increment_use cond
        | Switch { index; _ } -> increment_use index
        | Return { args }
        | Raise { args; _ }
        | Tailcall_self { args; _ }
        | Tailcall_func { args; _ }
        | Call { args; _ }
        | Invalid { args; _ } ->
          incr_all args
      in
      List.iter
        (fun (blk : Block.t) ->
          Array.iter
            (fun (i : Instruction.t) ->
              match i with
              | Op r when not (Operation.is_pure r.op) -> increment_use i
              | Name_for_debugger { regs; _ } -> Array.iter increment_use regs
              | Op _ | Push_trap _ | Pop_trap _ | Stack_check _ -> ()
              | Block_param _ | Proj _ | Tuple _ ->
                Misc.fatal_error "impossible body instruction")
            blk.body;
          increment_uses_in_terminator blk.terminator)
        reachable_blocks;
      List.iter
        (fun (blk : Block.t) ->
          blk.param_usage_counts <- Block.Tbl.find param_counts blk)
        reachable_blocks

    (* Walk [blocks] in the input order, but when first visiting a block emit
       its dominator chain ahead of it. The result has every block preceded by
       its dominators, while keeping the input order wherever the dominator
       constraint doesn't force a move. Requires [dominator_info] to already be
       populated. *)
    let order_blocks_dominators_first (blocks : Block.t list) : Block.t list =
      let visited = Block.Tbl.create 64 in
      let acc = ref [] in
      let rec visit (blk : Block.t) =
        if not (Block.Tbl.mem visited blk)
        then begin
          Block.Tbl.add visited blk ();
          let dom = blk.dominator_info.dominator in
          if not (Block.equal dom blk) then visit dom;
          acc := blk :: !acc
        end
      in
      List.iter visit blocks;
      List.rev !acc

    let compute_metadata ~(entry : Block.t) ~(finished_blocks : Block.t list) :
        Block.t list =
      let reachable_blocks : Block.t list =
        compute_reachability_and_trap_stacks ~entry ~finished_blocks
      in
      compute_dominators ~entry ~reachable_blocks;
      compute_use_counts ~reachable_blocks;
      order_blocks_dominators_first reachable_blocks

    let finish () : (module Finished_graph) =
      let all = List.rev !finished_blocks in
      let reachable = compute_metadata ~entry ~finished_blocks:all in
      let module Finished = struct
        module Instruction_id = Instruction_id
        module Block_id = Block_id

        type op = Operation.t

        type usage_count = int

        module Block = Block
        module Instruction = Instruction
        module Terminator = Terminator

        let function_info = function_info

        let entry = entry

        let blocks = reachable

        let predecessors = predecessors

        let params_machtype = params_machtype

        let successors = successors

        let trap_successor = trap_successor

        let dominates = dominates

        let common_dominator = common_dominator
      end in
      let result = (module Finished : Finished_graph) in
      result
  end in
  (module M : Graph_builder)
