[@@@ocaml.warning "+a-30-40-41-42-67"]

include Ssa_intf

(* ========================================================================
   Implementation: a single underlying module satisfies both
   [Standalone_graph_builder] and [Finished_graph]. During construction the
   abstract types in [Graph_builder] hide the use counts, predecessors and
   dominator info; at [finish] time the same module is repackaged with the
   [Finished_graph] view that exposes them. *)

let make_builder (function_info : function_info) :
    (module Standalone_graph_builder) =
  let module M = struct
    module Instruction_id = Oxcaml_utils.Id_counter.Make ()
    module Block_id = Oxcaml_utils.Id_counter.Make ()

    type op = Operation.t

    type usage_count = int

    module rec Block : sig
      type predecessors = Block_set.t

      type terminator = Terminator.t

      type dominator_info =
        { depth : int;
          dominator : t
        }

      and t =
        { id : Block_id.t;
          is_function_start : bool;
          params : Cmm.machtype;
          mutable predecessors : predecessors;
          mutable body : Instruction.t array;
          mutable terminator : Terminator.t;
          mutable terminator_dbg : Debuginfo.t;
          mutable dominator_info : dominator_info;
          mutable label_hint : Label.t option;
          mutable param_usage_counts : usage_count array
        }

      val id : t -> Block_id.t

      val is_function_start : t -> bool

      val params : t -> Cmm.machtype

      val equal : t -> t -> bool

      val compare : t -> t -> int

      val hash : t -> int

      val set_label_hint : t -> Label.t option -> unit

      module Map : Map.S with type key = t

      module Set : Set.S with type elt = t

      module Tbl : Hashtbl.S with type key = t
    end = struct
      type predecessors = Block_set.t

      type terminator = Terminator.t

      type dominator_info =
        { depth : int;
          dominator : t
        }

      and t =
        { id : Block_id.t;
          is_function_start : bool;
          params : Cmm.machtype;
          mutable predecessors : predecessors;
          mutable body : Instruction.t array;
          mutable terminator : Terminator.t;
          mutable terminator_dbg : Debuginfo.t;
          mutable dominator_info : dominator_info;
          mutable label_hint : Label.t option;
          mutable param_usage_counts : usage_count array
        }

      let id b = b.id

      let is_function_start b = b.is_function_start

      let params b = b.params

      let equal a b = Block_id.equal a.id b.id

      let compare a b = Block_id.compare a.id b.id

      let hash b = Block_id.hash b.id

      let set_label_hint b hint = b.label_hint <- hint

      module Self = struct
        type nonrec t = t

        let equal = equal

        let compare = compare

        let hash = hash
      end

      module Map = Map.Make (Self)
      module Set = Set.Make (Self)
      module Tbl = Hashtbl.Make (Self)
    end

    and Block_set : (Set.S with type elt = Block.t) = Block.Set

    and Instruction : sig
      type t =
        | Op of op_data
        | Block_param of block_param_data
        | Proj of proj_data
        | Tuple of Instruction.t array
        | Push_trap of { handler : Block.t option }
        | Pop_trap of { handler : Block.t option }
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
          mutable usage_count : usage_count
        }

      and block_param_data =
        { block : Block.t;
          index : int
        }

      and proj_data =
        { index : int;
          src : Instruction.t
        }

      val equal : t -> t -> bool

      val make_op :
        op:op -> typ:Cmm.machtype -> args:t array -> dbg:Debuginfo.t -> t

      val make_block_param : Block.t -> int -> t

      val make_proj : index:int -> t -> t

      val arg_type : t -> Cmm.machtype_component
    end = struct
      type t =
        | Op of op_data
        | Block_param of block_param_data
        | Proj of proj_data
        | Tuple of Instruction.t array
        | Push_trap of { handler : Block.t option }
        | Pop_trap of { handler : Block.t option }
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
          mutable usage_count : usage_count
        }

      and block_param_data =
        { block : Block.t;
          index : int
        }

      and proj_data =
        { index : int;
          src : Instruction.t
        }

      let equal (a : t) (b : t) =
        match[@warning "-4"] a, b with
        | Op a, Op b -> Instruction_id.equal a.id b.id
        | _ -> a == b

      let arg_type (i : t) : Cmm.machtype_component =
        match i with
        | Op { typ = [| t |]; _ } -> t
        | Op { typ; _ } ->
          Misc.fatal_errorf
            "Ssa.Instruction.arg_type: Op has %d-component type; project first"
            (Array.length typ)
        | Block_param { block; index } -> block.params.(index)
        | Proj { index; src = Op { typ; _ } } -> typ.(index)
        | Proj _ | Tuple _ | Push_trap _ | Pop_trap _ | Stack_check _
        | Name_for_debugger _ ->
          Misc.fatal_error "Ssa.Instruction.arg_type: not a valid argument"

      let make_op ~op ~typ ~args ~dbg =
        Op
          { id = Instruction_id.create (); op; typ; args; dbg; usage_count = 0 }

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
              args : Instruction.t array;
              handler : Block.t option
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
              exn_continuation : Block.t option
            }
        | Invalid of
            { message : string;
              args : Instruction.t array;
              continuation : Block.t option
            }

      val successors : t -> Block.t list
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
              args : Instruction.t array;
              handler : Block.t option
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
              exn_continuation : Block.t option
            }
        | Invalid of
            { message : string;
              args : Instruction.t array;
              continuation : Block.t option
            }

      let successors (t : t) : Block.t list =
        match t with
        | Return _ | Tailcall_func _ -> []
        | Raise { handler; _ } -> (
          match handler with Some h -> [h] | None -> [])
        | Goto { goto; _ } -> [goto]
        | Branch { ifso; ifnot; _ } -> [ifso; ifnot]
        | Switch { targets; _ } -> Array.to_list targets
        | Tailcall_self { destination; _ } -> [destination]
        | Call { continuation; exn_continuation; _ } -> (
          match exn_continuation with
          | Some l -> [continuation; l]
          | None -> [continuation])
        | Invalid { continuation; _ } -> (
          match continuation with Some l -> [l] | None -> [])
    end

    (* === Builder state === *)

    type unfinished_block =
      | Empty_block of Block.t
      | Add_instruction of unfinished_block * Instruction.t

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
        predecessors = Block_set.empty;
        body = [||];
        terminator = pending_terminator;
        terminator_dbg = Debuginfo.none;
        dominator_info = { depth = -1; dominator = dummy_block };
        label_hint = None;
        param_usage_counts = [||]
      }

    let create_block ~is_function_start ~params : Block.t =
      { id = Block_id.create ();
        is_function_start;
        params;
        predecessors = Block_set.empty;
        body = [||];
        terminator = pending_terminator;
        terminator_dbg = Debuginfo.none;
        dominator_info = { depth = -1; dominator = dummy_block };
        label_hint = None;
        param_usage_counts = [||]
      }

    let make_block_result ~is_function_start ~params : new_block_result =
      let block = create_block ~is_function_start ~params in
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
      block, params

    (* === Builder operations: functional, returning new unfinished_block === *)

    let start_block (blk : Block.t) : unfinished_block = Empty_block blk

    let emit_instruction (ub : unfinished_block) (i : Instruction.t) :
        unfinished_block * Instruction.t =
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
      Add_instruction (ub, i), i

    let emit_op (ub : unfinished_block) ~op ~dbg ~typ ~args :
        unfinished_block * Instruction.t =
      emit_instruction ub (Instruction.make_op ~op ~typ ~args ~dbg)

    let rec collect_body_into acc (ub : unfinished_block) =
      match ub with
      | Empty_block blk -> blk, acc
      | Add_instruction (ub', i) -> collect_body_into (i :: acc) ub'

    let check_args_arity ~term_name ~args ~(expected : Cmm.machtype) =
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
      | Raise { handler = Some h; args; _ } ->
        check_args_arity ~term_name:"Raise" ~args ~expected:h.params
      | Branch { ifso; ifnot; _ } ->
        check_target_has_no_params ~term_name:"Branch" ifso;
        check_target_has_no_params ~term_name:"Branch" ifnot
      | Switch { targets; _ } ->
        Array.iter (check_target_has_no_params ~term_name:"Switch") targets
      | Return _
      | Raise { handler = None; _ }
      | Tailcall_func _ | Call _ | Invalid _ ->
        ()

    let finish_block (ub : unfinished_block) ~(dbg : Debuginfo.t)
        (term : Terminator.t) : unit =
      let blk, body_list = collect_body_into [] ub in
      if blk.terminator != pending_terminator
      then Misc.fatal_error "Ssa.finish_block: block already finished";
      check_terminator term;
      blk.body <- Array.of_list body_list;
      blk.terminator <- term;
      blk.terminator_dbg <- dbg;
      finished_blocks := blk :: !finished_blocks

    (* === Predecessors / dominators === *)

    let predecessors (blk : Block.t) : Block.t list =
      Block_set.elements blk.predecessors

    let successors (blk : Block.t) : Block.t list =
      Terminator.successors blk.terminator

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

    (* === Compute metadata: predecessors, dominators, use counts === *)

    (* Forward search from [entry]: populates [predecessors] on each reachable
       block, assigns DFS post-order numbers (immutable, used by the dominator
       pass), and verifies that every reachable block has been finished. *)
    let compute_reachability ~(entry : Block.t)
        ~(finished_blocks : Block.t list) : Block.t list =
      let visited = Block.Tbl.create 64 in
      let worklist = ref [entry] in
      while not (List.is_empty !worklist) do
        let blk = List.hd !worklist in
        worklist := List.tl !worklist;
        if not (Block.Tbl.mem visited blk)
        then begin
          Block.Tbl.add visited blk ();
          List.iter
            (fun (succ : Block.t) ->
              succ.predecessors <- Block_set.add blk succ.predecessors;
              worklist := succ :: !worklist)
            (successors blk)
        end
      done;
      let reachable_blocks =
        List.filter (fun blk -> Block.Tbl.mem visited blk) finished_blocks
      in
      finished_blocks |> List.iter (Block.Tbl.remove visited);
      visited
      |> Block.Tbl.iter (fun unfinished ->
          Misc.fatal_errorf "Ssa.finish: reachable block B%d was never finished"
            (Block_id.hash unfinished.id));
      reachable_blocks

    (* Cooper-Harvey-Kennedy "Simple, Fast Dominance Algorithm". [intersect]
       walks two fingers up the partially-constructed dominator tree using DFS
       post-order numbers (immutable) rather than depth (which can be stale
       during iteration). Reachable blocks are processed in reverse post-order
       until a fixpoint is reached. *)
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
                Block_set.fold
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
            Block_set.iter
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
              | Op r when not (Operation.is_removable_when_unused r.op) ->
                increment_use i
              | Name_for_debugger { regs; _ } -> Array.iter increment_use regs
              | Op _ | Push_trap _ | Pop_trap _ | Stack_check _ -> ()
              | Block_param _ | Proj _ | Tuple _ -> assert false)
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
        compute_reachability ~entry ~finished_blocks
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
        module Block_set = Block_set
        module Instruction = Instruction
        module Terminator = Terminator

        let function_info = function_info

        let entry = entry

        let blocks = reachable

        let predecessors = predecessors

        let successors = successors

        let dominates = dominates

        let common_dominator = common_dominator
      end in
      (module Finished : Finished_graph)
  end in
  (module M : Standalone_graph_builder)
