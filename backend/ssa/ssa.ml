open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-30-40-41-42-67"]

(** SSA graph implementation; signatures are in {!Ssa_intf}.

    A single underlying module satisfies both [Graph_builder] and
    [Finished_graph]. During construction the abstract types in [Graph_builder]
    hide use counts, predecessors and dominator info; at [finish_graph] time the
    same module is repackaged with the [Finished_graph] view that exposes them.

    Construction appends each [emit_*] instruction onto the current block's
    [pending_body] (newest-first); [finish_block] reverses it into program
    order, seals the terminator, and records the block. [finish_graph] then
    fills in metadata and produces the final immutable [body] array.

    [finish_graph] performs four passes over the finished blocks, in order:
    - [compute_reachability_and_trap_stacks]: forward DFS from [entry] following
      structural and exception successors, threading the trap stack so each
      reachable block gets its [block_end_trap_stack] populated; unreachable
      blocks are pruned.
    - [compute_dominators]: iterative meet-over-predecessors fixpoint that walks
      reachable blocks until no [dominator_info] changes.
    - [compute_use_counts]: refcount over op args and block params; the latter
      propagate to predecessors' [Goto] args, so a transitively-unused arg keeps
      its defining op count at zero.
    - [finalize_blocks]: materialise [body] from [pending_body]. Always drops
      Push_trap/Pop_trap pairs whose handler is unreachable. When
      [keep_unused_ops] is false, also drops dead [Op]s and replaces [Goto] args
      going to unused target params with [None].

    Invariants enforced here:
    - Every reachable block is finished via [finish_block].
    - [Tuple] never appears in a finished graph, only transiently as an
      [Ssa_reducer] instruction replacement representative.
    - [Block_param], [Proj] never appear in a block body.
    - [Block_param.param_index] is bounds-checked by [make_block_param]. *)

include Ssa_intf

let make_builder (function_info : Function_info.t) ~keep_unused_ops :
    (module Make_builder_result) =
  let module Builder = struct
    module Instruction_id = Oxcaml_utils.Id_counter.Make ()
    module Block_id = Oxcaml_utils.Id_counter.Make ()

    type usage_count = int

    type op = Operation.t

    type block_param =
      { typ : Cmm.machtype_component;
        mutable name : string option;
        mutable usage_count : usage_count
      }

    type 'block instruction =
      | Op of 'block op_data
      | Block_param of 'block block_param_data
      | Proj of 'block proj_data
      | Tuple of 'block instruction array
      | Push_trap of { handler : 'block }
      | Pop_trap of { handler : 'block }
      | Stack_check of { max_frame_size_bytes : int }
      | Name_for_debugger of
          { ident : Ident.t;
            provenance : Backend_var.Provenance.t option;
            which_parameter : int option;
            args : 'block instruction array
          }

    and 'block op_data =
      { id : Instruction_id.t;
        op : Operation.t;
        typ : Cmm.machtype;
        args : 'block instruction array;
        dbg : Debuginfo.t;
        mutable usage_count : usage_count;
        mutable name : string option
      }

    and 'block block_param_data =
      { block : 'block;
        param_index : int
      }

    and 'block proj_data =
      { src : 'block instruction;
        output_index : int
      }

    type 'block terminator =
      | Goto of
          { goto : 'block;
            args : 'block instruction option array
          }
      | Branch of
          { cond : 'block instruction;
            ifso : 'block;
            ifnot : 'block
          }
      | Switch of
          { index : 'block instruction;
            targets : 'block array
          }
      | Return of { args : 'block instruction array }
      | Raise of
          { raise_kind : Lambda.raise_kind;
            args : 'block instruction array
          }
      | Tailcall_self of
          { destination : 'block;
            args : 'block instruction array
          }
      | Tailcall_func of
          { op : Cfg_intf.S.func_call_operation;
            args : 'block instruction array
          }
      | Call of
          { op : call_op;
            args : 'block instruction array;
            continuation : 'block;
            may_raise : bool;
            nontail : bool
          }
      | Invalid of
          { message : string;
            args : 'block instruction array;
            continuation : 'block option
          }

    let function_info : Function_info.t = function_info

    module Block = struct
      type param = block_param

      module Id = Block_id

      let set_param_name (p : param) name = p.name <- Some name

      type dominator_info =
        { depth : int;
          dominator : block
        }

      and block =
        { id : Block_id.t;
          is_function_start : bool;
          params : param array;
          mutable predecessors : block list;
          mutable body : block instruction array;
          mutable pending_body : block instruction list;
          mutable terminator : block terminator;
          mutable terminator_dbg : Debuginfo.t;
          mutable dominator_info : dominator_info;
          mutable block_end_trap_stack : block list
        }

      type t = block

      type pending_body = t instruction list

      let id (b : t) = b.id

      let is_function_start (b : t) = b.is_function_start

      let params (b : t) = b.params

      let equal (a : t) (b : t) = Block_id.equal a.id b.id

      let compare (a : t) (b : t) = Block_id.compare a.id b.id

      let hash (b : t) = Block_id.hash b.id

      module Self = struct
        type nonrec t = t

        let equal = equal

        let compare = compare

        let hash = hash
      end

      module Map = Map.Make (Self)
      module Set = Set.Make (Self)
      module Tbl = Hashtbl.Make (Self)

      let print_id ppf (b : t) = Format.fprintf ppf "B%d" (b.id :> int)

      let print_param ppf (block : t) index =
        block.params.(index).name |> Option.iter (Format.fprintf ppf "%s/");
        Format.fprintf ppf "%a.%d" print_id block index

      let predecessors (b : t) : t list = b.predecessors

      let params_machtype (b : t) : Cmm.machtype =
        Array.map (fun (p : param) -> p.typ) b.params

      (* The terminator is missing the trap successors, which are derived from
         [block_end_trap_stack]. *)
      let non_trap_successors_of_terminator (t : t terminator) : t list =
        match t with
        | Return _ | Tailcall_func _ | Raise _ -> []
        | Goto { goto; _ } -> [goto]
        | Branch { ifso; ifnot; _ } -> [ifso; ifnot]
        | Switch { targets; _ } -> Array.to_list targets
        | Tailcall_self { destination; _ } -> [destination]
        | Call { continuation; _ } -> [continuation]
        | Invalid { continuation; _ } -> (
          match continuation with Some l -> [l] | None -> [])

      let trap_successor (block : t) : t option =
        let raises =
          match block.terminator with
          | Raise _ -> true
          | Call { may_raise; _ } -> may_raise
          | Goto _ | Branch _ | Switch _ | Return _ | Tailcall_self _
          | Tailcall_func _ | Invalid _ ->
            false
        in
        if raises
        then
          match block.block_end_trap_stack with [] -> None | h :: _ -> Some h
        else None

      let successors (block : t) : t list =
        let structural = non_trap_successors_of_terminator block.terminator in
        match trap_successor block with
        | None -> structural
        | Some h -> h :: structural

      let rec dominates (a : t) (b : t) =
        equal a b
        || b.dominator_info.depth > a.dominator_info.depth
           && dominates a b.dominator_info.dominator

      let rec common_dominator (a : t) (b : t) : t =
        if equal a b
        then a
        else if a.dominator_info.depth > b.dominator_info.depth
        then common_dominator a.dominator_info.dominator b
        else if b.dominator_info.depth > a.dominator_info.depth
        then common_dominator a b.dominator_info.dominator
        else
          common_dominator a.dominator_info.dominator b.dominator_info.dominator
    end

    type block = Block.t

    module Instruction = struct
      type t = Block.t instruction

      module Id = Instruction_id

      type nonrec op_data = Block.t op_data

      let rec equal (instr1 : t) (instr2 : t) =
        match instr1, instr2 with
        | Op { id = id1; _ }, Op { id = id2; _ } -> Instruction_id.equal id1 id2
        | ( Block_param { block = b1; param_index = i1 },
            Block_param { block = b2; param_index = i2 } ) ->
          Block.equal b1 b2 && Int.equal i1 i2
        | ( Proj { src = src1; output_index = i1 },
            Proj { src = src2; output_index = i2 } ) ->
          equal src1 src2 && Int.equal i1 i2
        | Tuple arr1, Tuple arr2 ->
          Array.length arr1 = Array.length arr2
          && Array.for_all2 equal arr1 arr2
        | Push_trap { handler = h1 }, Push_trap { handler = h2 }
        | Pop_trap { handler = h1 }, Pop_trap { handler = h2 } ->
          Block.equal h1 h2
        | ( Stack_check { max_frame_size_bytes = s1 },
            Stack_check { max_frame_size_bytes = s2 } ) ->
          Int.equal s1 s2
        | ( Name_for_debugger
              { ident = ident1;
                provenance = provenance1;
                which_parameter = which_parameter1;
                args = args1
              },
            Name_for_debugger
              { ident = ident2;
                provenance = provenance2;
                which_parameter = which_parameter2;
                args = args2
              } ) ->
          Ident.same ident1 ident2
          && Option.equal Backend_var.Provenance.equal provenance1 provenance2
          && Option.equal Int.equal which_parameter1 which_parameter2
          && Array.length args1 = Array.length args2
          && Array.for_all2 equal args1 args2
        | ( ( Op _ | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
            | Stack_check _ | Name_for_debugger _ ),
            _ ) ->
          false

      let arg_type (instr : t) : Cmm.machtype_component =
        match instr with
        | Op { typ = [| t |]; _ } -> t
        | Op { typ; _ } ->
          Misc.fatal_errorf "Ssa.Instruction.arg_type: Op has %d results; %s"
            (Array.length typ)
            (if Array.length typ = 0
             then "cannot be used as argument"
             else "please project before using as an argument")
        | Block_param { block; param_index } -> block.params.(param_index).typ
        | Proj { src = Op { typ; _ }; output_index } -> typ.(output_index)
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

      let set_name (instr : t) name =
        match[@warning "-fragile-match"] instr with
        | Op r -> r.name <- Some name
        | Block_param { block; param_index } ->
          block.params.(param_index).name <- Some name
        | _ -> ()

      let make_block_param (block : Block.t) (param_index : int) =
        if param_index < 0 || param_index >= Array.length block.params
        then
          Misc.fatal_errorf
            "Ssa.Instruction.make_block_param: param_index %d out of range for \
             block with %d params"
            param_index
            (Array.length block.params);
        Block_param { block; param_index }

      let result_arity instr =
        match instr with
        | Tuple elems -> Array.length elems
        | Op { typ; _ } -> Array.length typ
        | Proj _ | Block_param _ -> 1
        | Name_for_debugger _ | Push_trap _ | Pop_trap _ | Stack_check _ -> 0

      let make_proj ~index src =
        match src with
        | Tuple elems -> Array.get elems index
        | Op _ ->
          let arity = result_arity src in
          if not (index >= 0 && index < arity)
          then
            Misc.fatal_errorf
              "Ssa.Instruction.make_proj: output_index %d out of range for \
               instruction with %d results"
              index arity;
          Proj { output_index = index; src }
        | Block_param _ | Proj _ | Push_trap _ | Pop_trap _ | Stack_check _
        | Name_for_debugger _ ->
          Misc.fatal_error
            "Ssa.Instruction.make_proj: cannot project from a \
             non-value-producing instruction"

      let print_op_id ppf (od : op_data) =
        od.name |> Option.iter (Format.fprintf ppf "%s/");
        Format.fprintf ppf "v%d" (od.id :> int)

      let rec print ppf (instr : t) =
        match instr with
        | Op ({ op; args; _ } as od) ->
          if result_arity instr <> 0
          then Format.fprintf ppf "%a = " print_op_id od;
          let op_str = Format.asprintf "%a" Operation.dump op in
          if Array.length args = 0
          then Format.pp_print_string ppf op_str
          else
            let formatted_op =
              if String.contains op_str ' ' then "(" ^ op_str ^ ")" else op_str
            in
            Format.fprintf ppf "%s(%a)" formatted_op print_args args
        | Push_trap { handler } ->
          Format.fprintf ppf "push_trap %a" Block.print_id handler
        | Pop_trap { handler } ->
          Format.fprintf ppf "pop_trap %a" Block.print_id handler
        | Stack_check { max_frame_size_bytes } ->
          Format.fprintf ppf "stack_check %d" max_frame_size_bytes
        | Name_for_debugger { ident; args; _ } ->
          Format.fprintf ppf "name_for_debugger %a(%a)" Ident.print ident
            print_args args
        | Block_param { block; param_index } ->
          Block.print_param ppf block param_index
        | Proj { output_index; src } ->
          Format.fprintf ppf "%a.%i" print_as_ref src output_index
        | Tuple elems -> Format.fprintf ppf "tuple(%a)" print_args elems

      and print_as_ref ppf (instr : t) =
        match[@warning "-fragile-match"] instr with
        | Op od -> print_op_id ppf od
        | _ -> print ppf instr

      and print_args ppf args =
        Array.iteri
          (fun i arg ->
            if i > 0 then Format.fprintf ppf ", ";
            print_as_ref ppf arg)
          args
    end

    module Terminator = struct
      type t = Block.t terminator

      type block = Block.t

      let non_trap_successors = Block.non_trap_successors_of_terminator

      let print_opt_args ppf args =
        Array.iteri
          (fun i arg ->
            if i > 0 then Format.fprintf ppf ", ";
            match arg with
            | None -> Format.fprintf ppf "_"
            | Some arg -> Instruction.print_as_ref ppf arg)
          args

      let print ppf (t : t) =
        match t with
        | Goto { goto; args } ->
          Format.fprintf ppf "goto %a(%a)" Block.print_id goto print_opt_args
            args
        | Branch { cond; ifso; ifnot } ->
          Format.fprintf ppf "if %a then goto %a else goto %a"
            Instruction.print_as_ref cond Block.print_id ifso Block.print_id
            ifnot
        | Switch { index; targets } ->
          Format.fprintf ppf "switch(%a) [" Instruction.print_as_ref index;
          Array.iteri
            (fun i tgt ->
              if i > 0 then Format.fprintf ppf ", ";
              Block.print_id ppf tgt)
            targets;
          Format.fprintf ppf "]"
        | Return { args } ->
          Format.fprintf ppf "return(%a)" Instruction.print_args args
        | Raise { args; _ } ->
          Format.fprintf ppf "raise(%a)" Instruction.print_args args
        | Tailcall_self { destination; args } ->
          Format.fprintf ppf "tailcall_self %a(%a)" Block.print_id destination
            Instruction.print_args args
        | Tailcall_func { op = Direct sym; args } ->
          Format.fprintf ppf "tailcall %s(%a)" sym.sym_name
            Instruction.print_args args
        | Tailcall_func { op = Indirect _; args } ->
          Format.fprintf ppf "tailcall_indirect(%a)" Instruction.print_args args
        | Call
            { op = Func (Direct sym); args; continuation; may_raise; nontail }
          ->
          Format.fprintf ppf "call %s(%a) -> %a" sym.sym_name
            Instruction.print_args args Block.print_id continuation;
          if may_raise then Format.fprintf ppf " may_raise";
          if nontail then Format.fprintf ppf " nontail"
        | Call
            { op = Func (Indirect _); args; continuation; may_raise; nontail }
          ->
          Format.fprintf ppf "call_indirect(%a) -> %a" Instruction.print_args
            args Block.print_id continuation;
          if may_raise then Format.fprintf ppf " may_raise";
          if nontail then Format.fprintf ppf " nontail"
        | Call
            { op = Prim (External { func_symbol; _ });
              args;
              continuation;
              may_raise;
              nontail = _
            } ->
          Format.fprintf ppf "call_prim %s(%a) -> %a" func_symbol
            Instruction.print_args args Block.print_id continuation;
          if may_raise then Format.fprintf ppf " may_raise"
        | Call { op = Prim (Probe { name; _ }); args; continuation; _ } ->
          Format.fprintf ppf "probe %s(%a) -> %a" name Instruction.print_args
            args Block.print_id continuation
        | Invalid { message; args; continuation } -> (
          Format.fprintf ppf "invalid(%a) \"%s\"" Instruction.print_args args
            message;
          match continuation with
          | Some l -> Format.fprintf ppf " -> %a" Block.print_id l
          | None -> ())
    end

    (* === Builder state === *)

    (* Finished blocks, in reverse order. *)
    let finished_blocks_rev : Block.t list ref = ref []

    (* Sentinel terminator used as the initial value of the [terminator] field.
       [Cursor.is_finished] checks (via physical equality) that the field still
       holds this sentinel. *)
    let rec pending_terminator : Terminator.t =
      Goto { goto = dummy_block; args = [||] }

    and dummy_block : Block.t =
      { id = Block_id.create ();
        is_function_start = false;
        params = [||];
        predecessors = [];
        body = [||];
        pending_body = [];
        terminator = pending_terminator;
        terminator_dbg = Debuginfo.none;
        dominator_info = { depth = -1; dominator = dummy_block };
        block_end_trap_stack = []
      }

    let create_block ~is_function_start ~params : Block.t =
      { id = Block_id.create ();
        is_function_start;
        params;
        predecessors = [];
        body = [||];
        pending_body = [];
        terminator = pending_terminator;
        terminator_dbg = Debuginfo.none;
        dominator_info = { depth = -1; dominator = dummy_block };
        block_end_trap_stack = []
      }

    let check_args_arity ~term_name ~args ~expected =
      if Array.length args <> Array.length expected
      then
        Misc.fatal_errorf
          "Ssa.check_args_arity: %s passes %d args but expected %d" term_name
          (Array.length args) (Array.length expected)

    let check_target_has_no_params ~term_name (target : Block.t) =
      if Array.length target.params > 0
      then
        Misc.fatal_errorf
          "Ssa.check_target_has_no_params: %s target must have no params"
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

    module Cursor = struct
      type t = { mutable block : Block.t }

      let start (block : Block.t) : t = { block }

      let is_finished (c : t) = c.block.terminator != pending_terminator

      let move (c : t) ~(new_pos : t) : unit =
        assert (is_finished c);
        c.block <- new_pos.block
    end

    type cursor = Cursor.t

    let emit_instruction (c : Cursor.t) (instr : Instruction.t) =
      match instr with
      | Op _ | Push_trap _ | Pop_trap _ | Stack_check _ | Name_for_debugger _ ->
        c.block.pending_body <- instr :: c.block.pending_body
      | Block_param _ | Proj _ | Tuple _ ->
        Misc.fatal_errorf
          "Ssa.emit_instruction: %a cannot be emitted into a block body"
          Instruction.print instr

    let emit_op (c : Cursor.t) ~op ~dbg ~typ ~args : Instruction.t =
      let instr = Instruction.make_op ~op ~typ ~args ~dbg in
      emit_instruction c instr;
      instr

    let finish_block (c : Cursor.t) ~(dbg : Debuginfo.t) (term : Terminator.t) :
        unit =
      let block = c.block in
      if Cursor.is_finished c
      then Misc.fatal_error "Ssa.finish_block: block already finished";
      check_terminator term;
      block.pending_body <- List.rev block.pending_body;
      block.terminator <- term;
      block.terminator_dbg <- dbg;
      finished_blocks_rev := block :: !finished_blocks_rev

    type new_block_result =
      { block : Block.t;
        params : Instruction.t array
      }

    let new_block_impl ~is_function_start ~(params : Cmm.machtype) :
        new_block_result =
      let params =
        params
        |> Array.map (fun typ : Block.param ->
            { typ; name = None; usage_count = 0 })
      in
      let block = create_block ~is_function_start ~params in
      let params_arr =
        Array.init (Array.length params) (fun i ->
            Instruction.make_block_param block i)
      in
      { block; params = params_arr }

    let new_block ~params = new_block_impl ~is_function_start:false ~params

    let new_block_with_names
        ~(params : (Cmm.machtype_component * string option) array) : Block.t =
      let params =
        params
        |> Array.map (fun (typ, name) : Block.param ->
            { typ; name; usage_count = 0 })
      in
      create_block ~is_function_start:false ~params

    let entry, entry_params =
      let { block; params } =
        new_block_impl ~is_function_start:true ~params:function_info.args
      in
      (* Initialize names from the function's argument names, when known. *)
      List.iteri
        (fun i (var, _ty) ->
          if i < Array.length block.params
          then
            Block.set_param_name block.params.(i)
              (Backend_var.name (Backend_var.With_provenance.var var)))
        function_info.args_names;
      block, params

    (* === Compute metadata: predecessors, traps, dominators, use counts === *)

    (* Apply the [Push_trap]/[Pop_trap] effects of [body] to [start_stack]. Each
       [Pop_trap { handler = h }] must find [h] at the top of the current
       stack. *)
    let compute_block_end_trap_stack ~(block : Block.t)
        (start_stack : Block.t list) (body : Instruction.t list) : Block.t list
        =
      List.fold_left
        (fun (stack : Block.t list) (instr : Instruction.t) ->
          match instr with
          | Push_trap { handler = h } -> h :: stack
          | Pop_trap { handler = h } -> (
            match stack with
            | [] ->
              Misc.fatal_errorf
                "Ssa.compute_block_end_trap_stack: block B%d pops handler B%d \
                 off an empty trap stack"
                (block.id :> int)
                (h.id :> int)
            | top :: rest ->
              if not (Block.equal top h)
              then
                Misc.fatal_errorf
                  "Ssa.compute_block_end_trap_stack: block B%d pops handler \
                   B%d but top of trap stack is B%d"
                  (block.id :> int)
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
    let compute_reachability_and_trap_stacks () : Block.t list =
      let visited = Block.Tbl.create 64 in
      let worklist = ref [entry, []] in
      while not (List.is_empty !worklist) do
        let block, start_stack = List.hd !worklist in
        worklist := List.tl !worklist;
        if not (Block.Tbl.mem visited block)
        then begin
          Block.Tbl.add visited block ();
          let end_stack =
            compute_block_end_trap_stack ~block start_stack block.pending_body
          in
          block.block_end_trap_stack <- end_stack;
          let add_pred start_stack (succ : Block.t) =
            succ.predecessors <- block :: succ.predecessors;
            worklist := (succ, start_stack) :: !worklist
          in
          Block.non_trap_successors_of_terminator block.terminator
          |> List.iter (fun succ -> add_pred end_stack succ);
          (* On entry to a trap handler, the runtime has popped the topmost
             handler off the trap stack. *)
          Block.trap_successor block
          |> Option.iter (fun succ -> add_pred (List.tl end_stack) succ)
        end
      done;
      let reachable_blocks =
        List.filter
          (fun block -> Block.Tbl.mem visited block)
          (List.rev !finished_blocks_rev)
      in
      let unfinished_blocks =
        Block.Set.diff
          (Block.Tbl.to_seq_keys visited |> Block.Set.of_seq)
          (Block.Set.of_list !finished_blocks_rev)
      in
      unfinished_blocks
      |> Block.Set.iter (fun unfinished ->
          Misc.fatal_errorf
            "Ssa.compute_reachability_and_trap_stacks: reachable block B%d was \
             never finished"
            (unfinished.id :> int));
      reachable_blocks

    let compute_dominators ~(reachable_blocks : Block.t list) : unit =
      entry.dominator_info <- { depth = 0; dominator = entry };
      let has_idom (block : Block.t) = block.dominator_info.depth >= 0 in
      let changed = ref true in
      while !changed do
        changed := false;
        List.iter
          (fun (block : Block.t) ->
            if not (Block.equal block entry)
            then begin
              let new_idom =
                List.fold_left
                  (fun acc pred ->
                    if has_idom pred
                    then
                      match acc with
                      | None -> Some pred
                      | Some current ->
                        Some (Block.common_dominator pred current)
                    else acc)
                  (if has_idom block
                   then Some block.dominator_info.dominator
                   else None)
                  block.predecessors
              in
              match new_idom with
              | None -> ()
              | Some idom ->
                let new_info : Block.dominator_info =
                  { depth = idom.dominator_info.depth + 1; dominator = idom }
                in
                if block.dominator_info.depth <> new_info.depth
                then begin
                  block.dominator_info <- new_info;
                  changed := true
                end
                else
                  assert (
                    Block.equal block.dominator_info.dominator
                      new_info.dominator)
            end)
          reachable_blocks
      done

    (* Reference-counting walks over each reachable block's body and terminator.
       An unused operation does not incrase use counts of its inputs.
       [Block_param] increments propagate to predecessors' [Goto] args when the
       parameter becomes used. *)
    let compute_use_counts ~(reachable_blocks : Block.t list) : unit =
      let rec increment_use (instr : Instruction.t) =
        match instr with
        | Op r ->
          r.usage_count <- r.usage_count + 1;
          if (not keep_unused_ops) && r.usage_count = 1
          then Array.iter increment_use r.args
        | Proj { src; _ } -> increment_use src
        | Block_param { block; param_index } ->
          let p = block.params.(param_index) in
          let old = p.usage_count in
          p.usage_count <- old + 1;
          if (not keep_unused_ops) && old = 0
          then
            block.predecessors
            |> List.iter (fun (pred : Block.t) ->
                match pred.terminator with
                | Goto { args; _ } ->
                  args.(param_index) |> Option.iter increment_use
                | Branch _ | Switch _ | Return _ | Raise _ | Tailcall_self _
                | Tailcall_func _ | Call _ | Invalid _ ->
                  (* CR ttebbi: We could also treat [Tailcall_self] arguments as
                     unused when the corresponding entry param is dead, since
                     the function-entry params themselves cannot be dropped
                     (they come from the ABI), but the args we pass at the
                     recursive call site could be replaced with an indefinite
                     value. *)
                  ())
        | Tuple _ ->
          Misc.fatal_error
            "Ssa.compute_use_counts: Tuple should have been short-circuited by \
             make_proj"
        | Push_trap _ | Pop_trap _ | Stack_check _ | Name_for_debugger _ -> ()
      in
      let increment_uses_in_terminator (term : Terminator.t) =
        match term with
        | Goto { args; _ } ->
          if keep_unused_ops then args |> Array.iter (Option.iter increment_use)
        | Branch { cond = arg; _ } | Switch { index = arg; _ } ->
          increment_use arg
        | Return { args }
        | Raise { args; _ }
        | Tailcall_self { args; _ }
        | Tailcall_func { args; _ }
        | Call { args; _ }
        | Invalid { args; _ } ->
          Array.iter increment_use args
      in
      reachable_blocks
      |> List.iter (fun (block : Block.t) ->
          block.pending_body
          |> List.iter (fun (instr : Instruction.t) ->
              match instr with
              | Op { op; args; _ } ->
                if keep_unused_ops
                then Array.iter increment_use args
                else if not (Operation.is_pure op)
                then increment_use instr
              | Name_for_debugger { args; _ } -> Array.iter increment_use args
              | Push_trap _ | Pop_trap _ | Stack_check _ -> ()
              | Block_param _ | Proj _ | Tuple _ ->
                Misc.fatal_error
                  "Ssa.compute_use_counts: impossible body instruction");
          increment_uses_in_terminator block.terminator)

    (** Materialise [body] from [pending_body], with three pieces of cleanup:
        - filter out [Op]s with [usage_count = 0] (skipped when
          [keep_unused_ops]);
        - drop [Push_trap]/[Pop_trap] whose handler has no predecessors (i.e. is
          never raised into) — the matching push/pop reference the same handler
          and are dropped together, preserving trap-stack balance;
        - null out [Goto] args going to unused target params (skipped when
          [keep_unused_ops], so [Cfg_compare] sees a structurally faithful
          output). *)
    let finalize_blocks ~(reachable_blocks : Block.t list) : unit =
      reachable_blocks
      |> List.iter (fun (block : Block.t) ->
          block.body
            <- block.pending_body
               |> List.filter (fun (instr : Instruction.t) : bool ->
                   match instr with
                   | Op { usage_count; _ } -> keep_unused_ops || usage_count > 0
                   | Stack_check _ | Name_for_debugger _ -> true
                   | Push_trap { handler } | Pop_trap { handler } ->
                     not (List.is_empty handler.predecessors)
                   | Block_param _ | Proj _ | Tuple _ ->
                     Misc.fatal_error
                       "Ssa.finalize_blocks: impossible body instruction")
               |> Array.of_list;
          block.pending_body <- [];
          if not keep_unused_ops
          then
            block.terminator
              <- (match[@warning "-fragile-match"] block.terminator with
                 | Goto { goto; args } ->
                   let args =
                     args
                     |> Array.mapi (fun i arg ->
                         if goto.params.(i).usage_count = 0 then None else arg)
                   in
                   Goto { goto; args }
                 | terminator -> terminator))

    (* Walk [blocks] in the input order, but when first visiting a block emit
       its dominator chain ahead of it. The result has every block preceded by
       its dominators, while keeping the input order wherever the dominator
       constraint doesn't force a move. Requires [dominator_info] to already be
       populated. *)
    let order_blocks_dominators_first (blocks : Block.t list) : Block.t list =
      let visited = Block.Tbl.create 64 in
      let acc = ref [] in
      let rec visit (block : Block.t) =
        if not (Block.Tbl.mem visited block)
        then begin
          Block.Tbl.add visited block ();
          let dom = block.dominator_info.dominator in
          if not (Block.equal dom block) then visit dom;
          acc := block :: !acc
        end
      in
      List.iter visit blocks;
      List.rev !acc

    let compute_metadata () : Block.t list =
      let reachable_blocks = compute_reachability_and_trap_stacks () in
      compute_dominators ~reachable_blocks;
      compute_use_counts ~reachable_blocks;
      finalize_blocks ~reachable_blocks;
      order_blocks_dominators_first reachable_blocks
  end in
  let finished = ref false in
  let module Result = struct
    include Builder

    let finish_graph () =
      assert (not !finished);
      finished := true;
      let module Finished = struct
        include Builder

        let blocks = Builder.compute_metadata ()
      end in
      (module Finished : Finished_graph)
  end in
  (module Result : Make_builder_result)
