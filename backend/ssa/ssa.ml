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

(** SSA graph implementation; the signature is in [ssa.mli].

    A graph is a plain value of type ['g graph]. The construction-state tags
    [under_construction] and [finished] are defined as the same here, so the
    phantom collapses inside this module: a graph in either state has the same
    representation and [finish_graph] is a plain identity that fills in metadata
    and returns the same value re-tagged. The signature keeps the tags abstract,
    so to every other module ['g] genuinely distinguishes the two states across
    [Block.t] / [Instruction.t] / [Value.t] / [Terminator.t].

    An [instruction] is a body element ([Op] / [Push_trap] / [Pop_trap]); a
    [value] is an argument, either [Res (instr, i)] (the i-th result of an
    emitted [Op]) or [Block_param (block, i)] (the i-th parameter of [block]).
    [Res] values are only produced by [emit_op] (which returns one per result),
    so they always point to an emitted instruction with an in-range output
    index.

    Construction appends each emitted instruction onto the current block's
    [pending_body] (newest-first); [finish_block] seals the terminator, and
    records the block. [finish_graph] then fills in metadata and produces the
    final immutable [body] array.

    [finish_graph] performs four passes over the finished blocks, in order:
    - [compute_reachability_and_trap_stacks]: forward DFS from [entry] following
      structural and exception successors, threading the trap stack so each
      reachable block gets its [block_end_trap_stack] populated; unreachable
      blocks are pruned.
    - [compute_dominators]: iterative meet-over-predecessors fixpoint.
    - [increment_uses_in_block]: refcount over op args and block params; the
      latter propagate to predecessors' [Continue] args, so a
      transitively-unused arg keeps its defining op count at zero.
    - [finalize_block]: materialise [body] from [pending_body]. Always drops
      Push_trap/Pop_trap pairs whose handler is unreachable. When
      [keep_unused_ops] is false, also drops dead [Op]s and replaces
      [Continue (Goto _)] args going to unused target params with [Undefined].

    Invariants enforced here:
    - Every reachable block is finished via [finish_block].
    - [Block_param] / [Res] output indices are in range by construction. *)

module Block_id = Oxcaml_utils.Id_counter.Make ()
module Instruction_id = Oxcaml_utils.Id_counter.Make ()

(* The two construction-state tags are the same type here; the signature makes
   them abstract and distinct. *)
type finished

type under_construction = finished

type usage_count = int

type op = Operation.t

type call_op =
  | Direct of Cmm.symbol
  | Indirect of Cmm.symbol list option
  | External of Cfg_intf.S.external_call_operation
  | Probe of
      { name : string;
        handler_code_sym : string;
        enabled_at_init : bool
      }

module Function_info = struct
  type t =
    { sym_name : string;
      parameters : (Backend_var.With_provenance.t * Cmm.machtype) list;
      codegen_options : Cmm.codegen_option list;
      dbg : Debuginfo.t;
      poll : Lambda.poll_attribute;
      ret_type : Cmm.machtype
    }

  let flattened_parameters function_info =
    List.map snd function_info.parameters |> Array.concat
end

type block_param =
  { typ : Cmm.machtype_component;
    mutable name : string option;
    mutable usage_count : usage_count
  }

type 'g block =
  { block_id : Block_id.t;
    is_function_start : bool;
    params : block_param array;
    mutable predecessors : 'g block list;
    mutable body : 'g instruction array;
    mutable pending_body : 'g instruction list;
    mutable terminator : 'g terminator;
    mutable terminator_dbg : Debuginfo.t;
    mutable dominator_info : 'g dominator_info;
    mutable block_end_trap_stack : 'g block list
  }

and 'g dominator_info =
  { depth : int;
    dominator : 'g block
  }

and 'g op_data =
  { id : Instruction_id.t;
    op : Operation.t;
    typ : Cmm.machtype;
    args : 'g value array;
    dbg : Debuginfo.t;
    mutable usage_count : usage_count;
    mutable name : string option
  }

and 'g instruction =
  | Op of 'g op_data
  | Push_trap of { handler : 'g block }
  | Pop_trap of { handler : 'g block }

and 'g value =
  | Res of 'g op_data * int
  | Block_param of 'g block * int
  | Undefined

and 'g continuation =
  | Goto of 'g block
  | Return
  | Raise of Lambda.raise_kind

and 'g terminator =
  | Continue of
      { continuation : 'g continuation;
        args : 'g value array
      }
  | Switch of
      { index : 'g value;
        targets : 'g block array
      }
  | Call of
      { op : call_op;
        args : 'g value array;
        continuation : 'g continuation;
        may_raise : bool;
        nontail : bool
      }
  | Invalid of
      { message : string;
        args : 'g value array;
        continuation : 'g block option
      }

type 'g graph =
  { function_info : Function_info.t;
    keep_unused_ops : bool;
    block_id_gen : Block_id.generator;
    instruction_id_gen : Instruction_id.generator;
    entry : 'g block;
    mutable finished_blocks_rev : 'g block list;
    mutable blocks : 'g block list;
    mutable finished : bool
  }

(* Sentinel used as the initial [terminator] of a freshly created block, and as
   the placeholder dominator. [Cursor.is_finished] tests against it physically.
   It is shared across all graphs; its id is [Block_id.dummy], which no real
   block can have. *)
let rec pending_terminator : under_construction terminator =
  Continue { continuation = Goto dummy_block; args = [||] }

and dummy_block : under_construction block =
  { block_id = Block_id.dummy;
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

let create_block ~block_id_gen ~is_function_start ~(params : block_param array)
    : under_construction block =
  { block_id = Block_id.get_and_incr block_id_gen;
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

module Block = struct
  type nonrec 'g t = 'g block

  module Id = Block_id

  let create (graph : under_construction graph) ~(params : Cmm.machtype) :
      under_construction t =
    create_block ~block_id_gen:graph.block_id_gen ~is_function_start:false
      ~params:
        (params |> Array.map (fun typ -> { typ; name = None; usage_count = 0 }))

  let create_with_names (graph : under_construction graph)
      ~(params : (Cmm.machtype_component * string option) array) :
      under_construction t =
    create_block ~block_id_gen:graph.block_id_gen ~is_function_start:false
      ~params:
        (params |> Array.map (fun (typ, name) -> { typ; name; usage_count = 0 }))

  let id (b : 'g t) = b.block_id

  let is_function_start (b : 'g t) = b.is_function_start

  let param (b : 'g t) index : 'g value =
    if index < 0 || index >= Array.length b.params
    then
      Misc.fatal_errorf
        "Ssa.Block.param: index %d out of range for block with %d params" index
        (Array.length b.params);
    Block_param (b, index)

  let params (b : 'g t) : 'g value array =
    Array.init (Array.length b.params) (fun i -> Block_param (b, i))

  let equal (a : 'g t) (b : 'g t) =
    (* Blocks have mutable fields, so physical equality is safe. *)
    a == b

  let compare (a : 'g t) (b : 'g t) = Block_id.compare a.block_id b.block_id

  let hash (b : 'g t) = Block_id.hash b.block_id

  module Self = struct
    type t = finished block

    let equal = (equal : t -> t -> bool)

    let compare = (compare : t -> t -> int)

    let hash = (hash : t -> int)
  end

  module Map = Map.Make (Self)
  module Set = Set.Make (Self)
  module Tbl = Hashtbl.Make (Self)

  let print_id ppf (b : 'g t) = Format.fprintf ppf "B%d" (b.block_id :> int)

  let predecessors (b : finished t) : finished t list = b.predecessors

  let params_machtype (b : 'g t) : Cmm.machtype =
    Array.map (fun (p : block_param) -> p.typ) b.params

  let non_exn_successors_of_continuation (c : 'g continuation) : 'g block list =
    match c with Goto b -> [b] | Return | Raise _ -> []

  (* The terminator is missing the exception successors, which are derived from
     [block_end_trap_stack]. *)
  let non_exn_successors_of_terminator (t : 'g terminator) : 'g block list =
    match t with
    | Continue { continuation; _ } | Call { continuation; _ } ->
      non_exn_successors_of_continuation continuation
    | Switch { targets; _ } -> Array.to_list targets
    | Invalid { continuation; _ } -> (
      match continuation with Some l -> [l] | None -> [])

  let non_exn_successors (b : finished t) : finished t list =
    non_exn_successors_of_terminator b.terminator

  let exn_successor (block : finished t) : finished t option =
    let raises =
      match block.terminator with
      | Continue { continuation = Raise _; _ } -> true
      | Call { may_raise; _ } -> may_raise
      | Continue { continuation = Goto _ | Return; _ } | Switch _ | Invalid _ ->
        false
    in
    if raises
    then match block.block_end_trap_stack with [] -> None | h :: _ -> Some h
    else None

  let block_end_trap_stack (block : finished t) : finished t list =
    block.block_end_trap_stack

  let successors (block : finished t) : finished t list =
    let structural = non_exn_successors_of_terminator block.terminator in
    match exn_successor block with
    | None -> structural
    | Some h -> h :: structural

  let rec dominates (a : finished t) (b : finished t) =
    equal a b
    || b.dominator_info.depth > a.dominator_info.depth
       && dominates a b.dominator_info.dominator

  let rec common_dominator (a : finished t) (b : finished t) : finished t =
    if equal a b
    then a
    else if a.dominator_info.depth > b.dominator_info.depth
    then common_dominator a.dominator_info.dominator b
    else if b.dominator_info.depth > a.dominator_info.depth
    then common_dominator a b.dominator_info.dominator
    else common_dominator a.dominator_info.dominator b.dominator_info.dominator

  let immediate_dominator (b : finished t) : finished t =
    b.dominator_info.dominator

  let dominator_depth (b : finished t) : int = b.dominator_info.depth

  let body (b : finished t) : finished instruction array = b.body

  let terminator (b : finished t) : finished terminator = b.terminator

  let terminator_dbg (b : finished t) : Debuginfo.t = b.terminator_dbg
end

(* Print the [name/vN] reference of an [Op]; shared by [Value.print] (which
   prints args as references) and [Instruction.print] (which prefixes a full op
   with its result name). *)
let print_op_id ppf ~id ~(name : string option) =
  name |> Option.iter (Format.fprintf ppf "%s/");
  Format.fprintf ppf "v%d" (id : Instruction_id.t :> int)

module Value = struct
  type nonrec 'g t = 'g value

  let undefined : 'g t = Undefined

  let equal (a : 'g t) (b : 'g t) =
    match a, b with
    | Res (op1, n1), Res (op2, n2) ->
      (* [op_data] has mutable fields, so physical equality is safe. *)
      op1 == op2 && n1 = n2
    | Block_param (b1, n1), Block_param (b2, n2) ->
      Block.equal b1 b2 && Int.equal n1 n2
    | Undefined, Undefined -> true
    | (Res _ | Block_param _ | Undefined), _ -> false

  let typ (value : 'g t) : Cmm.machtype_component =
    match value with
    | Res ({ typ; _ }, i) -> typ.(i)
    | Block_param (block, i) -> block.params.(i).typ
    | Undefined -> Misc.fatal_error "Ssa.Value.typ: Undefined has no type"

  (* Print a value as it appears in an argument position: an [Op] result prints
     as [vN] (or [vN.i] when the op produces multiple results), a block param as
     [B.i]. *)
  let print ppf (value : 'g t) =
    match value with
    | Res ({ id; name; typ; _ }, i) ->
      print_op_id ppf ~id ~name;
      if Array.length typ > 1 then Format.fprintf ppf ".%d" i
    | Block_param (block, i) ->
      block.params.(i).name |> Option.iter (Format.fprintf ppf "%s/");
      Format.fprintf ppf "%a.%d" Block.print_id block i
    | Undefined -> Format.fprintf ppf "undef"

  let set_name (value : 'g t) name =
    match value with
    | Res (r, _) -> r.name <- Some name
    | Block_param (block, i) -> block.params.(i).name <- Some name
    | Undefined -> ()

  let name (value : 'g t) : string option =
    match value with
    | Res ({ name; _ }, _) -> name
    | Block_param (block, i) -> block.params.(i).name
    | Undefined -> None

  let usage_count (value : finished t) : int =
    match value with
    | Res ({ usage_count; _ }, _) -> usage_count
    | Block_param (block, i) -> block.params.(i).usage_count
    | Undefined -> 0
end

module Instruction = struct
  module Id = Instruction_id

  type nonrec 'g op_data = 'g op_data =
    { id : Id.t;
      op : Operation.t;
      typ : Cmm.machtype;
      args : 'g value array;
      dbg : Debuginfo.t;
      mutable usage_count : usage_count;
      mutable name : string option
    }

  type nonrec 'g t = 'g instruction =
    | Op of 'g op_data
    | Push_trap of { handler : 'g block }
    | Pop_trap of { handler : 'g block }

  type nonrec 'g value = 'g value =
    | Res of 'g op_data * int
    | Block_param of 'g block * int
    | Undefined

  let result_arity (instr : 'g t) =
    match instr with
    | Op { typ; _ } -> Array.length typ
    | Push_trap _ | Pop_trap _ -> 0

  let removable_when_unused (instr : 'g t) : bool =
    match instr with
    | Op { op; _ } -> Operation.is_pure op
    | Push_trap _ | Pop_trap _ -> false

  let has_side_effect (instr : 'g t) : bool =
    match instr with
    | Op { op; _ } -> (
      match op with
      | Move | Spill | Reload -> false
      | Const_int _ | Const_float32 _ | Const_float _ | Const_symbol _
      | Const_vec128 _ | Const_vec256 _ | Const_vec512 _ ->
        false
      | Stackoffset _ -> true
      | Load _ -> false
      | Store _ -> true
      | Intop _ | Int128op _ | Intop_imm _ -> false
      | Intop_atomic _ -> true
      | Floatop _ | Csel _ -> false
      | Reinterpret_cast _ | Static_cast _ -> false
      | Probe_is_enabled _ -> false
      | Opaque -> true
      | Begin_region | End_region -> true
      | Specific s -> not (Arch.operation_is_pure s)
      | Name_for_debugger _ -> false
      | Dls_get | Tls_get | Domain_index -> false
      | Poll | Pause -> true
      | Alloc _ -> true)
    | Push_trap _ | Pop_trap _ -> true

  let usage_count (op : finished op_data) : int = op.usage_count

  let print ppf (instr : 'g t) =
    match instr with
    | Op { op; args; id; name; _ } ->
      if result_arity instr <> 0
      then begin
        print_op_id ppf ~id ~name;
        Format.fprintf ppf " = "
      end;
      let arg_regs =
        Array.mapi
          (fun i _ ->
            Reg.For_printing.create ~name:Reg.dummy.name ~typ:Cmm.Val
              ~stamp:(Reg.Stamp.of_int_unsafe i)
              ~preassigned:false ~loc:Reg.Unknown)
          args
      in
      let print_reg ppf (r : Reg.t) =
        Value.print ppf args.(Reg.Stamp.to_int r.stamp)
      in
      Printoperation.operation ~print_reg op arg_regs ppf [||]
    | Push_trap { handler } ->
      Format.fprintf ppf "push_trap %a" Block.print_id handler
    | Pop_trap { handler } ->
      Format.fprintf ppf "pop_trap %a" Block.print_id handler
end

module Terminator = struct
  type nonrec 'g t = 'g terminator =
    | Continue of
        { continuation : 'g continuation;
          args : 'g value array
        }
    | Switch of
        { index : 'g value;
          targets : 'g block array
        }
    | Call of
        { op : call_op;
          args : 'g value array;
          continuation : 'g continuation;
          may_raise : bool;
          nontail : bool
        }
    | Invalid of
        { message : string;
          args : 'g value array;
          continuation : 'g block option
        }

  let print_args ppf args =
    Array.iteri
      (fun i arg ->
        if i > 0 then Format.fprintf ppf ", ";
        Value.print ppf arg)
      args

  let print ppf (t : 'g t) =
    match t with
    | Continue { continuation = Goto goto; args } ->
      Format.fprintf ppf "goto %a(%a)" Block.print_id goto print_args args
    | Continue { continuation = Return; args } ->
      Format.fprintf ppf "return(%a)" print_args args
    | Continue { continuation = Raise _; args } ->
      Format.fprintf ppf "raise(%a)" print_args args
    | Switch { index; targets } ->
      Format.fprintf ppf "switch(%a) [" Value.print index;
      Array.iteri
        (fun i tgt ->
          if i > 0 then Format.fprintf ppf ", ";
          Block.print_id ppf tgt)
        targets;
      Format.fprintf ppf "]"
    | Call { op; args; continuation; may_raise; nontail } ->
      let kind =
        match continuation with
        | Return -> "tailcall"
        | Goto _ | Raise _ -> "call"
      in
      (match op with
      | Direct sym ->
        Format.fprintf ppf "%s %s(%a)" kind sym.sym_name print_args args
      | Indirect _ -> Format.fprintf ppf "%s_indirect(%a)" kind print_args args
      | External { func_symbol; _ } ->
        Format.fprintf ppf "%s_prim %s(%a)" kind func_symbol print_args args
      | Probe { name; _ } ->
        Format.fprintf ppf "probe %s(%a)" name print_args args);
      (match continuation with
      | Goto b -> Format.fprintf ppf " -> %a" Block.print_id b
      | Return -> ()
      | Raise _ -> Format.fprintf ppf " -> raise");
      if may_raise then Format.fprintf ppf " may_raise";
      if nontail then Format.fprintf ppf " nontail"
    | Invalid { message; args; continuation } -> (
      Format.fprintf ppf "invalid(%a) \"%s\"" print_args args message;
      match continuation with
      | Some l -> Format.fprintf ppf " -> %a" Block.print_id l
      | None -> ())
end

let check_args_arity ~term_name ~args ~expected =
  if Array.length args <> Array.length expected
  then
    Misc.fatal_errorf "Ssa.check_args_arity: %s passes %d args but expected %d"
      term_name (Array.length args) (Array.length expected)

let check_target_has_no_params ~term_name (target : under_construction block) =
  if Array.length target.params > 0
  then
    Misc.fatal_errorf
      "Ssa.check_target_has_no_params: %s target must have no params" term_name

let check_terminator (term : under_construction terminator) : unit =
  match term with
  | Continue { continuation = Goto goto; args } ->
    check_args_arity ~term_name:"Continue" ~args ~expected:goto.params
  | Continue { continuation = Return | Raise _; _ } -> ()
  | Switch { targets; _ } ->
    Array.iter (check_target_has_no_params ~term_name:"Switch") targets
  | Call { continuation = Return; nontail; _ } ->
    if nontail
    then
      Misc.fatal_error
        "Ssa.check_terminator: a tail Call (continuation Return) cannot be \
         nontail"
  | Call { continuation = Raise _; _ } ->
    Misc.fatal_error "Ssa.check_terminator: a Call continuation cannot be Raise"
  | Call { continuation = Goto _; _ } | Invalid _ -> ()

module Cursor = struct
  type t = { mutable block : under_construction block }

  let start (block : under_construction Block.t) : t = { block }

  let is_finished (c : t) = c.block.terminator != pending_terminator

  let move (c : t) ~(new_pos : under_construction Block.t) : unit =
    assert (is_finished c);
    c.block <- new_pos

  let emit_op (graph : under_construction graph) (c : t) (op : op)
      (dbg : Debuginfo.t) (typ : Cmm.machtype)
      (args : under_construction value array) : under_construction value array =
    let operation =
      { id = Instruction_id.get_and_incr graph.instruction_id_gen;
        op;
        typ;
        args;
        dbg;
        usage_count = 0;
        name = None
      }
    in
    c.block.pending_body <- Op operation :: c.block.pending_body;
    Array.init (Array.length typ) (fun i -> Res (operation, i))

  let emit_push_trap (c : t) ~(handler : under_construction Block.t) =
    c.block.pending_body <- Push_trap { handler } :: c.block.pending_body

  let emit_pop_trap (c : t) ~(handler : under_construction Block.t) =
    c.block.pending_body <- Pop_trap { handler } :: c.block.pending_body

  let finish_block (graph : under_construction graph) (c : t)
      ~(dbg : Debuginfo.t) (term : under_construction terminator) : unit =
    let block = c.block in
    if is_finished c
    then Misc.fatal_error "Ssa.Cursor.finish_block: block already finished";
    check_terminator term;
    block.pending_body <- List.rev block.pending_body;
    block.terminator <- term;
    block.terminator_dbg <- dbg;
    graph.finished_blocks_rev <- block :: graph.finished_blocks_rev
end

let function_info (graph : 'g graph) = graph.function_info

let entry (graph : 'g graph) : 'g Block.t = graph.entry

let blocks (graph : finished graph) : finished Block.t list = graph.blocks

let create_graph (function_info : Function_info.t) ~keep_unused_ops :
    under_construction graph =
  let block_id_gen = Block_id.create_generator () in
  let instruction_id_gen = Instruction_id.create_generator () in
  let entry =
    create_block ~block_id_gen ~is_function_start:true
      ~params:
        (Function_info.flattened_parameters function_info
        |> Array.map (fun typ : block_param ->
            { typ; name = None; usage_count = 0 }))
  in
  (* Initialize names from the function's argument names, when known. *)
  List.iteri
    (fun i (var, _ty) ->
      if i < Array.length entry.params
      then
        entry.params.(i).name
          <- Some (Backend_var.name (Backend_var.With_provenance.var var)))
    function_info.parameters;
  { function_info;
    keep_unused_ops;
    block_id_gen;
    instruction_id_gen;
    entry;
    finished_blocks_rev = [];
    blocks = [];
    finished = false
  }

(* === Compute metadata: predecessors, traps, dominators, use counts === *)

(* Apply the [Push_trap]/[Pop_trap] effects of [body] to [start_stack]. Each
   [Pop_trap { handler = h }] must find [h] at the top of the current stack. *)
let compute_block_end_trap_stack ~(block : finished block)
    (start_stack : finished block list) (body : finished instruction list) :
    finished block list =
  List.fold_left
    (fun (stack : finished block list) (instr : finished instruction) ->
      match instr with
      | Push_trap { handler = h } -> h :: stack
      | Pop_trap { handler = h } -> (
        match stack with
        | [] ->
          Misc.fatal_errorf
            "Ssa.compute_block_end_trap_stack: block %a pops handler %a off an \
             empty trap stack"
            Block.print_id block Block.print_id h
        | top :: rest ->
          if not (Block.equal top h)
          then
            Misc.fatal_errorf
              "Ssa.compute_block_end_trap_stack: block %a pops handler %a but \
               top of trap stack is %a"
              Block.print_id block Block.print_id h Block.print_id top;
          rest)
      | Op _ -> stack)
    start_stack body

(* Forward search from [entry]: populates [predecessors] on each reachable
   block, computes [block_end_trap_stack] for each visited block (also needed to
   derive exception successors), and verifies that every reachable block has
   been finished. *)
let compute_reachability_and_trap_stacks (graph : finished graph) :
    finished block list =
  let visited = Block.Tbl.create 64 in
  let worklist = ref [graph.entry, []] in
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
      let add_pred_to (succ : finished block) trap_stack =
        succ.predecessors <- block :: succ.predecessors;
        worklist := (succ, trap_stack) :: !worklist
      in
      Block.non_exn_successors_of_terminator block.terminator
      |> List.iter (fun succ -> add_pred_to succ end_stack);
      (* On entry to a trap handler, the runtime has popped the topmost handler
         off the trap stack. *)
      Block.exn_successor block
      |> Option.iter (fun succ -> add_pred_to succ (List.tl end_stack))
    end
  done;
  let reachable_blocks =
    List.filter
      (fun block -> Block.Tbl.mem visited block)
      (List.rev graph.finished_blocks_rev)
  in
  let unfinished_blocks =
    Block.Set.diff
      (Block.Tbl.to_seq_keys visited |> Block.Set.of_seq)
      (Block.Set.of_list graph.finished_blocks_rev)
  in
  unfinished_blocks
  |> Block.Set.iter (fun unfinished ->
      Misc.fatal_errorf
        "Ssa.compute_reachability_and_trap_stacks: reachable block %a was \
         never finished"
        Block.print_id unfinished);
  reachable_blocks

let compute_dominators (graph : finished graph)
    ~(reachable_blocks : finished block list) : unit =
  let entry = graph.entry in
  entry.dominator_info <- { depth = 0; dominator = entry };
  let has_idom (block : finished block) = block.dominator_info.depth >= 0 in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun (block : finished block) ->
        if not (Block.equal block entry)
        then begin
          let new_dominator =
            List.fold_left
              (fun acc pred ->
                if has_idom pred
                then
                  match acc with
                  | None -> Some pred
                  | Some current -> Some (Block.common_dominator pred current)
                else acc)
              (if has_idom block
               then Some block.dominator_info.dominator
               else None)
              block.predecessors
          in
          new_dominator
          |> Option.iter (fun new_dominator ->
              let new_info : finished dominator_info =
                { depth = new_dominator.dominator_info.depth + 1;
                  dominator = new_dominator
                }
              in
              if block.dominator_info.depth <> new_info.depth
              then begin
                block.dominator_info <- new_info;
                changed := true
              end
              else
                assert (
                  Block.equal block.dominator_info.dominator new_info.dominator))
        end)
      reachable_blocks
  done

(* Reference-counting walks over each reachable block's body and terminator. An
   unused operation does not increase the use counts of its inputs.
   [Block_param] increments propagate to predecessors' [Continue] args when the
   parameter becomes used. *)

let rec increment_use ~keep_unused_ops (value : finished value) =
  match value with
  | Res (op, _) -> increment_operation_use ~keep_unused_ops op
  | Undefined -> ()
  | Block_param (block, i) ->
    let p = block.params.(i) in
    let old = p.usage_count in
    p.usage_count <- old + 1;
    if (not keep_unused_ops) && old = 0
    then
      block.predecessors
      |> List.iter (fun (pred : finished block) ->
          match pred.terminator with
          | Continue { continuation = Goto _; args } ->
            increment_use ~keep_unused_ops args.(i)
          | Continue { continuation = Return | Raise _; _ }
          | Switch _ | Call _ | Invalid _ ->
            (* The arg feeding this param arrives through an edge whose position
               is fixed (call results, exception bucket), so it is counted
               unconditionally where that terminator is visited. *)
            ())

and increment_operation_use ~keep_unused_ops (r : finished op_data) =
  r.usage_count <- r.usage_count + 1;
  if (not keep_unused_ops) && r.usage_count = 1
  then Array.iter (increment_use ~keep_unused_ops) r.args

let increment_uses_in_terminator ~keep_unused_ops (term : finished terminator) =
  match term with
  | Continue { continuation = Goto _; args } ->
    (* An ordinary [Continue]'s args are counted through block-param use
       propagation; only count them all here when keeping unused ops. *)
    if keep_unused_ops then Array.iter (increment_use ~keep_unused_ops) args
  | Continue { continuation = Return | Raise _; args } ->
    Array.iter (increment_use ~keep_unused_ops) args
  | Switch { index; _ } -> increment_use ~keep_unused_ops index
  | Call { args; _ } | Invalid { args; _ } ->
    Array.iter (increment_use ~keep_unused_ops) args

(* For each block, count uses originating from its body's must-keep operations
   and from its terminator. Operations that are [removable_when_unused] only
   acquire uses transitively, through the args of operations that are themselves
   used. *)
let increment_uses_in_block ~keep_unused_ops (block : finished block) =
  block.pending_body
  |> List.iter (fun (instr : finished instruction) ->
      match instr with
      | Op ({ args; _ } as operation) ->
        if keep_unused_ops then Array.iter (increment_use ~keep_unused_ops) args;
        if not (Instruction.removable_when_unused instr)
        then increment_operation_use ~keep_unused_ops operation
      | Push_trap _ | Pop_trap _ -> ());
  increment_uses_in_terminator ~keep_unused_ops block.terminator

(** Materialise [body] from [pending_body], with two pieces of cleanup:
    - filter out [Op]s with [usage_count = 0] (skipped when [keep_unused_ops]);
    - drop [Push_trap]/[Pop_trap] whose handler has no predecessors (i.e. is
      never raised into) — the matching push/pop reference the same handler and
      are dropped together, preserving trap-stack balance; and, when
      [keep_unused_ops] is false, replace [Continue (Goto _)] args going to
      unused target params with [Undefined]. (The entry params are pre-marked as
      used, so a self-tail call's args are never dropped here.) *)
let finalize_block ~keep_unused_ops (block : finished block) =
  block.body
    <- block.pending_body
       |> List.filter (fun (instr : finished instruction) : bool ->
           match instr with
           | Op { usage_count; _ } -> keep_unused_ops || usage_count > 0
           | Push_trap { handler } | Pop_trap { handler } ->
             not (List.is_empty handler.predecessors))
       |> Array.of_list;
  block.pending_body <- [];
  if not keep_unused_ops
  then
    match block.terminator with
    | Continue { continuation = Goto goto; args } ->
      let args =
        args
        |> Array.mapi (fun i arg ->
            (* Arguments passed to unused block parameters could themselves be
               unused and already be dropped from the graph. So we replace such
               arguments with Undefined, to avoid dangling uses.*)
            if goto.params.(i).usage_count = 0 then Undefined else arg)
      in
      block.terminator <- Continue { continuation = Goto goto; args }
    | Continue { continuation = Return | Raise _; _ }
    | Switch _ | Call _ | Invalid _ ->
      ()

(* Walk [blocks] in the input order, but when first visiting a block emit its
   dominator chain ahead of it. The result has every block preceded by its
   dominators, while keeping the input order wherever the dominator constraint
   doesn't force a move. Requires [dominator_info] to already be populated. *)
let order_blocks_dominators_first (blocks : finished block list) :
    finished block list =
  let visited = Block.Tbl.create 64 in
  let acc = ref [] in
  let rec visit (block : finished block) =
    if not (Block.Tbl.mem visited block)
    then begin
      Block.Tbl.add visited block ();
      let dom = block.dominator_info.dominator in
      if block.dominator_info.depth > 0 then visit dom;
      acc := block :: !acc
    end
  in
  List.iter visit blocks;
  List.rev !acc

let compute_metadata (graph : finished graph) : finished block list =
  let keep_unused_ops = graph.keep_unused_ops in
  let reachable_blocks = compute_reachability_and_trap_stacks graph in
  compute_dominators graph ~reachable_blocks;
  (* The function's entry parameters come from the ABI: count them as used, so
     they are never dropped. *)
  Array.iteri
    (fun i _ -> increment_use ~keep_unused_ops (Block.param graph.entry i))
    graph.entry.params;
  reachable_blocks |> List.iter (increment_uses_in_block ~keep_unused_ops);
  reachable_blocks |> List.iter (finalize_block ~keep_unused_ops);
  order_blocks_dominators_first reachable_blocks

let finish_graph (graph : under_construction graph) : finished graph =
  assert (not graph.finished);
  graph.finished <- true;
  graph.blocks <- compute_metadata graph;
  graph

module Export = struct
  module Instruction = Instruction
  module Value = Value
  module Terminator = Terminator
  module Block = Block
  module Cursor = Cursor

  type nonrec finished = finished

  type nonrec under_construction = under_construction
end
