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

(** SSA graph implementation.

    A graph is a plain value of type ['g graph]. The construction-state tags
    [under_construction] and [finished] are defined as the same here, so the
    phantom collapses inside this module: a graph in either state has the same
    representation and [finish_graph] fills in metadata and returns the same
    value as type [finished graph]. The signature keeps the tags abstract, so to
    every other module ['g] genuinely distinguishes the two states across
    [Block.t] / [Instruction.t] / [Value.t] / [Terminator.t].

    An [Instruction.t] is a body element of a basic block ([Op] / [Push_trap] /
    [Pop_trap]); a [Value.t] is an argument of an instruction. [Block_param]
    values refer to the parameters of a basic block, which are passed by a
    [Continue] block terminator. This is an alternative to phi-functions in
    other SSA representations. [Res] values are only produced by [emit_op]
    (which returns one per result), so they always point to an emitted
    instruction with an in-range output index.

    Construction appends each emitted instruction onto the current block's
    [pending_body] (newest-first); [finish_block] seals the terminator, and
    records the block. [finish_graph] then fills in metadata and produces the
    final immutable [body] array.

    [finish_graph] performs five passes over the finished blocks, in order:
    - [compute_reachability_and_trap_stacks]: forward DFS from [entry] following
      structural and exception successors, threading the trap stack so each
      reachable block gets its [block_end_trap_stack] populated; unreachable
      blocks are pruned.
    - [compute_dominators]: iterative meet-over-predecessors fixpoint.
    - [increment_uses_in_block]: refcount over op args and block params, leaving
      anything dead at the [-1] "scheduled for removal" sentinel (its inputs are
      not counted, so a transitively-dead chain stays at [-1]). Block-param
      liveness propagates to predecessors' [Continue] args.
    - [finalize_block]: materialise [body] from [pending_body]. Always drops
      Push_trap/Pop_trap pairs whose handler is unreachable. When
      [keep_unused_ops] is false, also drops dead [Op]s ([-1]) and replaces
      [Continue (Goto _)] args going to params scheduled for removal with the
      [Undefined] value.
    - [check_value_invariants]: every use is dominated by its definition, and
      [Undefined] only feeds block params scheduled for removal. *)

module Block_id = Oxcaml_utils.Id_counter.Make ()
module Instruction_id = Oxcaml_utils.Id_counter.Make ()

(* The two construction-state tags are the same type here; the signature keeps
   them distinct. *)
type finished

type under_construction = finished

type usage_count = int

(* [usage_count] sentinel set by the metadata pass on a value that is unused and
   whose inputs it therefore did not count: the value is scheduled for removal
   (an [Op] is pruned; a block param is dropped by [Ssa_reducer]). A count of
   [0] means kept with no remaining uses (a side-effecting / existence-only op,
   or a forced-kept param); [n > 0] means live with [n] counted uses. *)
let removal_sentinel = -1

(* Hide [removal_sentinel] behind the public count of [0]. *)
let visible_usage_count (c : usage_count) : int =
  if c = removal_sentinel then 0 else c

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
  | Unreachable

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

(** Sentinels used as the initial [terminator] of a freshly created block, and
    as the placeholder dominator. [Block.is_finished] checks for
    [pending_terminator]. Doing it like this keeps the publicly visible
    [finished Terminator.t] type clean, as it cannot contain
    [pending_terminator]. *)
let rec pending_terminator : under_construction terminator =
  Continue { continuation = Goto dummy_block; args = [||] }

and dummy_block : under_construction block =
  { block_id = Block_id.dummy;
    params = [||];
    predecessors = [];
    body = [||];
    pending_body = [];
    terminator = pending_terminator;
    terminator_dbg = Debuginfo.none;
    dominator_info = { depth = -1; dominator = dummy_block };
    block_end_trap_stack = []
  }

let create_block ~block_id_gen ~(params : block_param array) :
    under_construction block =
  { block_id = Block_id.get_and_incr block_id_gen;
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

  let create (graph : under_construction graph) ~(params : Cmm.machtype) :
      under_construction t =
    create_block ~block_id_gen:graph.block_id_gen
      ~params:
        (params
        |> Array.map (fun typ ->
            { typ; name = None; usage_count = removal_sentinel }))

  let create_with_names (graph : under_construction graph)
      ~(params : (Cmm.machtype_component * string option) array) :
      under_construction t =
    create_block ~block_id_gen:graph.block_id_gen
      ~params:
        (params
        |> Array.map (fun (typ, name) ->
            { typ; name; usage_count = removal_sentinel }))

  let param (block : 'g t) index : 'g value =
    if index < 0 || index >= Array.length block.params
    then
      Misc.fatal_errorf
        "Ssa.Block.param: index %d out of range for block with %d params" index
        (Array.length block.params);
    Block_param (block, index)

  let params (block : 'g t) : 'g value array =
    Array.init (Array.length block.params) (fun i -> Block_param (block, i))

  let equal (a : 'g t) (b : 'g t) =
    (* Blocks have mutable fields, so physical equality is safe. *)
    a == b

  let compare (a : 'g t) (b : 'g t) = Block_id.compare a.block_id b.block_id

  let hash (block : 'g t) = Block_id.hash block.block_id

  module Self = struct
    type t = finished block

    let equal = (equal : t -> t -> bool)

    let compare = (compare : t -> t -> int)

    let hash = (hash : t -> int)
  end

  module Map = Map.Make (Self)
  module Set = Set.Make (Self)
  module Tbl = Hashtbl.Make (Self)

  let print_id ppf (block : 'g t) =
    Format.fprintf ppf "B%d" (block.block_id :> int)

  let may_raise (block : finished t) =
    match block.terminator with
    | Continue { continuation = Raise _; _ } -> true
    | Call { may_raise; _ } -> may_raise
    | Continue { continuation = Goto _ | Return | Unreachable; _ }
    | Switch _ | Invalid _ ->
      false

  let predecessors (block : finished t) : finished t list = block.predecessors

  let params_machtype (block : 'g t) : Cmm.machtype =
    Array.map (fun (p : block_param) -> p.typ) block.params

  let non_exn_successors_of_continuation (cont : 'g continuation) :
      'g block list =
    match cont with
    | Goto block -> [block]
    | Return | Raise _ | Unreachable -> []

  (* The terminator is missing the exception successors, which are derived from
     [block_end_trap_stack]. *)
  let non_exn_successors_of_terminator (t : 'g terminator) : 'g block list =
    match t with
    | Continue { continuation; _ } | Call { continuation; _ } ->
      non_exn_successors_of_continuation continuation
    | Switch { targets; _ } -> Array.to_list targets
    | Invalid { continuation; _ } -> (
      match continuation with Some l -> [l] | None -> [])

  let non_exn_successors (block : finished t) : finished t list =
    non_exn_successors_of_terminator block.terminator

  let exn_successor (block : finished t) : finished t option =
    match block.block_end_trap_stack with
    | h :: _ when may_raise block -> Some h
    | _ -> None

  let block_end_trap_stack (block : finished t) : finished t list =
    block.block_end_trap_stack

  let successors (block : finished t) : finished t list =
    let structural = non_exn_successors_of_terminator block.terminator in
    match exn_successor block with
    | None -> structural
    | Some h -> h :: structural

  let dominates (a : finished t) (b : finished t) =
    let rec loop a b =
      equal a b
      || b.dominator_info.depth > a.dominator_info.depth
         && loop a b.dominator_info.dominator
    in
    assert (a.dominator_info.depth >= 0 && b.dominator_info.depth >= 0);
    loop a b

  let common_dominator (a : finished t) (b : finished t) : finished t =
    let rec loop a b =
      if equal a b
      then a
      else if a.dominator_info.depth > b.dominator_info.depth
      then loop a.dominator_info.dominator b
      else if b.dominator_info.depth > a.dominator_info.depth
      then loop a b.dominator_info.dominator
      else loop a.dominator_info.dominator b.dominator_info.dominator
    in
    assert (a.dominator_info.depth >= 0 && b.dominator_info.depth >= 0);
    loop a b

  let immediate_dominator (block : finished t) : finished t =
    assert (block.dominator_info.depth >= 0);
    block.dominator_info.dominator

  let dominator_depth (block : finished t) : int =
    assert (block.dominator_info.depth >= 0);
    block.dominator_info.depth

  let body (block : finished t) : finished instruction array = block.body

  let terminator (block : finished t) : finished terminator = block.terminator

  let terminator_dbg (block : finished t) : Debuginfo.t = block.terminator_dbg

  let is_finished block =
    match block.terminator with
    | Continue { continuation = Goto block; _ } -> block != dummy_block
    | Continue { continuation = Return | Raise _ | Unreachable; _ }
    | Invalid _ | Call _ | Switch _ ->
      true
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

  (* Keep an existing name: when a value is reused for another binding (e.g.
     [let y = x] or a reducer substituting an existing value), the original name
     describes it best. *)
  let set_name (value : 'g t) name =
    match value with
    | Res (r, _) -> if Option.is_none r.name then r.name <- Some name
    | Block_param (block, i) ->
      if Option.is_none block.params.(i).name
      then block.params.(i).name <- Some name
    | Undefined -> ()

  let name (value : 'g t) : string option =
    match value with
    | Res ({ name; _ }, _) -> name
    | Block_param (block, i) -> block.params.(i).name
    | Undefined -> None

  let usage_count (value : finished t) : int =
    match value with
    | Res ({ usage_count; _ }, _) -> visible_usage_count usage_count
    | Block_param (block, i) -> visible_usage_count block.params.(i).usage_count
    | Undefined -> 0

  let scheduled_for_removal (value : finished t) : bool =
    match value with
    | Res ({ usage_count; _ }, _) -> usage_count = removal_sentinel
    | Block_param (block, i) -> block.params.(i).usage_count = removal_sentinel
    | Undefined -> false
end

module Instruction = struct
  module Id = struct
    type 'g t = Instruction_id.t

    let equal = Instruction_id.equal

    module Tbl = struct
      type 'g key = 'g t

      type ('g, 'a) t = 'a Instruction_id.Tbl.t

      let create n : (_, _) t = Instruction_id.Tbl.create n

      let find tbl key = Instruction_id.Tbl.find tbl key

      let find_opt tbl key = Instruction_id.Tbl.find_opt tbl key

      let replace tbl key value = Instruction_id.Tbl.replace tbl key value
    end
  end

  type nonrec 'g op_data = 'g op_data =
    { id : 'g Id.t;
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

  let usage_count (op : finished op_data) : int =
    visible_usage_count op.usage_count

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

  let print_args ppf arr =
    Format.pp_print_array
      ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
      Value.print ppf arr

  let print ppf (t : 'g t) =
    match t with
    | Continue { continuation = Goto goto; args } ->
      Format.fprintf ppf "goto %a(%a)" Block.print_id goto print_args args
    | Continue { continuation = Return; args } ->
      Format.fprintf ppf "return(%a)" print_args args
    | Continue { continuation = Raise _; args } ->
      Format.fprintf ppf "raise(%a)" print_args args
    | Continue { continuation = Unreachable; args } ->
      Format.fprintf ppf "unreachable(%a)" print_args args
    | Switch { index; targets } ->
      Format.fprintf ppf "switch(%a)" Value.print index;
      Array.iteri
        (fun i block -> Format.fprintf ppf " | %i -> %a" i Block.print_id block)
        targets
    | Call { op; args; continuation; may_raise; nontail } -> (
      let kind =
        match continuation with
        | Return -> "tailcall"
        | Goto _ | Raise _ | Unreachable -> "call"
      in
      (match op with
      | Direct sym ->
        Format.fprintf ppf "%s %s(%a)" kind sym.sym_name print_args args
      | Indirect _ -> Format.fprintf ppf "%s_indirect(%a)" kind print_args args
      | External { func_symbol; _ } ->
        Format.fprintf ppf "%s_prim %s(%a)" kind func_symbol print_args args
      | Probe { name; _ } ->
        Format.fprintf ppf "probe %s(%a)" name print_args args);
      if may_raise then Format.fprintf ppf " may_raise";
      if nontail then Format.fprintf ppf " nontail";
      match continuation with
      | Goto b -> Format.fprintf ppf " -> %a" Block.print_id b
      | Return -> ()
      | Raise _ -> Format.fprintf ppf " -> raise"
      | Unreachable -> Format.fprintf ppf " -> unreachable")
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
  | Continue { continuation = Unreachable; _ } ->
    Misc.fatal_error
      "Ssa.check_terminator: a Continue continuation cannot be Unreachable"
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
  | Call { continuation = Unreachable; op = External _; _ } -> ()
  | Call { continuation = Unreachable; op = Direct _ | Indirect _ | Probe _; _ }
    ->
    Misc.fatal_error
      "Ssa.check_terminator: a Call with continuation Unreachable must be an \
       external call"
  | Call { continuation = Goto _; _ } | Invalid _ -> ()

module Cursor = struct
  type t = { mutable block : under_construction block }

  let start (block : under_construction Block.t) : t = { block }

  let is_finished (c : t) = Block.is_finished c.block

  let move (c : t) ~(new_pos : under_construction Block.t) : unit =
    assert (is_finished c);
    c.block <- new_pos

  (* Emitting through a cursor whose block was already finished would silently
     prepend to the (reversed-in-place by [finish_block]) body, so fail
     loudly. *)
  let check_not_finished (c : t) ~fun_name =
    if is_finished c
    then Misc.fatal_errorf "Ssa.Cursor.%s: block already finished" fun_name

  let emit_op (graph : under_construction graph) (c : t) (op : op)
      (dbg : Debuginfo.t) (typ : Cmm.machtype)
      (args : under_construction value array) : under_construction value array =
    check_not_finished c ~fun_name:"emit_op";
    let operation =
      { id = Instruction_id.get_and_incr graph.instruction_id_gen;
        op;
        typ;
        args;
        dbg;
        usage_count = removal_sentinel;
        name = None
      }
    in
    c.block.pending_body <- Op operation :: c.block.pending_body;
    Array.init (Array.length typ) (fun i -> Res (operation, i))

  let emit_push_trap (c : t) ~(handler : under_construction Block.t) =
    check_not_finished c ~fun_name:"emit_push_trap";
    c.block.pending_body <- Push_trap { handler } :: c.block.pending_body

  let emit_pop_trap (c : t) ~(handler : under_construction Block.t) =
    check_not_finished c ~fun_name:"emit_pop_trap";
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
    create_block ~block_id_gen
      ~params:
        (Function_info.flattened_parameters function_info
        |> Array.map (fun typ : block_param ->
            { typ; name = None; usage_count = removal_sentinel }))
  in
  (* Initialize names from the function's argument names; each parameter covers
     one flattened entry param per machtype component. *)
  let (_ : int) =
    List.fold_left
      (fun pos (var, (ty : Cmm.machtype)) ->
        let name = Backend_var.name (Backend_var.With_provenance.var var) in
        for i = 0 to Array.length ty - 1 do
          entry.params.(pos + i).name <- Some name
        done;
        pos + Array.length ty)
      0 function_info.parameters
  in
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
   been finished and that all edges into a block agree on the trap stack at its
   entry. *)
let compute_reachability_and_trap_stacks (graph : finished graph) :
    finished block list =
  let print_trap_stack ppf stack =
    Format.fprintf ppf "[%a]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
         Block.print_id)
      stack
  in
  let visited : finished block list Block.Tbl.t = Block.Tbl.create 64 in
  let worklist = ref [graph.entry, []] in
  while not (List.is_empty !worklist) do
    let block, start_stack = List.hd !worklist in
    worklist := List.tl !worklist;
    match Block.Tbl.find_opt visited block with
    | Some recorded_start_stack ->
      (* The trap stack at entry must agree across all incoming edges (the
         exception edge into a handler already accounts for the runtime popping
         the handler). *)
      if not (List.equal Block.equal start_stack recorded_start_stack)
      then
        Misc.fatal_errorf
          "Ssa.compute_reachability_and_trap_stacks (%s): block %a is entered \
           with mismatching trap stacks %a and %a"
          graph.function_info.sym_name Block.print_id block print_trap_stack
          recorded_start_stack print_trap_stack start_stack
    | None ->
      if not (Block.is_finished block)
      then
        Misc.fatal_errorf
          "Ssa.compute_reachability_and_trap_stacks (%s): reachable block %a \
           was never finished"
          graph.function_info.sym_name Block.print_id block;
      Block.Tbl.add visited block start_stack;
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
  done;
  List.filter
    (fun block -> Block.Tbl.mem visited block)
    (List.rev graph.finished_blocks_rev)

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

(* Reference-counting walks over each reachable block's body and terminator.
   Counts start at [removal_sentinel]; marking a value live ([removal_sentinel]
   -> [0]) counts its inputs exactly once, then each use adds [1]. A value left
   at [removal_sentinel] is dead: its inputs were never counted, so a
   transitively-dead chain stays scheduled for removal. A [Block_param] becoming
   live propagates to its predecessors' [Continue (Goto _)] args. *)

let rec increment_use (value : finished value) =
  match value with
  | Res (op, _) ->
    mark_op_live op;
    op.usage_count <- op.usage_count + 1
  | Undefined -> ()
  | Block_param (block, i) ->
    mark_param_live block i;
    let p = block.params.(i) in
    p.usage_count <- p.usage_count + 1

and mark_op_live (op : finished op_data) =
  (* Transition a dead op to kept ([0]), counting its inputs once; the guard
     also breaks cycles. *)
  if op.usage_count = removal_sentinel
  then begin
    op.usage_count <- 0;
    Array.iter increment_use op.args
  end

and mark_param_live (block : finished block) i =
  let p = block.params.(i) in
  if p.usage_count = removal_sentinel
  then begin
    p.usage_count <- 0;
    block.predecessors
    |> List.iter (fun (pred : finished block) ->
        match pred.terminator with
        | Continue { continuation = Goto _; args } -> increment_use args.(i)
        | Continue { continuation = Return | Raise _ | Unreachable; _ }
        | Switch _ | Call _ | Invalid _ ->
          (* The arg feeding this param arrives through an edge whose position
             is fixed (call results, exception bucket), so it is counted
             unconditionally where that terminator is visited. *)
          ())
  end

let increment_uses_in_terminator (block : finished block) =
  (* A successor reached by anything other than a positional [Continue (Goto _)]
     gets its params at a runtime-fixed position — a [Call]/[Invalid] result, or
     the exception bucket of the handler entered by a [Call] or [Raise] — so
     those params cannot be dropped; force-keep them. *)
  let keep_successor_params (succ : finished block) =
    Array.iteri (fun i _ -> mark_param_live succ i) succ.params
  in
  match block.terminator with
  (* A [Goto]'s args feed the target's params and are counted lazily, when the
     corresponding param is marked live (see [mark_param_live]). *)
  | Continue { continuation = Goto _; args = _ } -> ()
  | Continue { continuation = Return | Unreachable; args } ->
    Array.iter increment_use args
  | Continue { continuation = Raise _; args } ->
    Array.iter increment_use args;
    Block.exn_successor block |> Option.iter keep_successor_params
  | Switch { index; _ } -> increment_use index
  | Call { args; continuation; _ } ->
    Array.iter increment_use args;
    (match continuation with
    | Goto cont -> keep_successor_params cont
    | Return | Raise _ | Unreachable -> ());
    Block.exn_successor block |> Option.iter keep_successor_params
  | Invalid { args; continuation; _ } ->
    Array.iter increment_use args;
    Option.iter keep_successor_params continuation

(** Mark live the values a block must keep — every op that is not
    [removable_when_unused], plus terminator args; this can lead to transitive
    marking through [increment_use] and [mark_param_live]/[mark_op_live]. With
    [keep_unused_ops], mark every op and param live so nothing is pruned. *)
let increment_uses_in_block ~keep_unused_ops (block : finished block) =
  block.pending_body
  |> List.iter (fun (instr : finished instruction) ->
      match instr with
      | Op operation ->
        if keep_unused_ops || not (Instruction.removable_when_unused instr)
        then mark_op_live operation
      | Push_trap _ | Pop_trap _ -> ());
  if keep_unused_ops
  then Array.iteri (fun i _ -> mark_param_live block i) block.params;
  increment_uses_in_terminator block

(** Materialise [body] from [pending_body], with two pieces of cleanup:
    - filter out [Op]s scheduled for removal (count is [removal_sentinel]);
    - drop [Push_trap]/[Pop_trap] whose handler has no predecessors (i.e. is
      never raised into) — the matching push/pop reference the same handler and
      are dropped together, preserving trap-stack balance; the same handlers are
      filtered out of [block_end_trap_stack] so it stays consistent with the
      finalized body; and replace [Continue (Goto _)] args going to params
      scheduled for removal with [Undefined]. (With [keep_unused_ops] nothing is
      scheduled for removal, so both rewrites are no-ops; entry params are kept,
      so a self-tail call's args are never dropped here.) *)
let finalize_block (block : finished block) =
  let handler_reachable handler = not (List.is_empty handler.predecessors) in
  block.body
    <- block.pending_body
       |> List.filter (fun (instr : finished instruction) : bool ->
           match instr with
           | Op { usage_count; _ } -> usage_count <> removal_sentinel
           | Push_trap { handler } | Pop_trap { handler } ->
             handler_reachable handler)
       |> Array.of_list;
  block.pending_body <- [];
  block.block_end_trap_stack
    <- List.filter handler_reachable block.block_end_trap_stack;
  match block.terminator with
  | Continue { continuation = Goto goto; args } ->
    let args =
      args
      |> Array.mapi (fun i arg ->
          (* Args feeding a param scheduled for removal may themselves be dead
             (already pruned from the graph), so replace them with Undefined to
             avoid dangling uses. *)
          if goto.params.(i).usage_count = removal_sentinel
          then Undefined
          else arg)
    in
    block.terminator <- Continue { continuation = Goto goto; args }
  | Continue { continuation = Return | Raise _ | Unreachable; _ }
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

(* Check the SSA invariants that the construction interface cannot enforce and
   that no other pass checks: every value use must be dominated by its
   definition, [Undefined] may only appear as a [Goto] argument feeding an
   unused block parameter, and trap handler arities are consistent ([Raise]
   argument counts match their handler's parameter count; handlers entered from
   calls take only the exception bucket). Requires [ordered_blocks] to list
   every block after its dominators (so that, walking in order, the definitions
   dominating a use have already been seen). *)
let check_value_invariants (graph : finished graph)
    (ordered_blocks : finished block list) : unit =
  let fail fmt =
    Format.kasprintf
      (fun s ->
        Misc.fatal_errorf "Ssa.check_value_invariants (%s): %s"
          graph.function_info.sym_name s)
      fmt
  in
  let defining_block : finished block Instruction_id.Tbl.t =
    Instruction_id.Tbl.create 64
  in
  let check_use (user : finished block) (v : finished value) =
    match v with
    | Res (op, _) -> (
      match Instruction_id.Tbl.find_opt defining_block op.id with
      | Some def_block ->
        if not (Block.dominates def_block user)
        then
          fail "block %a: use of %a, defined in non-dominating block %a"
            Block.print_id user Value.print v Block.print_id def_block
      | None ->
        fail "block %a: use of undefined value %a" Block.print_id user
          Value.print v)
    | Block_param (param_block, _) ->
      if not (Block.dominates param_block user)
      then
        fail "block %a: use of %a of non-dominating block %a" Block.print_id
          user Value.print v Block.print_id param_block
    | Undefined ->
      fail
        "block %a: Undefined is only allowed as a Goto argument feeding an \
         unused block parameter"
        Block.print_id user
  in
  let check_instr block = function
    | Op { id; args; _ } ->
      Array.iter (check_use block) args;
      Instruction_id.Tbl.replace defining_block id block
    | Push_trap _ | Pop_trap _ -> ()
  in
  let check_terminator block = function
    | Continue { continuation = Goto goto; args } ->
      args
      |> Array.iteri (fun i (arg : finished value) ->
          match arg with
          | Undefined ->
            if goto.params.(i).usage_count <> removal_sentinel
            then
              fail "block %a: Undefined passed to live parameter %d of %a"
                Block.print_id block i Block.print_id goto
          | Res _ | Block_param _ -> check_use block arg)
    | Continue { continuation = Raise _; args } ->
      Array.iter (check_use block) args;
      (* Extra raise args are passed by moving them into the target handler's
         parameter registers, so their number must match the handler's parameter
         count. A raise that leaves the function (empty trap stack) can only
         carry the exception bucket: the runtime unwinding mechanism transports
         nothing else. *)
      let expected =
        match Block.exn_successor block with
        | Some handler -> Array.length handler.params
        | None -> 1
      in
      if Array.length args <> expected
      then
        fail "block %a: raise passes %d values but its handler expects %d"
          Block.print_id block (Array.length args) expected
    | Call { args; continuation; _ } -> (
      Array.iter (check_use block) args;
      (* Tail calls are lowered without popping trap handlers, so the trap stack
         must already be empty. *)
      (match continuation with
      | Return ->
        if not (List.is_empty block.block_end_trap_stack)
        then
          fail "block %a: tail call with a non-empty trap stack" Block.print_id
            block
      | Goto _ | Raise _ | Unreachable -> ());
      (* A raising call delivers only the exception bucket to its handler:
         handlers with extra parameters can only be entered from explicit raises
         ([to_cmm] guarantees they are never top of the trap stack across a
         call, by wrapping such calls in a re-raising handler without extra
         args). *)
      match Block.exn_successor block with
      | Some handler ->
        if Array.length handler.params <> 1
        then
          fail
            "block %a: handler %a of a call must take exactly one parameter \
             (the exception bucket), but takes %d"
            Block.print_id block Block.print_id handler
            (Array.length handler.params)
      | None -> ())
    | Continue { continuation = Return | Unreachable; args }
    | Invalid { args; _ } ->
      Array.iter (check_use block) args
    | Switch { index; _ } -> check_use block index
  in
  ordered_blocks
  |> List.iter (fun block ->
      block.body |> Array.iter (check_instr block);
      check_terminator block block.terminator)

let compute_metadata (graph : finished graph) : finished block list =
  let keep_unused_ops = graph.keep_unused_ops in
  let reachable_blocks = compute_reachability_and_trap_stacks graph in
  compute_dominators graph ~reachable_blocks;
  (* Entry params are ABI-fixed and never dropped: force-keep them (they would
     otherwise be left scheduled for removal) and, in doing so, count the args
     feeding them on any back-edge, e.g. a self-recursive tail call. *)
  for i = 0 to Array.length graph.entry.params - 1 do
    mark_param_live graph.entry i
  done;
  reachable_blocks |> List.iter (increment_uses_in_block ~keep_unused_ops);
  reachable_blocks |> List.iter finalize_block;
  let ordered_blocks = order_blocks_dominators_first reachable_blocks in
  check_value_invariants graph ordered_blocks;
  ordered_blocks

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
