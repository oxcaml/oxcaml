[@@@ocaml.warning "+a-30-40-41-42"]

module InstructionId = Oxcaml_utils.Id_counter.Make ()
module BlockId = Oxcaml_utils.Id_counter.Make ()

type block_desc =
  | Merge
  | Branch_target
  | Function_start
  | Call_continuation
  | Trap_handler

type instruction =
  | Op of op_data
  | Block_param of
      { block : block;
        index : int;
        typ : Cmm.machtype_component
      }
  | Proj of
      { index : int;
        src : instruction
      }
  | Push_trap of { handler : block option }
      (** [handler = None] is resolved to a shared dummy "invalid handler" block
          at CFG conversion time (used for trap handlers that are statically
          known to never fire). *)
  | Pop_trap of { handler : block option }
  | Stack_check of { max_frame_size_bytes : int }
  | Name_for_debugger of
      { ident : Ident.t;
        provenance : Backend_var.Provenance.t option;
        which_parameter : int option;
        regs : instruction array
      }

and op_data =
  { id : InstructionId.t;
    op : Operation.t;
    typ : Cmm.machtype;
    args : instruction array;
    dbg : Debuginfo.t;
    mutable usage_count : int
  }

and terminator =
  | Pending_construction
  | Goto of
      { goto : block;
        args : instruction array
      }
  | Branch of
      { cond : instruction;
        ifso : block;
        ifnot : block
      }
  | Switch of block array * instruction array
  | Return of instruction array
  | Raise of
      Lambda.raise_kind * instruction array * block option (* exn handler *)
  | Tailcall_self of
      { destination : block;
        args : instruction array
      }
  | Tailcall_func of Cfg_intf.S.func_call_operation * instruction array
  | Call of
      { op : Cfg_intf.S.func_call_operation;
        args : instruction array;
        continuation : block;
        exn_continuation : block option
      }
  | Prim of
      { op : Cfg_intf.S.prim_call_operation;
        args : instruction array;
        continuation : block;
        exn_continuation : block option
      }
  | Invalid of
      { message : string;
        args : instruction array;
        continuation : block option
      }

and dominator_info =
  | Unreachable
  | Reachable of dominator_of_reachable

and dominator_of_reachable =
  { dominator : block;  (** Immediate dominator, or self for the entry. *)
    depth : int  (** Depth in the dominator tree, 0 for the entry. *)
  }

and block =
  { id : BlockId.t;
    desc : block_desc;
    params : Cmm.machtype;
    mutable predecessors : block list;
    mutable body : instruction array;
    mutable terminator : terminator;
    mutable terminator_dbg : Debuginfo.t;
    mutable dominator_info : dominator_info;
    mutable label_hint : Label.t option
        (** Cached CFG label. Set by [cfg_of_ssa] during conversion so that a
            second conversion pass can reuse the same label. May be updated
            after a [cfg_compare] round to align with the old pipeline's labels.
        *)
  }

(* Block identity: based on the unique [id] field *)
let block_equal (a : block) (b : block) = BlockId.equal a.id b.id

let set_label_hint (b : block) (hint : Label.t option) = b.label_hint <- hint

(* Smart constructor for [Op] instructions. Allocates a fresh [InstructionId.t]
   and sets [usage_count] to 0. Does not emit into any builder: the caller (e.g.
   a reducer) controls emission. *)
let make_op ~op ~typ ~args ~dbg : instruction =
  Op { id = InstructionId.create (); op; typ; args; dbg; usage_count = 0 }

let create_block ~desc ~params =
  { id = BlockId.create ();
    desc;
    params;
    predecessors = [];
    body = [||];
    terminator = Pending_construction;
    terminator_dbg = Debuginfo.none;
    dominator_info = Unreachable;
    label_hint = None
  }

module Block = struct
  type t = block

  let equal = block_equal

  let compare (a : t) (b : t) = BlockId.compare a.id b.id

  let hash (b : t) = BlockId.hash b.id

  module Self = struct
    type nonrec t = t

    let equal = equal

    let compare = compare

    let hash = hash
  end

  module Set = Set.Make (Self)
  module Map = Map.Make (Self)
  module Tbl = Hashtbl.Make (Self)
end

type t =
  { blocks : block list;
    fun_name : string;
    fun_args : Cmm.machtype;
    fun_args_names : (Backend_var.With_provenance.t * Cmm.machtype) list;
    fun_codegen_options : Cmm.codegen_option list;
    fun_dbg : Debuginfo.t;
    entry : block;
    fun_poll : Lambda.poll_attribute;
    fun_ret_type : Cmm.machtype
  }

let predecessors (blk : block) : block list = blk.predecessors

let successors (blk : block) : block list =
  match blk.terminator with
  | Pending_construction | Return _ | Tailcall_func _ -> []
  | Raise (_, _, handler) -> ( match handler with Some h -> [h] | None -> [])
  | Goto { goto; _ } -> [goto]
  | Branch { ifso; ifnot; _ } -> [ifso; ifnot]
  | Switch (targets, _) -> Array.to_list targets
  | Tailcall_self { destination; _ } -> [destination]
  | Call { continuation; exn_continuation; _ }
  | Prim { continuation; exn_continuation; _ } -> (
    match exn_continuation with
    | Some l -> [continuation; l]
    | None -> [continuation])
  | Invalid { continuation; _ } -> (
    match continuation with Some l -> [l] | None -> [])

let reachable (dominator_info : dominator_info) =
  match dominator_info with Unreachable -> false | Reachable _ -> true

(* [Unreachable] is the neutral element of the dominator lattice meet: an
   unreachable block imposes no constraint on the common dominator. The result
   may itself be unreachable if both inputs are unreachable. *)
let rec common_dominator (a : dominator_info) (b : dominator_info) :
    dominator_info =
  match a, b with
  | Unreachable, other | other, Unreachable -> other
  | ( Reachable { depth = depth_a; dominator = dominator_a },
      Reachable { depth = depth_b; dominator = dominator_b } ) ->
    if depth_a > depth_b
    then common_dominator dominator_a.dominator_info b
    else if block_equal dominator_a dominator_b
    then (
      assert (depth_a = depth_b);
      Reachable { depth = depth_a; dominator = dominator_a })
    else (
      assert (depth_b > 0);
      common_dominator a dominator_b.dominator_info)

let rec dominates (a : block) (b : block) =
  match a.dominator_info, b.dominator_info with
  | _, Unreachable -> true
  | Unreachable, _ -> false
  | Reachable { depth = depth_a; _ }, Reachable { depth = depth_b; dominator }
    ->
    block_equal a b || (depth_b > depth_a && dominates a dominator)

let block_finished (b : block) =
  match[@warning "-fragile-match"] b.terminator with
  | Pending_construction -> false
  | _ -> true

let dominator_info_equal (a : dominator_info) (b : dominator_info) =
  match a, b with
  | Unreachable, Unreachable -> true
  | ( Reachable { dominator = d1; depth = dp1 },
      Reachable { dominator = d2; depth = dp2 } ) ->
    block_equal d1 d2 && dp1 = dp2
  | Unreachable, Reachable _ | Reachable _, Unreachable -> false

let rec update_dominator (blk : block) =
  if block_finished blk
  then (
    let old_info = blk.dominator_info in
    let new_info =
      match[@warning "-fragile-match"] blk.desc with
      | Function_start -> Reachable { dominator = blk; depth = 0 }
      | _ -> (
        let info blk =
          match blk.dominator_info with
          | Unreachable -> Unreachable
          | Reachable { depth; _ } ->
            Reachable { depth = depth + 1; dominator = blk }
        in
        match predecessors blk with
        | [] -> Unreachable
        | first :: rest ->
          List.fold_left
            (fun a b -> common_dominator a (info b))
            (info first) rest)
    in
    blk.dominator_info <- new_info;
    if not (dominator_info_equal new_info old_info)
    then
      if (not (reachable old_info)) && reachable new_info
      then
        (* Block became reachable; register it as a predecessor of its
           successors. *)
        List.iter (fun succ -> add_pred ~src:blk ~dst:succ) (successors blk)
      else List.iter update_dominator (successors blk))

and add_pred ~src ~(dst : block) =
  (* Skip unreachable predecessors: they will be added later if this block
     becomes reachable. *)
  if reachable src.dominator_info
  then (
    (match dst.desc with
    | Branch_target | Call_continuation -> (
      match dst.predecessors with
      | [] -> dst.predecessors <- [src]
      | [pred] -> assert (block_equal pred src)
      | _ -> assert false)
    | Function_start | Merge | Trap_handler ->
      dst.predecessors <- src :: dst.predecessors);
    update_dominator dst)

(* Use counts behave like reference counting: an Op with usage_count = 0 does
   NOT contribute to its args' use counts. Transitions (0 → 1 on increment, 1 →
   0 on decrement) propagate to args. *)
let rec increment_use (i : instruction) =
  match i with
  | Op r ->
    r.usage_count <- r.usage_count + 1;
    if r.usage_count = 1 then Array.iter increment_use r.args
  | Proj { src; _ } -> increment_use src
  | Block_param _ | Push_trap _ | Pop_trap _ | Stack_check _
  | Name_for_debugger _ ->
    ()

let rec decrement_use (i : instruction) =
  match i with
  | Op r ->
    assert (r.usage_count > 0);
    r.usage_count <- r.usage_count - 1;
    if r.usage_count = 0 then Array.iter decrement_use r.args
  | Proj { src; _ } -> decrement_use src
  | Block_param _ | Push_trap _ | Pop_trap _ | Stack_check _
  | Name_for_debugger _ ->
    ()

let increment_uses_in_terminator (term : terminator) =
  let incr_all = Array.iter increment_use in
  match term with
  | Pending_construction -> ()
  | Goto { args; _ } -> incr_all args
  | Branch { cond; _ } -> increment_use cond
  | Switch (_, args) -> incr_all args
  | Return args -> incr_all args
  | Raise (_, args, _) -> incr_all args
  | Tailcall_self { args; _ } -> incr_all args
  | Tailcall_func (_, args) -> incr_all args
  | Call { args; _ } -> incr_all args
  | Prim { args; _ } -> incr_all args
  | Invalid { args; _ } -> incr_all args

(* === Builder: assembler-like interface for constructing SSA graphs ===
   Maintains a current open block. [finish_block] seals the current block with a
   terminator. *)

module Builder : sig
  type t

  (** Create a builder that binds the start block. *)
  val make : Cmm.machtype -> t

  (** Add an instruction to the given incomplete block. *)
  val emit_instruction : t -> instruction -> unit

  (** Create an Op instruction and add it to the current block. *)
  val emit_op :
    t ->
    op:Operation.t ->
    dbg:Debuginfo.t ->
    typ:Cmm.machtype ->
    args:instruction array ->
    instruction

  (** Seal the current block with a terminator. *)
  val finish_block : t -> dbg:Debuginfo.t -> terminator -> unit

  val current_block : t -> block

  (** Create a new builder that binds a fresh block with the given [desc] and
      [params] (defaulting to no parameters). Predecessor wiring happens when
      the current block's terminator references the new block (via
      [finish_block]). *)
  val new_block : t -> ?params:Cmm.machtype -> block_desc -> t

  (** Create block_param instructions for a given block. *)
  val block_params : t -> instruction array

  (** Finalize: collect all created blocks. Returns blocks in emission order. *)
  val finish : t -> block list
end = struct
  (* Each [t] represents an incomplete block being built. All [t] values created
     from the same [make] call share the same [blocks] ref, which accumulatesf
     all created blocks in reverse order. *)
  type t =
    { blocks : block list ref;
      current_block : block;
      mutable body : instruction list
    }

  let new_block t ?(params = [||]) desc =
    let blk = create_block ~desc ~params in
    { blocks = t.blocks; current_block = blk; body = [] }

  let make params =
    let start_block = create_block ~desc:Function_start ~params in
    { blocks = ref []; current_block = start_block; body = [] }

  let current_block t = t.current_block

  let emit_instruction t (i : instruction) =
    assert (not (block_finished t.current_block));
    (match i with
    | Name_for_debugger { regs; _ } -> Array.iter increment_use regs
    | Op _ | Block_param _ | Proj _ | Push_trap _ | Pop_trap _ | Stack_check _
      ->
      ());
    t.body <- i :: t.body

  let emit_op t ~op ~dbg ~typ ~args =
    let i : instruction =
      Op { id = InstructionId.create (); op; typ; args; dbg; usage_count = 0 }
    in
    (* Impure ops are "pre-incremented" because their side effect counts as a
       use. [increment_use] handles the recursive propagation to args. *)
    if not (Operation.is_pure op) then increment_use i;
    emit_instruction t i;
    i

  let finish_block t ~dbg term =
    assert (not (block_finished t.current_block));
    (match[@warning "-fragile-match"] term with
    | Pending_construction ->
      Misc.fatal_error
        "Ssa.Builder.finish_block: cannot finish with Pending_construction"
    | _ -> ());
    t.current_block.body <- Array.of_list (List.rev t.body);
    t.current_block.terminator <- term;
    t.current_block.terminator_dbg <- dbg;
    increment_uses_in_terminator term;
    t.blocks := t.current_block :: !(t.blocks);
    update_dominator t.current_block

  let block_params t =
    Array.mapi
      (fun i typ ->
        (Block_param { block = t.current_block; index = i; typ } : instruction))
      t.current_block.params

  let finish t =
    List.filter
      (fun (b : block) -> reachable b.dominator_info)
      (List.rev !(t.blocks))
end
