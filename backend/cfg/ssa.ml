[@@@ocaml.warning "+a-40-41-42"]

module InstructionId : sig
  type t

  val create : unit -> t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val hash : t -> int

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  module Tbl : Hashtbl.S with type key = t
end = struct
  type t = int

  let next_id = ref 0

  let create () =
    let id = !next_id in
    incr next_id;
    id

  let equal = Int.equal

  let compare = Int.compare

  let hash = Fun.id

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

(* All types are mutually recursive because block contains terminator which
   references block. *)

type instruction =
  | Op of
      { id : InstructionId.t;
        op : Operation.t;
        typ : Cmm.machtype;
        args : instruction array;
        dbg : Debuginfo.t
      }
  | Block_param of
      { block : block;
        index : int;
        typ : Cmm.machtype_component
      }
  | Proj of
      { index : int;
        src : instruction
      }
  | Pushtrap of { handler : block }
  | Poptrap of { handler : block }
  | Stack_check of { max_frame_size_bytes : int }
  | Name_for_debugger of
      { ident : Ident.t;
        provenance : Backend_var.Provenance.t option;
        which_parameter : int option;
        regs : instruction array
      }

and terminator =
  | Never
  | Goto of
      { goto : block;
        args : instruction array
      }
  | Branch of
      { conditions : (instruction * block) array;
        else_goto : block
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

and block_desc =
  | Merge of { mutable predecessors : block list }
  | BranchTarget of { predecessor : block }
  | FunctionStart
  | CallContinuation of { predecessor : block }
  | TrapHandler of { mutable predecessors : block list }

and block =
  { id : int;
    desc : block_desc;
    params : Cmm.machtype;
    mutable body : instruction array;
    mutable terminator : terminator;
    mutable terminator_dbg : Debuginfo.t
  }

(* Block identity: based on the unique [id] field *)
let block_equal a b = Int.equal a.id b.id

let block_id b = b.id

let next_block_id = ref 0

let create_block ~desc ~params =
  let id = !next_block_id in
  incr next_block_id;
  { id;
    desc;
    params;
    body = [||];
    terminator = Never;
    terminator_dbg = Debuginfo.none
  }

module Block = struct
  type t = block

  let equal = block_equal

  let compare a b = Int.compare a.id b.id

  let hash b = b.id

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

let add_pred ~src ~(dst : block) =
  match dst.desc with
  | Merge r -> r.predecessors <- src :: r.predecessors
  | TrapHandler r -> r.predecessors <- src :: r.predecessors
  | BranchTarget _ | FunctionStart | CallContinuation _ -> ()

let add_terminator_preds src (term : terminator) =
  let add dst = add_pred ~src ~dst in
  match term with
  | Never | Return _ | Raise _ | Tailcall_func _ -> ()
  | Goto { goto; _ } -> add goto
  | Branch { conditions; else_goto } ->
    Array.iter (fun (_, dst) -> add dst) conditions;
    add else_goto
  | Switch (targets, _) -> Array.iter add targets
  | Tailcall_self { destination; _ } -> add destination
  | Call { continuation; exn_continuation; _ }
  | Prim { continuation; exn_continuation; _ } ->
    add continuation;
    Option.iter add exn_continuation
  | Invalid { continuation; _ } -> Option.iter add continuation

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

  (** Creates a new builder that binds a new merge block. *)
  val create_merge : t -> Cmm.machtype -> t

  val create_trap_handler : t -> Cmm.machtype -> t

  (** Creates a new builder for a branch target, implicitly sets the current
      block as predecessor. *)
  val create_branch_target : t -> t

  val create_call_continuation : t -> Cmm.machtype -> t

  (** Create block_param instructions for a given block. *)
  val block_params : t -> instruction array

  (** Finalize: collect all created blocks. Returns blocks in emission order. *)
  val finish : t -> block list
end = struct
  (* Each [t] represents an incomplete block being built. All [t] values created
     from the same [make] call share the same [blocks] ref, which accumulates
     all created blocks in reverse order. *)
  type t =
    { blocks : block list ref;
      current_block : block;
      mutable body : instruction list;
      mutable block_finished : bool
    }

  let create_block_from t desc params =
    let blk = create_block ~desc ~params in
    let new_t =
      { blocks = t.blocks;
        current_block = blk;
        body = [];
        block_finished = false
      }
    in
    t.blocks := blk :: !(t.blocks);
    new_t

  let make params =
    let blocks = ref [] in
    let blk = create_block ~desc:FunctionStart ~params in
    blocks := [blk];
    { blocks; current_block = blk; body = []; block_finished = false }

  let current_block t = t.current_block

  let emit_instruction t (i : instruction) =
    assert (not t.block_finished);
    t.body <- i :: t.body

  let emit_op t ~op ~dbg ~typ ~args =
    let i : instruction =
      Op { id = InstructionId.create (); op; typ; args; dbg }
    in
    emit_instruction t i;
    i

  let finish_block t ~dbg term =
    assert (not t.block_finished);
    t.current_block.body <- Array.of_list (List.rev t.body);
    t.current_block.terminator <- term;
    t.current_block.terminator_dbg <- dbg;
    add_terminator_preds t.current_block term;
    t.block_finished <- true

  let create_merge t params =
    create_block_from t (Merge { predecessors = [] }) params

  let create_trap_handler t params =
    create_block_from t (TrapHandler { predecessors = [] }) params

  let create_branch_target t =
    create_block_from t (BranchTarget { predecessor = t.current_block }) [||]

  let create_call_continuation t params =
    create_block_from t
      (CallContinuation { predecessor = t.current_block })
      params

  let block_params t =
    Array.mapi
      (fun i typ ->
        (Block_param { block = t.current_block; index = i; typ } : instruction))
      t.current_block.params

  let finish t = List.rev !(t.blocks)
end

(* Printing *)

let print_block_id ppf (b : block) = Format.fprintf ppf "%d" b.id

let rec print_instruction ppf (i : instruction) =
  match i with
  | Op { id; op; args; _ } ->
    Format.fprintf ppf "v%d = %a(%a)" (InstructionId.hash id) Operation.dump op
      print_args args
  | Block_param { block; index; _ } ->
    Format.fprintf ppf "%a.%d" print_block_id block index
  | Proj { index; src } ->
    Format.fprintf ppf "proj(%d, %a)" index print_instr_ref src
  | Pushtrap { handler } ->
    Format.fprintf ppf "pushtrap %a" print_block_id handler
  | Poptrap { handler } ->
    Format.fprintf ppf "poptrap %a" print_block_id handler
  | Stack_check { max_frame_size_bytes } ->
    Format.fprintf ppf "stack_check %d" max_frame_size_bytes
  | Name_for_debugger { ident; _ } ->
    Format.fprintf ppf "name_for_debugger %a" Ident.print ident

and print_instr_ref ppf (i : instruction) =
  match i with
  | Op { id; _ } -> Format.fprintf ppf "v%d" (InstructionId.hash id)
  | Block_param { block; index; _ } ->
    Format.fprintf ppf "%a.%d" print_block_id block index
  | Proj { index; src } ->
    Format.fprintf ppf "proj(%d, %a)" index print_instr_ref src
  | Pushtrap _ | Poptrap _ | Stack_check _ | Name_for_debugger _ ->
    print_instruction ppf i

and print_args ppf args =
  Array.iteri
    (fun i arg ->
      if i > 0 then Format.fprintf ppf ", ";
      print_instr_ref ppf arg)
    args

let print_instr_array ppf arr =
  Array.iteri
    (fun i arg ->
      if i > 0 then Format.fprintf ppf ", ";
      print_instr_ref ppf arg)
    arr

let print_terminator ppf (t : terminator) =
  match t with
  | Never -> Format.fprintf ppf "never"
  | Goto { goto; args } ->
    Format.fprintf ppf "goto %a(%a)" print_block_id goto print_instr_array args
  | Branch { conditions; else_goto } ->
    Array.iter
      (fun (cond, target) ->
        Format.fprintf ppf "if %a then goto %a; " print_instr_ref cond
          print_block_id target)
      conditions;
    Format.fprintf ppf "else goto %a" print_block_id else_goto
  | Switch (targets, arg) ->
    Format.fprintf ppf "switch(%a) [%a]" print_instr_array arg
      (Format.pp_print_array
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
         print_block_id)
      targets
  | Return args -> Format.fprintf ppf "return(%a)" print_instr_array args
  | Raise (_, args, _) -> Format.fprintf ppf "raise(%a)" print_instr_array args
  | Tailcall_self { destination; args } ->
    Format.fprintf ppf "tailcall_self %a(%a)" print_block_id destination
      print_instr_array args
  | Tailcall_func (_, args) ->
    Format.fprintf ppf "tailcall_func(%a)" print_instr_array args
  | Call { op = Direct sym; args; continuation; exn_continuation } -> (
    Format.fprintf ppf "call %s(%a) -> %a" sym.sym_name print_instr_array args
      print_block_id continuation;
    match exn_continuation with
    | Some l -> Format.fprintf ppf " exn %a" print_block_id l
    | None -> ())
  | Call { op = Indirect _; args; continuation; exn_continuation } -> (
    Format.fprintf ppf "call_indirect(%a) -> %a" print_instr_array args
      print_block_id continuation;
    match exn_continuation with
    | Some l -> Format.fprintf ppf " exn %a" print_block_id l
    | None -> ())
  | Prim
      { op = External { func_symbol; _ }; args; continuation; exn_continuation }
    -> (
    Format.fprintf ppf "prim %s(%a) -> %a" func_symbol print_instr_array args
      print_block_id continuation;
    match exn_continuation with
    | Some l -> Format.fprintf ppf " exn %a" print_block_id l
    | None -> ())
  | Prim { op = Probe { name; _ }; args; continuation; _ } ->
    Format.fprintf ppf "probe %s(%a) -> %a" name print_instr_array args
      print_block_id continuation
  | Invalid { message = _; args; continuation } -> (
    Format.fprintf ppf "invalid(%a)" print_instr_array args;
    match continuation with
    | Some l -> Format.fprintf ppf " -> %a" print_block_id l
    | None -> ())

let print_preds ppf predecessors =
  Format.fprintf ppf "preds=[%a]"
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
       print_block_id)
    predecessors

let print_block_desc ppf (desc : block_desc) =
  match desc with
  | Merge { predecessors } ->
    Format.fprintf ppf "merge %a" print_preds predecessors
  | BranchTarget { predecessor } ->
    Format.fprintf ppf "branch_target pred=%a" print_block_id predecessor
  | FunctionStart -> Format.fprintf ppf "function_start"
  | CallContinuation { predecessor } ->
    Format.fprintf ppf "call_cont pred=%a" print_block_id predecessor
  | TrapHandler { predecessors } ->
    Format.fprintf ppf "trap_handler %a" print_preds predecessors

let print_block ppf (blk : block) =
  Format.fprintf ppf "%a: %a(%a)@." print_block_id blk print_block_desc blk.desc
    Printcmm.machtype blk.params;
  Array.iter
    (fun bi -> Format.fprintf ppf "  %a@." print_instruction bi)
    blk.body;
  Format.fprintf ppf "  %a@." print_terminator blk.terminator

let print ppf (t : t) =
  Format.fprintf ppf "ssa %s(%a)@." t.fun_name Printcmm.machtype t.fun_args;
  Format.fprintf ppf "  entry = %a@.@." print_block_id t.entry;
  List.iter (print_block ppf) t.blocks;
  Format.fprintf ppf "@."

(* === SSA invariant validation === *)

let validate (t : t) =
  let error fmt =
    Format.kasprintf
      (fun s -> Misc.fatal_errorf "SSA validation (%s): %s" t.fun_name s)
      fmt
  in
  let pb = print_block_id in
  (* Build block set for membership checking *)
  let block_set = Block.Tbl.create 16 in
  List.iter (fun bl -> Block.Tbl.replace block_set bl ()) t.blocks;
  let block_exists b = Block.Tbl.mem block_set b in
  (* Check entry block exists *)
  if not (block_exists t.entry) then error "entry block %a not found" pb t.entry;
  (* Compute actual predecessors from terminators *)
  let actual_preds = Block.Tbl.create 16 in
  List.iter (fun bl -> Block.Tbl.replace actual_preds bl []) t.blocks;
  let add_pred ~src ~dst =
    match Block.Tbl.find_opt actual_preds dst with
    | Some ps -> Block.Tbl.replace actual_preds dst (src :: ps)
    | None ->
      error "block %a references non-existent successor %a" pb src pb dst
  in
  let successor_blocks (bl : block) =
    match bl.terminator with
    | Never -> ()
    | Goto { goto; _ } -> add_pred ~src:bl ~dst:goto
    | Branch { conditions; else_goto } ->
      Array.iter (fun (_, dst) -> add_pred ~src:bl ~dst) conditions;
      add_pred ~src:bl ~dst:else_goto
    | Switch (targets, _) ->
      Array.iter (fun dst -> add_pred ~src:bl ~dst) targets
    | Return _ | Raise _ | Tailcall_func _ -> ()
    | Tailcall_self { destination; _ } -> add_pred ~src:bl ~dst:destination
    | Call { continuation; exn_continuation; _ }
    | Prim { continuation; exn_continuation; _ } -> (
      add_pred ~src:bl ~dst:continuation;
      match exn_continuation with
      | Some l -> add_pred ~src:bl ~dst:l
      | None -> ())
    | Invalid { continuation; _ } ->
      Option.iter (fun l -> add_pred ~src:bl ~dst:l) continuation
  in
  List.iter successor_blocks t.blocks;
  (* Compute successor blocks for RPO/dominator computation *)
  let get_successors (bl : block) =
    match bl.terminator with
    | Never -> []
    | Goto { goto; _ } -> [goto]
    | Branch { conditions; else_goto } ->
      let succs = ref [else_goto] in
      Array.iter (fun (_, dst) -> succs := dst :: !succs) conditions;
      !succs
    | Switch (targets, _) -> Array.to_list targets
    | Return _ | Raise _ | Tailcall_func _ -> []
    | Tailcall_self { destination; _ } -> [destination]
    | Call { continuation; exn_continuation; _ }
    | Prim { continuation; exn_continuation; _ } ->
      continuation :: (match exn_continuation with Some l -> [l] | None -> [])
    | Invalid { continuation; _ } -> (
      match continuation with Some l -> [l] | None -> [])
  in
  (* Compute reverse postorder via DFS *)
  let rpo =
    let visited = Block.Tbl.create 16 in
    let order = ref [] in
    let rec dfs bl =
      if not (Block.Tbl.mem visited bl)
      then (
        Block.Tbl.replace visited bl ();
        List.iter dfs (get_successors bl);
        order := bl :: !order)
    in
    dfs t.entry;
    !order
  in
  (* Compute immediate dominators (Cooper-Harvey-Kennedy algorithm) *)
  let rpo_index = Block.Tbl.create 16 in
  List.iteri (fun i bl -> Block.Tbl.replace rpo_index bl i) rpo;
  let idom = Block.Tbl.create 16 in
  Block.Tbl.replace idom t.entry t.entry;
  let intersect b1 b2 =
    let b1 = ref b1 and b2 = ref b2 in
    while not (block_equal !b1 !b2) do
      while Block.Tbl.find rpo_index !b1 > Block.Tbl.find rpo_index !b2 do
        b1 := Block.Tbl.find idom !b1
      done;
      while Block.Tbl.find rpo_index !b2 > Block.Tbl.find rpo_index !b1 do
        b2 := Block.Tbl.find idom !b2
      done
    done;
    !b1
  in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun bl ->
        if not (block_equal bl t.entry)
        then
          let preds = Block.Tbl.find actual_preds bl in
          let processed = List.filter (fun p -> Block.Tbl.mem idom p) preds in
          match processed with
          | [] -> ()
          | first :: rest ->
            let new_idom = List.fold_left intersect first rest in
            if
              not
                (match Block.Tbl.find_opt idom bl with
                | Some d -> block_equal d new_idom
                | None -> false)
            then (
              Block.Tbl.replace idom bl new_idom;
              changed := true))
      rpo
  done;
  (* Build dominator tree and compute DFS in/out times for O(1) dominance
     queries *)
  let dom_children = Block.Tbl.create 16 in
  Block.Tbl.iter
    (fun bl parent ->
      if not (block_equal bl parent)
      then
        let kids =
          match Block.Tbl.find_opt dom_children parent with
          | Some l -> l
          | None -> []
        in
        Block.Tbl.replace dom_children parent (bl :: kids))
    idom;
  let dom_in = Block.Tbl.create 16 in
  let dom_out = Block.Tbl.create 16 in
  let time = ref 0 in
  let rec compute_dom_times bl =
    Block.Tbl.replace dom_in bl !time;
    incr time;
    (match Block.Tbl.find_opt dom_children bl with
    | Some kids -> List.iter compute_dom_times kids
    | None -> ());
    Block.Tbl.replace dom_out bl !time;
    incr time
  in
  compute_dom_times t.entry;
  let dominates a b =
    Block.Tbl.find dom_in a <= Block.Tbl.find dom_in b
    && Block.Tbl.find dom_out a >= Block.Tbl.find dom_out b
  in
  (* Validate blocks in dominator tree order, building Op definition map as we
     go *)
  let defined_ops = InstructionId.Tbl.create 64 in
  let rec check_arg (bl : block) (i : instruction) =
    match i with
    | Op { id; _ } -> (
      match InstructionId.Tbl.find_opt defined_ops id with
      | None ->
        error "block %a: Op v%d used but not defined" pb bl
          (InstructionId.hash id)
      | Some def_block ->
        if not (dominates def_block bl)
        then
          error "block %a: Op v%d defined in non-dominating block %a" pb bl
            (InstructionId.hash id) pb def_block)
    | Block_param { block; index; typ } ->
      if not (block_exists block)
      then
        error "block %a: BlockParam references non-existent block %a" pb bl pb
          block;
      if index < 0 || index >= Array.length block.params
      then
        error
          "block %a: BlockParam index %d out of range for block %a (params \
           length %d)"
          pb bl index pb block
          (Array.length block.params);
      let expected = block.params.(index) in
      if not (Cmm.equal_machtype_component expected typ)
      then
        error "block %a: BlockParam %a.%d has type %a but block params say %a"
          pb bl pb block index Printcmm.machtype_component typ
          Printcmm.machtype_component expected;
      if not (dominates block bl)
      then
        error "block %a: BlockParam of non-dominating block %a" pb bl pb block
    | Proj { src; _ } -> (
      match src with
      | Op _ -> check_arg bl src
      | Block_param _ | Proj _ | Pushtrap _ | Poptrap _ | Stack_check _
      | Name_for_debugger _ ->
        error "block %a: Proj source must be an Op" pb bl)
    | Pushtrap _ | Poptrap _ | Stack_check _ | Name_for_debugger _ ->
      error "block %a: non-value instruction used as argument" pb bl
  and check_args bl args = Array.iter (check_arg bl) args
  and visit_block bl =
    (* Check entry block is FunctionStart *)
    (if block_equal bl t.entry
     then
       match bl.desc with
       | FunctionStart -> ()
       | Merge _ | BranchTarget _ | CallContinuation _ | TrapHandler _ ->
         error "entry block %a is not FunctionStart" pb bl);
    (* Check BranchTarget has exactly one predecessor and it matches the
       declared one *)
    (match bl.desc with
    | BranchTarget { predecessor } ->
      if Array.length bl.params > 0
      then error "block %a: BranchTarget must not have parameters" pb bl;
      let preds = Block.Tbl.find actual_preds bl in
      if not (List.exists (block_equal predecessor) preds)
      then
        error
          "block %a: BranchTarget declares predecessor %a but it is not an \
           actual predecessor"
          pb bl pb predecessor
    | Merge { predecessors } ->
      let preds = Block.Tbl.find actual_preds bl in
      let pred_set =
        List.fold_left (fun s b -> Block.Set.add b s) Block.Set.empty preds
      in
      let declared_set =
        List.fold_left
          (fun s b -> Block.Set.add b s)
          Block.Set.empty predecessors
      in
      if not (Block.Set.equal pred_set declared_set)
      then
        error
          "block %a: Merge declares predecessors {%a} but actual predecessors \
           are {%a}"
          pb bl
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
             pb)
          predecessors
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
             pb)
          preds
    | FunctionStart | CallContinuation _ | TrapHandler _ -> ());
    (* Check body: validate args then register Op *)
    Array.iter
      (fun (i : instruction) ->
        match i with
        | Op { id; args; _ } ->
          check_args bl args;
          if InstructionId.Tbl.mem defined_ops id
          then
            error "block %a: duplicate Op id v%d" pb bl (InstructionId.hash id);
          InstructionId.Tbl.replace defined_ops id bl
        | Pushtrap _ | Poptrap _ | Stack_check _ | Name_for_debugger _ -> ()
        | Block_param _ | Proj _ -> ())
      bl.body;
    (* Check terminator *)
    (match bl.terminator with
    | Never -> ()
    | Goto { goto; args } ->
      check_args bl args;
      if Array.length args <> Array.length goto.params
      then
        error "block %a: goto %a has %d args but target has %d params" pb bl pb
          goto (Array.length args) (Array.length goto.params)
    | Branch { conditions; else_goto } ->
      let check_branch_target target =
        match target.desc with
        | BranchTarget _ -> ()
        | Merge _ | FunctionStart | CallContinuation _ | TrapHandler _ ->
          error "block %a: Branch target %a is not a BranchTarget block" pb bl
            pb target
      in
      Array.iter
        (fun (cond, target) ->
          check_arg bl cond;
          check_branch_target target)
        conditions;
      check_branch_target else_goto
    | Switch (targets, args) ->
      check_args bl args;
      Array.iter
        (fun target ->
          match target.desc with
          | BranchTarget _ -> ()
          | Merge _ | FunctionStart | CallContinuation _ | TrapHandler _ ->
            error "block %a: Switch target %a is not a BranchTarget block" pb bl
              pb target)
        targets
    | Return args -> check_args bl args
    | Raise (_, args, _) -> check_args bl args
    | Tailcall_self { args; _ } -> check_args bl args
    | Tailcall_func (_, args) -> check_args bl args
    | Call { args; _ } -> check_args bl args
    | Prim { args; _ } -> check_args bl args
    | Invalid { args; _ } -> check_args bl args);
    (* Visit dominator tree children *)
    match Block.Tbl.find_opt dom_children bl with
    | Some kids -> List.iter visit_block kids
    | None -> ()
  in
  visit_block t.entry
