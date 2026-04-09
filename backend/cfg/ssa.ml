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

type instruction =
  | Op of
      { id : InstructionId.t;
        op : Operation.t;
        typ : Cmm.machtype;
        args : instruction array;
        dbg : Debuginfo.t
      }
  | Block_param of
      { block : Label.t;
        index : int;
        typ : Cmm.machtype_component
      }
  | Proj of
      { index : int;
        src : instruction
      }
  | Pushtrap of { lbl_handler : Label.t }
  | Poptrap of { lbl_handler : Label.t }
  | Stack_check of { max_frame_size_bytes : int }
  | Name_for_debugger of
      { ident : Ident.t;
        provenance : Backend_var.Provenance.t option;
        which_parameter : int option;
        regs : instruction array
      }

type terminator =
  | Never
  | Goto of
      { goto : Label.t;
        args : instruction array
      }
  | Branch of
      { conditions : (instruction * Label.t) array;
        else_goto : Label.t
      }
  | Switch of Label.t array * instruction array
  | Return of instruction array
  | Raise of
      Lambda.raise_kind
      * instruction array
      * Label.t option (* exn handler label *)
  | Tailcall_self of
      { destination : Label.t;
        args : instruction array
      }
  | Tailcall_func of Cfg_intf.S.func_call_operation * instruction array
  | Call of
      { op : Cfg_intf.S.func_call_operation;
        args : instruction array;
        continuation : Label.t;
        exn_continuation : Label.t option
      }
  | Prim of
      { op : Cfg_intf.S.prim_call_operation;
        args : instruction array;
        continuation : Label.t;
        exn_continuation : Label.t option
      }

type block_desc =
  | Merge of { mutable predecessors : Label.t list }
  | BranchTarget of { predecessor : Label.t }
  | FunctionStart
  | CallContinuation of { predecessor : Label.t }
  | TrapHandler of { mutable predecessors : Label.t list }

type basic_block =
  { label : Label.t;
    desc : block_desc;
    params : Cmm.machtype;
    body : instruction array;
    terminator : terminator;
    terminator_dbg : Debuginfo.t
  }

type t =
  { blocks : basic_block Label.Tbl.t;
    block_order : Label.t list;
    fun_name : string;
    fun_args : Cmm.machtype;
    fun_args_names : (Backend_var.With_provenance.t * Cmm.machtype) list;
    fun_codegen_options : Cmm.codegen_option list;
    fun_dbg : Debuginfo.t;
    entry_label : Label.t;
    fun_contains_calls : bool;
    fun_poll : Lambda.poll_attribute;
    fun_ret_type : Cmm.machtype
  }

(* Printing *)

let rec print_instruction ppf (i : instruction) =
  match i with
  | Op { id; op; args; _ } ->
    Format.fprintf ppf "v%d = %a(%a)" (InstructionId.hash id) Operation.dump op
      print_args args
  | Block_param { block; index; _ } ->
    Format.fprintf ppf "%a.%d" Label.format block index
  | Proj { index; src } ->
    Format.fprintf ppf "proj(%d, %a)" index print_instr_ref src
  | Pushtrap { lbl_handler } ->
    Format.fprintf ppf "pushtrap %a" Label.format lbl_handler
  | Poptrap { lbl_handler } ->
    Format.fprintf ppf "poptrap %a" Label.format lbl_handler
  | Stack_check { max_frame_size_bytes } ->
    Format.fprintf ppf "stack_check %d" max_frame_size_bytes
  | Name_for_debugger { ident; _ } ->
    Format.fprintf ppf "name_for_debugger %a" Ident.print ident

and print_instr_ref ppf (i : instruction) =
  match i with
  | Op { id; _ } -> Format.fprintf ppf "v%d" (InstructionId.hash id)
  | Block_param { block; index; _ } ->
    Format.fprintf ppf "%a.%d" Label.format block index
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
    Format.fprintf ppf "goto %a(%a)" Label.format goto print_instr_array args
  | Branch { conditions; else_goto } ->
    Array.iter
      (fun (cond, label) ->
        Format.fprintf ppf "if %a then goto %a; " print_instr_ref cond
          Label.format label)
      conditions;
    Format.fprintf ppf "else goto %a" Label.format else_goto
  | Switch (labels, arg) ->
    Format.fprintf ppf "switch(%a) [%a]" print_instr_array arg
      (Format.pp_print_array
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
         Label.format)
      labels
  | Return args -> Format.fprintf ppf "return(%a)" print_instr_array args
  | Raise (_, args, _) -> Format.fprintf ppf "raise(%a)" print_instr_array args
  | Tailcall_self { destination; args } ->
    Format.fprintf ppf "tailcall_self %a(%a)" Label.format destination
      print_instr_array args
  | Tailcall_func (_, args) ->
    Format.fprintf ppf "tailcall_func(%a)" print_instr_array args
  | Call { op = Direct sym; args; continuation; exn_continuation } -> (
    Format.fprintf ppf "call %s(%a) -> %a" sym.sym_name print_instr_array args
      Label.format continuation;
    match exn_continuation with
    | Some l -> Format.fprintf ppf " exn %a" Label.format l
    | None -> ())
  | Call { op = Indirect _; args; continuation; exn_continuation } -> (
    Format.fprintf ppf "call_indirect(%a) -> %a" print_instr_array args
      Label.format continuation;
    match exn_continuation with
    | Some l -> Format.fprintf ppf " exn %a" Label.format l
    | None -> ())
  | Prim
      { op = External { func_symbol; _ }; args; continuation; exn_continuation }
    -> (
    Format.fprintf ppf "prim %s(%a) -> %a" func_symbol print_instr_array args
      Label.format continuation;
    match exn_continuation with
    | Some l -> Format.fprintf ppf " exn %a" Label.format l
    | None -> ())
  | Prim { op = Probe { name; _ }; args; continuation; _ } ->
    Format.fprintf ppf "probe %s(%a) -> %a" name print_instr_array args
      Label.format continuation

let print_preds ppf predecessors =
  Format.fprintf ppf "preds=[%a]"
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
       Label.format)
    predecessors

let print_block_desc ppf (desc : block_desc) =
  match desc with
  | Merge { predecessors } ->
    Format.fprintf ppf "merge %a" print_preds predecessors
  | BranchTarget { predecessor } ->
    Format.fprintf ppf "branch_target pred=%a" Label.format predecessor
  | FunctionStart -> Format.fprintf ppf "function_start"
  | CallContinuation { predecessor } ->
    Format.fprintf ppf "call_cont pred=%a" Label.format predecessor
  | TrapHandler { predecessors } ->
    Format.fprintf ppf "trap_handler %a" print_preds predecessors

let print_block ppf (block : basic_block) =
  Format.fprintf ppf "%a: %a(%a)@." Label.format block.label print_block_desc
    block.desc Printcmm.machtype block.params;
  Array.iter
    (fun bi -> Format.fprintf ppf "  %a@." print_instruction bi)
    block.body;
  Format.fprintf ppf "  %a@." print_terminator block.terminator

let print ppf (t : t) =
  Format.fprintf ppf "ssa %s(%a)@." t.fun_name Printcmm.machtype t.fun_args;
  Format.fprintf ppf "  entry = %a@.@." Label.format t.entry_label;
  Label.Tbl.iter (fun _label block -> print_block ppf block) t.blocks;
  Format.fprintf ppf "@."

(* === SSA invariant validation === *)

let validate (t : t) =
  let error fmt =
    Format.kasprintf
      (fun s -> Misc.fatal_errorf "SSA validation (%s): %s" t.fun_name s)
      fmt
  in
  (* Check entry block exists *)
  if not (Label.Tbl.mem t.blocks t.entry_label)
  then error "entry block %a not found" Label.format t.entry_label;
  (* Compute actual predecessors from terminators *)
  let actual_preds = Label.Tbl.create 16 in
  Label.Tbl.iter
    (fun _ bl -> Label.Tbl.replace actual_preds bl.label [])
    t.blocks;
  let add_pred ~src ~dst =
    match Label.Tbl.find_opt actual_preds dst with
    | Some ps -> Label.Tbl.replace actual_preds dst (src :: ps)
    | None ->
      error "block %a references non-existent successor %a" Label.format src
        Label.format dst
  in
  let successor_labels label (term : terminator) =
    match term with
    | Never -> ()
    | Goto { goto; _ } -> add_pred ~src:label ~dst:goto
    | Branch { conditions; else_goto } ->
      Array.iter (fun (_, lbl) -> add_pred ~src:label ~dst:lbl) conditions;
      add_pred ~src:label ~dst:else_goto
    | Switch (labels, _) ->
      Array.iter (fun lbl -> add_pred ~src:label ~dst:lbl) labels
    | Return _ | Raise _ | Tailcall_func _ -> ()
    | Tailcall_self { destination; _ } -> add_pred ~src:label ~dst:destination
    | Call { continuation; exn_continuation; _ }
    | Prim { continuation; exn_continuation; _ } -> (
      add_pred ~src:label ~dst:continuation;
      match exn_continuation with
      | Some l -> add_pred ~src:label ~dst:l
      | None -> ())
  in
  Label.Tbl.iter
    (fun label (bl : basic_block) -> successor_labels label bl.terminator)
    t.blocks;
  (* Compute successor labels for RPO/dominator computation *)
  let get_successors (term : terminator) =
    match term with
    | Never -> []
    | Goto { goto; _ } -> [goto]
    | Branch { conditions; else_goto } ->
      let succs = ref [else_goto] in
      Array.iter (fun (_, lbl) -> succs := lbl :: !succs) conditions;
      !succs
    | Switch (labels, _) -> Array.to_list labels
    | Return _ | Raise _ | Tailcall_func _ -> []
    | Tailcall_self { destination; _ } -> [destination]
    | Call { continuation; exn_continuation; _ }
    | Prim { continuation; exn_continuation; _ } ->
      continuation :: (match exn_continuation with Some l -> [l] | None -> [])
  in
  (* Compute reverse postorder via DFS *)
  let rpo =
    let visited = Label.Tbl.create 16 in
    let order = ref [] in
    let rec dfs label =
      if not (Label.Tbl.mem visited label)
      then (
        Label.Tbl.replace visited label ();
        let bl = Label.Tbl.find t.blocks label in
        List.iter dfs (get_successors bl.terminator);
        order := label :: !order)
    in
    dfs t.entry_label;
    !order
  in
  (* Compute immediate dominators (Cooper-Harvey-Kennedy algorithm) *)
  let rpo_index = Label.Tbl.create 16 in
  List.iteri (fun i lbl -> Label.Tbl.replace rpo_index lbl i) rpo;
  let idom = Label.Tbl.create 16 in
  Label.Tbl.replace idom t.entry_label t.entry_label;
  let intersect b1 b2 =
    let b1 = ref b1 and b2 = ref b2 in
    while not (Label.equal !b1 !b2) do
      while Label.Tbl.find rpo_index !b1 > Label.Tbl.find rpo_index !b2 do
        b1 := Label.Tbl.find idom !b1
      done;
      while Label.Tbl.find rpo_index !b2 > Label.Tbl.find rpo_index !b1 do
        b2 := Label.Tbl.find idom !b2
      done
    done;
    !b1
  in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun label ->
        if not (Label.equal label t.entry_label)
        then
          let preds = Label.Tbl.find actual_preds label in
          let processed = List.filter (fun p -> Label.Tbl.mem idom p) preds in
          match processed with
          | [] -> ()
          | first :: rest ->
            let new_idom = List.fold_left intersect first rest in
            if
              not
                (match Label.Tbl.find_opt idom label with
                | Some d -> Label.equal d new_idom
                | None -> false)
            then (
              Label.Tbl.replace idom label new_idom;
              changed := true))
      rpo
  done;
  (* Build dominator tree and compute DFS in/out times for O(1) dominance
     queries *)
  let dom_children = Label.Tbl.create 16 in
  Label.Tbl.iter
    (fun label parent ->
      if not (Label.equal label parent)
      then
        let kids =
          match Label.Tbl.find_opt dom_children parent with
          | Some l -> l
          | None -> []
        in
        Label.Tbl.replace dom_children parent (label :: kids))
    idom;
  let dom_in = Label.Tbl.create 16 in
  let dom_out = Label.Tbl.create 16 in
  let time = ref 0 in
  let rec compute_dom_times label =
    Label.Tbl.replace dom_in label !time;
    incr time;
    (match Label.Tbl.find_opt dom_children label with
    | Some kids -> List.iter compute_dom_times kids
    | None -> ());
    Label.Tbl.replace dom_out label !time;
    incr time
  in
  compute_dom_times t.entry_label;
  let dominates a b =
    Label.Tbl.find dom_in a <= Label.Tbl.find dom_in b
    && Label.Tbl.find dom_out a >= Label.Tbl.find dom_out b
  in
  (* Validate blocks in dominator tree order, building Op definition map as we
     go *)
  let defined_ops = InstructionId.Tbl.create 64 in
  let rec check_arg label (i : instruction) =
    match i with
    | Op { id; _ } -> (
      match InstructionId.Tbl.find_opt defined_ops id with
      | None ->
        error "block %a: Op v%d used but not defined" Label.format label
          (InstructionId.hash id)
      | Some def_label ->
        if not (dominates def_label label)
        then
          error "block %a: Op v%d defined in non-dominating block %a"
            Label.format label (InstructionId.hash id) Label.format def_label)
    | Block_param { block; index; typ } -> (
      match Label.Tbl.find_opt t.blocks block with
      | None ->
        error "block %a: BlockParam references non-existent block %a"
          Label.format label Label.format block
      | Some target ->
        if index < 0 || index >= Array.length target.params
        then
          error
            "block %a: BlockParam index %d out of range for block %a (params \
             length %d)"
            Label.format label index Label.format block
            (Array.length target.params);
        let expected = target.params.(index) in
        if not (Cmm.equal_machtype_component expected typ)
        then
          error "block %a: BlockParam %a.%d has type %a but block params say %a"
            Label.format label Label.format block index
            Printcmm.machtype_component typ Printcmm.machtype_component expected;
        if not (dominates block label)
        then
          error "block %a: BlockParam of non-dominating block %a" Label.format
            label Label.format block)
    | Proj { src; _ } -> (
      match src with
      | Op _ -> check_arg label src
      | Block_param _ | Proj _ | Pushtrap _ | Poptrap _ | Stack_check _
      | Name_for_debugger _ ->
        error "block %a: Proj source must be an Op" Label.format label)
    | Pushtrap _ | Poptrap _ | Stack_check _ | Name_for_debugger _ ->
      error "block %a: non-value instruction used as argument" Label.format
        label
  and check_args label args = Array.iter (check_arg label) args
  and visit_block label =
    let bl = Label.Tbl.find t.blocks label in
    (* Check entry block is FunctionStart *)
    (if Label.equal label t.entry_label
     then
       match bl.desc with
       | FunctionStart -> ()
       | Merge _ | BranchTarget _ | CallContinuation _ | TrapHandler _ ->
         error "entry block %a is not FunctionStart" Label.format label);
    (* Check BranchTarget has exactly one predecessor and it matches the
       declared one *)
    (match bl.desc with
    | BranchTarget { predecessor } ->
      if Array.length bl.params > 0
      then
        error "block %a: BranchTarget must not have parameters" Label.format
          label;
      let preds = Label.Tbl.find actual_preds label in
      if not (List.exists (Label.equal predecessor) preds)
      then
        error
          "block %a: BranchTarget declares predecessor %a but it is not an \
           actual predecessor"
          Label.format label Label.format predecessor
    | Merge _ | FunctionStart | CallContinuation _ | TrapHandler _ -> ());
    (* Check body: validate args then register Op *)
    Array.iter
      (fun (i : instruction) ->
        match i with
        | Op { id; args; _ } ->
          check_args label args;
          if InstructionId.Tbl.mem defined_ops id
          then
            error "block %a: duplicate Op id v%d" Label.format label
              (InstructionId.hash id);
          InstructionId.Tbl.replace defined_ops id label
        | Pushtrap _ | Poptrap _ | Stack_check _ | Name_for_debugger _ -> ()
        | Block_param _ | Proj _ -> ())
      bl.body;
    (* Check terminator *)
    (match bl.terminator with
    | Never -> ()
    | Goto { goto; args } ->
      check_args label args;
      let target = Label.Tbl.find t.blocks goto in
      if Array.length args <> Array.length target.params
      then
        error "block %a: goto %a has %d args but target has %d params"
          Label.format label Label.format goto (Array.length args)
          (Array.length target.params)
    | Branch { conditions; else_goto } ->
      let check_branch_target lbl =
        let target = Label.Tbl.find t.blocks lbl in
        match target.desc with
        | BranchTarget _ -> ()
        | Merge _ | FunctionStart | CallContinuation _ | TrapHandler _ ->
          error "block %a: Branch target %a is not a BranchTarget block"
            Label.format label Label.format lbl
      in
      Array.iter
        (fun (cond, lbl) ->
          check_arg label cond;
          check_branch_target lbl)
        conditions;
      check_branch_target else_goto
    | Switch (labels, args) ->
      check_args label args;
      Array.iter
        (fun lbl ->
          let target = Label.Tbl.find t.blocks lbl in
          match target.desc with
          | BranchTarget _ -> ()
          | Merge _ | FunctionStart | CallContinuation _ | TrapHandler _ ->
            error "block %a: Switch target %a is not a BranchTarget block"
              Label.format label Label.format lbl)
        labels
    | Return args -> check_args label args
    | Raise (_, args, _) -> check_args label args
    | Tailcall_self { args; _ } -> check_args label args
    | Tailcall_func (_, args) -> check_args label args
    | Call { args; _ } -> check_args label args
    | Prim { args; _ } -> check_args label args);
    (* Visit dominator tree children *)
    match Label.Tbl.find_opt dom_children label with
    | Some kids -> List.iter visit_block kids
    | None -> ()
  in
  visit_block t.entry_label
