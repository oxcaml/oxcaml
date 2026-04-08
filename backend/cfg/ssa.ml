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
        args : instruction array
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

type body_instruction =
  | Instr of instruction
  | Pushtrap of { lbl_handler : Label.t }
  | Poptrap of { lbl_handler : Label.t }
  | Stack_check of { max_frame_size_bytes : int }
  | Debuginfo of Debuginfo.t

type terminator =
  | Never
  | Always of
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
  | Tailcall_func of
      Cfg_intf.S.func_call_operation * instruction array
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
  | FunctionStart
  | CallContinuation of { predecessor : Label.t }
  | TrapHandler of
      { mutable predecessors : Label.t list }

type basic_block =
  { label : Label.t;
    desc : block_desc;
    params : Cmm.machtype;
    body : body_instruction array;
    terminator : terminator
  }

type t =
  { blocks : basic_block Label.Tbl.t;
    fun_name : string;
    fun_args : Cmm.machtype;
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
    Format.fprintf ppf "v%d = %a(%a)"
      (InstructionId.hash id) Operation.dump op print_args
      args
  | Block_param { block; index; _ } ->
    Format.fprintf ppf "%a.%d" Label.format block index
  | Proj { index; src } ->
    Format.fprintf ppf "proj(%d, %a)" index print_instr_ref
      src

and print_instr_ref ppf (i : instruction) =
  match i with
  | Op { id; _ } ->
    Format.fprintf ppf "v%d" (InstructionId.hash id)
  | Block_param { block; index; _ } ->
    Format.fprintf ppf "%a.%d" Label.format block index
  | Proj { index; src } ->
    Format.fprintf ppf "proj(%d, %a)" index print_instr_ref
      src

and print_args ppf args =
  Array.iteri
    (fun i arg ->
      if i > 0 then Format.fprintf ppf ", ";
      print_instr_ref ppf arg)
    args

let print_body_instruction ppf (bi : body_instruction) =
  match bi with
  | Instr i -> print_instruction ppf i
  | Pushtrap { lbl_handler } ->
    Format.fprintf ppf "pushtrap %a" Label.format
      lbl_handler
  | Poptrap { lbl_handler } ->
    Format.fprintf ppf "poptrap %a" Label.format
      lbl_handler
  | Stack_check { max_frame_size_bytes } ->
    Format.fprintf ppf "stack_check %d"
      max_frame_size_bytes
  | Debuginfo dbg ->
    Format.fprintf ppf "debuginfo %a" Debuginfo.print_compact
      dbg

let print_instr_array ppf arr =
  Array.iteri
    (fun i arg ->
      if i > 0 then Format.fprintf ppf ", ";
      print_instr_ref ppf arg)
    arr

let print_terminator ppf (t : terminator) =
  match t with
  | Never -> Format.fprintf ppf "never"
  | Always { goto; args } ->
    Format.fprintf ppf "goto %a(%a)" Label.format goto
      print_instr_array args
  | Branch { conditions; else_goto } ->
    Array.iter
      (fun (cond, label) ->
        Format.fprintf ppf "if %a then goto %a; "
          print_instr_ref cond Label.format label)
      conditions;
    Format.fprintf ppf "else goto %a" Label.format
      else_goto
  | Switch (labels, arg) ->
    Format.fprintf ppf "switch(%a) [%a]"
      print_instr_array arg
      (Format.pp_print_array ~pp_sep:(fun ppf () ->
           Format.fprintf ppf ", ")
         Label.format)
      labels
  | Return args ->
    Format.fprintf ppf "return(%a)" print_instr_array args
  | Raise (_, args, _) ->
    Format.fprintf ppf "raise(%a)" print_instr_array args
  | Tailcall_self { destination; args } ->
    Format.fprintf ppf "tailcall_self %a(%a)"
      Label.format destination print_instr_array args
  | Tailcall_func (_, args) ->
    Format.fprintf ppf "tailcall_func(%a)"
      print_instr_array args
  | Call { op = Direct sym; args; continuation;
           exn_continuation } ->
    Format.fprintf ppf "call %s(%a) -> %a"
      sym.sym_name print_instr_array args Label.format
      continuation;
    (match exn_continuation with
    | Some l ->
      Format.fprintf ppf " exn %a" Label.format l
    | None -> ())
  | Call { op = Indirect _; args; continuation;
           exn_continuation } ->
    Format.fprintf ppf "call_indirect(%a) -> %a"
      print_instr_array args Label.format continuation;
    (match exn_continuation with
    | Some l ->
      Format.fprintf ppf " exn %a" Label.format l
    | None -> ())
  | Prim { op = External { func_symbol; _ }; args;
           continuation; exn_continuation } ->
    Format.fprintf ppf "prim %s(%a) -> %a" func_symbol
      print_instr_array args Label.format continuation;
    (match exn_continuation with
    | Some l ->
      Format.fprintf ppf " exn %a" Label.format l
    | None -> ())
  | Prim { op = Probe { name; _ }; args; continuation; _ }
    ->
    Format.fprintf ppf "probe %s(%a) -> %a" name
      print_instr_array args Label.format continuation

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
  | FunctionStart -> Format.fprintf ppf "function_start"
  | CallContinuation { predecessor } ->
    Format.fprintf ppf "call_cont pred=%a" Label.format
      predecessor
  | TrapHandler { predecessors } ->
    Format.fprintf ppf "trap_handler %a" print_preds
      predecessors

let print_block ppf (block : basic_block) =
  Format.fprintf ppf "%a: %a(%a)@." Label.format
    block.label print_block_desc block.desc
    Printcmm.machtype block.params;
  Array.iter
    (fun bi ->
      Format.fprintf ppf "  %a@." print_body_instruction bi)
    block.body;
  Format.fprintf ppf "  %a@." print_terminator
    block.terminator

let print ppf (t : t) =
  Format.fprintf ppf "ssa %s(%a)@." t.fun_name
    Printcmm.machtype t.fun_args;
  Format.fprintf ppf "  entry = %a@.@." Label.format
    t.entry_label;
  Label.Tbl.iter
    (fun _label block -> print_block ppf block)
    t.blocks;
  Format.fprintf ppf "@."

(* === SSA invariant validation === *)

let validate (t : t) =
  let error fmt =
    Format.kasprintf
      (fun s ->
        Misc.fatal_errorf "SSA validation (%s): %s"
          t.fun_name s)
      fmt
  in
  (* Check entry block exists *)
  if not (Label.Tbl.mem t.blocks t.entry_label)
  then
    error "entry block %a not found"
      Label.format t.entry_label;
  (* Compute actual predecessors from terminators *)
  let actual_preds = Label.Tbl.create 16 in
  Label.Tbl.iter
    (fun _ bl ->
      Label.Tbl.replace actual_preds bl.label [])
    t.blocks;
  let add_pred ~src ~dst =
    match Label.Tbl.find_opt actual_preds dst with
    | Some ps ->
      Label.Tbl.replace actual_preds dst (src :: ps)
    | None ->
      error "block %a references non-existent \
             successor %a"
        Label.format src Label.format dst
  in
  let successor_labels label (term : terminator) =
    match term with
    | Never -> ()
    | Always { goto; _ } -> add_pred ~src:label ~dst:goto
    | Branch { conditions; else_goto } ->
      Array.iter
        (fun (_, lbl) -> add_pred ~src:label ~dst:lbl)
        conditions;
      add_pred ~src:label ~dst:else_goto
    | Switch (labels, _) ->
      Array.iter
        (fun lbl -> add_pred ~src:label ~dst:lbl)
        labels
    | Return _ | Raise _ | Tailcall_func _ -> ()
    | Tailcall_self { destination; _ } ->
      add_pred ~src:label ~dst:destination
    | Call { continuation; exn_continuation; _ }
    | Prim { continuation; exn_continuation; _ } ->
      add_pred ~src:label ~dst:continuation;
      (match exn_continuation with
      | Some l -> add_pred ~src:label ~dst:l
      | None -> ())
  in
  Label.Tbl.iter
    (fun label (bl : basic_block) ->
      successor_labels label bl.terminator)
    t.blocks;
  (* Collect all defined Op ids and check uniqueness *)
  let defined_ops = InstructionId.Tbl.create 64 in
  Label.Tbl.iter
    (fun label (bl : basic_block) ->
      Array.iter
        (fun (bi : body_instruction) ->
          match bi with
          | Instr (Op { id; _ }) ->
            if InstructionId.Tbl.mem defined_ops id
            then
              error "block %a: duplicate Op id v%d"
                Label.format label
                (InstructionId.hash id);
            InstructionId.Tbl.replace defined_ops id ()
          | Instr (Block_param _ | Proj _)
          | Pushtrap _ | Poptrap _ | Stack_check _
          | Debuginfo _ -> ())
        bl.body)
    t.blocks;
  (* Validate each block *)
  Label.Tbl.iter
    (fun label (bl : basic_block) ->
      (* BlockParam types match block params *)
      let check_instr_ref (i : instruction) =
        match i with
        | Block_param { block; index; typ } ->
          (match Label.Tbl.find_opt t.blocks block
           with
          | None ->
            error "block %a: BlockParam references \
                   non-existent block %a"
              Label.format label Label.format block
          | Some target ->
            if index < 0
               || index >= Array.length target.params
            then
              error "block %a: BlockParam index %d \
                     out of range for block %a \
                     (params length %d)"
                Label.format label index
                Label.format block
                (Array.length target.params);
            let expected = target.params.(index) in
            if not
                 (Cmm.equal_machtype_component
                    expected typ)
            then
              error "block %a: BlockParam %a.%d has \
                     type %a but block params say %a"
                Label.format label Label.format block
                index Printcmm.machtype_component typ
                Printcmm.machtype_component expected)
        | Op _ | Proj _ -> ()
      in
      let rec check_instruction (i : instruction) =
        check_instr_ref i;
        match i with
        | Op { id; args; _ } ->
          if not
               (InstructionId.Tbl.mem defined_ops id)
          then
            error "block %a: Op v%d referenced but \
                   not defined in any block body"
              Label.format label
              (InstructionId.hash id);
          Array.iter check_instruction args
        | Proj { src; _ } ->
          (match src with
           | Op _ -> ()
           | Block_param _ | Proj _ ->
             error "block %a: Proj source must be \
                    an Op"
               Label.format label);
          check_instruction src
        | Block_param _ -> ()
      in
      (* Check body instructions *)
      Array.iter
        (fun (bi : body_instruction) ->
          match bi with
          | Instr i -> check_instruction i
          | Pushtrap _ | Poptrap _ | Stack_check _
          | Debuginfo _ -> ())
        bl.body;
      (* Check terminator instruction refs *)
      let check_term_args args =
        Array.iter check_instruction args
      in
      (match bl.terminator with
      | Never -> ()
      | Always { goto; args } ->
        check_term_args args;
        (* Goto args count must match target params *)
        (match Label.Tbl.find_opt t.blocks goto with
        | None -> () (* already reported above *)
        | Some target ->
          if Array.length args
             <> Array.length target.params
          then
            error "block %a: goto %a has %d args \
                   but target has %d params"
              Label.format label Label.format goto
              (Array.length args)
              (Array.length target.params))
      | Branch { conditions; else_goto } ->
        Array.iter
          (fun (cond, lbl) ->
            check_instruction cond;
            (match Label.Tbl.find_opt t.blocks lbl
             with
            | None -> ()
            | Some target ->
              if Array.length target.params > 0
              then
                error "block %a: Branch target %a \
                       has block parameters"
                  Label.format label Label.format
                  lbl))
          conditions;
        (match Label.Tbl.find_opt t.blocks else_goto
         with
        | None -> ()
        | Some target ->
          if Array.length target.params > 0
          then
            error "block %a: Branch else target %a \
                   has block parameters"
              Label.format label Label.format
              else_goto)
      | Switch (_, args) -> check_term_args args
      | Return args -> check_term_args args
      | Raise (_, args, _) -> check_term_args args
      | Tailcall_self { args; _ } ->
        check_term_args args
      | Tailcall_func (_, args) ->
        check_term_args args
      | Call { args; _ } -> check_term_args args
      | Prim { args; _ } -> check_term_args args);
      (* Check entry block is FunctionStart *)
      if Label.equal label t.entry_label
      then
        match bl.desc with
        | FunctionStart -> ()
        | Merge _ | CallContinuation _
        | TrapHandler _ ->
          error "entry block %a is not FunctionStart"
            Label.format label)
    t.blocks
