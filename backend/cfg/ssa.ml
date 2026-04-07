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
        index : int
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
  | Switch of Label.t array
  | Return of instruction array
  | Raise of Lambda.raise_kind * instruction array
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
  | Block_param { block; index } ->
    Format.fprintf ppf "%a.%d" Label.format block index
  | Proj { index; src } ->
    Format.fprintf ppf "proj(%d, %a)" index print_instr_ref
      src

and print_instr_ref ppf (i : instruction) =
  match i with
  | Op { id; _ } ->
    Format.fprintf ppf "v%d" (InstructionId.hash id)
  | Block_param { block; index } ->
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
  | Switch labels ->
    Format.fprintf ppf "switch [%a]"
      (Format.pp_print_array ~pp_sep:(fun ppf () ->
           Format.fprintf ppf ", ")
         Label.format)
      labels
  | Return args ->
    Format.fprintf ppf "return(%a)" print_instr_array args
  | Raise (_, args) ->
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
