(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                              Jane Street                               *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
[@@@ocaml.warning "+a-30-40-41-42"]

module CL = Cfg_with_layout
module DLL = Oxcaml_utils.Doubly_linked_list

type error = Asm_generation of (string * int)

exception Error of error

module Ident : sig
  (** Unnamed LLVM identifiers that start with "%".  This includes
      basic blocks, unnamed function parameters, and temporaries
      (virtual registers).  *)
  type t

  val print : Format.formatter -> t -> unit

  module Gen : sig
    (** per-function counter for generating identifiers *)
    type ident = t

    type t

    val create : unit -> t

    val get_fresh : t -> ident
  end
end = struct
  type t = int

  let print fmt t = Format.fprintf fmt "%d" t

  module Gen = struct
    type ident = t

    type nonrec t = { mutable next : ident }

    (* Local identifiers are only valid within function scope, so we can reset
       it to 0 every time *)
    let create () = { next = 0 }

    let get_fresh t =
      let res = t.next in
      t.next <- succ res;
      res
  end
end

module Llvm_typ = struct
  (** Type representing LLVM types *)
  type t =
    | Int of int  (** Argument is the bitwidth of the integer *)
    | Ptr
    | Struct of t list

  let of_machtyp_component (c : Cmm.machtype_component) =
    match c with
    | Val | Addr | Int ->
      Int 64
      (* Cfg allows vals to be assigned to ints and vice versa, so we do this to
         make LLVM happy for now. *)
    | Float | Vec128 | Vec256 | Vec512 | Float32 | Valx2 ->
      Misc.fatal_error "Not implemented [Llvm_typ.of_machtyp_component]"

  let rec pp_t ppf t =
    let open Format in
    match t with
    | Int n -> fprintf ppf "i%d" n
    | Ptr -> fprintf ppf "ptr"
    | Struct typs ->
      fprintf ppf "{ %a }"
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_t)
        typs

  let to_string t = Format.asprintf "%a" pp_t t
end

type fun_info =
  { ident_gen : Ident.Gen.t;
    reg2ident : Ident.t Reg.Tbl.t;  (** Map register's stamp to identifier  *)
    label2ident : Ident.t Label.Tbl.t;
        (** Map label to identifier. Avoid clashes between pre-existing Cfg labels and unnamed identifiers. *)
    mutable arg_regs : Reg.Set.t;
        (** Registers mentioned in the argument list of the function. *)
    mutable temp_regs : Reg.Set.t
        (** Temporary registers mentioned in the body of the function. *)
  }

type t =
  { llvmir_filename : string;
    oc : Out_channel.t;
    ppf : Format.formatter;
    asm_filename : string;
    current_fun_info : fun_info ref; (* should be reset for every function *)
    data : Cmm.data_item list ref
  }

let create_fun_info () =
  { ident_gen = Ident.Gen.create ();
    reg2ident =
      Reg.Tbl.create 37 (* CR yusumez: change this to be more reasonable *);
    label2ident = Label.Tbl.create 37;
    arg_regs = Reg.Set.empty;
    temp_regs = Reg.Set.empty
  }

let create ~llvmir_filename ~asm_filename =
  let oc = Out_channel.open_text llvmir_filename in
  let ppf = Format.formatter_of_out_channel oc in
  let current_fun_info = ref (create_fun_info ()) in
  let data = ref [] in
  { llvmir_filename; asm_filename; oc; ppf; current_fun_info; data }

let reset_fun_info t = t.current_fun_info := create_fun_info ()

let get_ident_for_label t label =
  let fun_info = !(t.current_fun_info) in
  match Label.Tbl.find_opt fun_info.label2ident label with
  | Some ident -> ident
  | None ->
    let ident = Ident.Gen.get_fresh fun_info.ident_gen in
    Label.Tbl.add fun_info.label2ident label ident;
    ident

let get_ident_for_reg t reg =
  let fun_info = !(t.current_fun_info) in
  match Reg.Tbl.find_opt fun_info.reg2ident reg with
  | Some ident -> ident
  | None ->
    let ident = Ident.Gen.get_fresh fun_info.ident_gen in
    Reg.Tbl.add fun_info.reg2ident reg ident;
    ident

module F = struct
  open Format

  let pp_indent ppf () = fprintf ppf "  "

  let pp_comma ppf () = fprintf ppf ", "

  let line ppf = kfprintf (fun ppf -> pp_print_newline ppf ()) ppf

  let pp_dbg ppf dbg =
    (* CR gyorsh: emit metadata debuginfo. For now just emit a comment, to help
       debug llvmize pass. Emit a block comment in the case there is a newline
       after it. *)
    (* CR yusumez: Multiline comments don't work...? *)
    fprintf ppf "  ; !dbg <id> %a" Debuginfo.print_compact dbg

  let pp_dbg_instr ppf instr =
    let dbg =
      match instr with
      | `Basic (i : Cfg.basic Cfg.instruction) -> i.dbg
      | `Terminator (i : Cfg.terminator Cfg.instruction) -> i.dbg
    in
    fprintf ppf "  ; !dbg <id> %a [ %a ]" Debuginfo.print_compact dbg
      Cfg.print_instruction instr

  (* let pp_dbg_newline ppf dbg = line ppf "%a" pp_dbg dbg *)

  let pp_dbg_instr_newline ppf dbg = line ppf "%a" pp_dbg_instr dbg

  let ins t =
    pp_indent t.ppf ();
    kfprintf (fun ppf -> pp_print_newline ppf ()) t.ppf

  let _source_filename t s = line t.ppf "source_filename = \"%s\"" s

  let machtyp_component (m : Cmm.machtype_component) =
    Llvm_typ.(of_machtyp_component m |> to_string)

  let pp_machtyp_component ppf c = Format.fprintf ppf "%s" (machtyp_component c)

  let machtyp machtyp =
    if Array.length machtyp = 1
    then machtyp_component machtyp.(0)
    else
      Llvm_typ.(
        Struct (Array.map of_machtyp_component machtyp |> Array.to_list)
        |> to_string)

  let pp_machtyp ppf cs = Format.fprintf ppf "%s" (machtyp cs)

  let pp_global ppf s = fprintf ppf "@%s" s

  let pp_ident ppf ident = fprintf ppf "%%%a" Ident.print ident

  let pp_label_ident t ppf label = get_ident_for_label t label |> pp_ident ppf

  let pp_label t ppf label = fprintf ppf "label %a" (pp_label_ident t) label

  let pp_label_def t ppf label =
    let ident = get_ident_for_label t label in
    fprintf ppf "%a:" Ident.print ident

  let block_label_with_predecessors t label preds =
    pp_label_def t t.ppf label;
    (* XCR gyorsh: map label to unique ident *)
    if not (Misc.Stdlib.List.is_empty preds)
    then
      fprintf t.ppf
        "                                                ; preds = %a\n"
        (pp_print_list ~pp_sep:pp_comma (pp_label_ident t))
        preds
    else fprintf t.ppf "\n"

  let pp_reg_ident t ppf (reg : Reg.t) =
    let ident = get_ident_for_reg t reg in
    pp_ident ppf ident

  let pp_reg_typ_and_ident t ppf (reg : Reg.t) =
    fprintf ppf "%a %a" pp_machtyp_component reg.typ (pp_reg_ident t) reg

  let pp_fun_args t ppf fun_args =
    Array.to_seq fun_args
    |> pp_print_seq ~pp_sep:pp_comma (pp_reg_typ_and_ident t) ppf

  let pp_attrs ppf attrs =
    fprintf ppf "%a" (pp_print_list ~pp_sep:pp_comma pp_print_string) attrs

  let define t ~fun_name ~fun_args ~fun_res_type ~fun_dbg ~fun_attrs pp_body =
    line t.ppf "%a" pp_dbg fun_dbg;
    line t.ppf "define %a @%s(%a) %a {" pp_machtyp fun_res_type fun_name
      (pp_fun_args t) fun_args pp_attrs fun_attrs;
    pp_body ();
    line t.ppf "}"

  let alloca t (reg : Reg.t) =
    ins t "%a = alloca %a" (pp_reg_ident t) reg pp_machtyp_component reg.typ

  (* CR-soon yusumez: Add implementations for missing basic and terminator
     instructions *)

  let fresh_ident t = Ident.Gen.get_fresh !(t.current_fun_info).ident_gen

  (* CR yusumez: loads/stores might be of different sizes *)
  let ins_load t ident (reg : Reg.t) =
    ins t "%a = load %a, ptr %a" pp_ident ident pp_machtyp_component reg.typ
      (pp_reg_ident t) reg

  let ins_store t ident (reg : Reg.t) =
    ins t "store %a %a, ptr %a" pp_machtyp_component reg.typ pp_ident ident
      (pp_reg_ident t) reg

  let ins_store_global t sym (reg : Reg.t) =
    ins t "store ptr %a, ptr %a" pp_global sym (pp_reg_ident t) reg

  let ins_store_nativeint t n (reg : Reg.t) =
    ins t "store %a %s, ptr %a" pp_machtyp_component reg.typ
      (Nativeint.to_string n) (pp_reg_ident t) reg

  let load_reg_to_temp t reg =
    let temp = fresh_ident t in
    ins_load t temp reg;
    temp

  let terminator t (i : Cfg.terminator Cfg.instruction) =
    pp_dbg_instr_newline t.ppf (`Terminator i);
    match i.desc with
    | Never -> assert false
    | Always lbl -> ins t "br %a" (pp_label t) lbl
    | Parity_test b ->
      (* Check if the argument is even *)
      let arg_temp = load_reg_to_temp t i.arg.(0) in
      let is_odd = fresh_ident t in
      ins t "%a = trunc %a %a to i1" pp_ident is_odd pp_machtyp_component
        i.arg.(0).typ pp_ident arg_temp;
      (* reverse the branches *)
      ins t "br i1 %a, %a, %a" pp_ident is_odd (pp_label t) b.ifnot (pp_label t)
        b.ifso
    | Truth_test b ->
      (* Check if the argument is true. *)
      let arg_temp = load_reg_to_temp t i.arg.(0) in
      let is_true = fresh_ident t in
      ins t "%a = trunc %a %a to i1" pp_ident is_true pp_machtyp_component
        i.arg.(0).typ pp_ident arg_temp;
      ins t "br i1 %a, %a, %a" pp_ident is_true (pp_label t) b.ifso (pp_label t)
        b.ifnot
    | Return ->
      let temp = load_reg_to_temp t i.arg.(0) in
      ins t "ret %a %a" pp_machtyp_component i.arg.(0).typ pp_ident temp
    | Float_test _ | Int_test _ | Switch _ | Raise _ | Tailcall_self _
    | Tailcall_func _ | Call_no_return _ | Call _ | Prim _ ->
      Misc.fatal_error
        (asprintf "Unimplemented instruction: %a" Cfg.print_instruction
           (`Terminator i))

  let int_op t (i : Cfg.basic Cfg.instruction)
      (op : Operation.integer_operation) =
    match op with
    | Iadd ->
      let temp1 = load_reg_to_temp t i.arg.(0) in
      let temp2 = load_reg_to_temp t i.arg.(1) in
      let add_res = fresh_ident t in
      ins t "%a = add %a %a, %a" pp_ident add_res pp_machtyp_component
        i.res.(0).typ pp_ident temp1 pp_ident temp2;
      ins_store t add_res i.res.(0)
    | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl | Ilsr
    | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ->
      Misc.fatal_error
        (asprintf "Unimplemented instruction: %a" Cfg.print_instruction
           (`Basic i))

  let basic_op t (i : Cfg.basic Cfg.instruction) (op : Operation.t) =
    match op with
    | Move ->
      let temp = load_reg_to_temp t i.arg.(0) in
      ins_store t temp i.res.(0)
    | Const_int n -> ins_store_nativeint t n i.res.(0)
    | Const_symbol { sym_name; sym_global = _ } ->
      ins_store_global t sym_name i.res.(0)
    | Intop op -> int_op t i op
    | Spill | Reload | Const_float32 _ | Const_float _ | Const_vec128 _
    | Const_vec256 _ | Const_vec512 _ | Stackoffset _ | Load _ | Store _
    | Intop_imm _ | Intop_atomic _ | Floatop _ | Csel _ | Reinterpret_cast _
    | Static_cast _ | Probe_is_enabled _ | Opaque | Begin_region | End_region
    | Specific _ | Name_for_debugger _ | Dls_get | Poll | Pause | Alloc _ ->
      Misc.fatal_error
        (asprintf "Unimplemented instruction: %a" Cfg.print_instruction
           (`Basic i))

  let basic t (i : Cfg.basic Cfg.instruction) =
    pp_dbg_instr_newline t.ppf (`Basic i);
    match i.desc with
    | Op op -> basic_op t i op
    | Prologue | Reloadretaddr -> ()
    | Poptrap _ | Pushtrap _ | Stack_check _ ->
      Misc.fatal_error
        (asprintf "Unimplemented instruction: %a" Cfg.print_instruction
           (`Basic i))

  let typ_of_data_item (d : Cmm.data_item) =
    match d with
    | Cdefine_symbol _ -> assert false (* cannot happen *)
    | Cint _ -> Llvm_typ.Int 64
    | Csymbol_address _ -> Llvm_typ.Ptr
    | Cint8 _ | Cint16 _ | Cint32 _ | Csingle _ | Cdouble _ | Cvec128 _
    | Cvec256 _ | Cvec512 _ | Csymbol_offset _ | Cstring _ | Cskip _ | Calign _
      ->
      Misc.fatal_error
        "Unimplemented data item encountered in [Llvmize.typ_of_data_item]"

  let pp_const_data_item ppf (d : Cmm.data_item) =
    match d with
    | Cdefine_symbol _ -> assert false (* cannot happen *)
    | Cint n -> fprintf ppf "%s" (Nativeint.to_string n)
    | Csymbol_address { sym_name; sym_global = _ } -> pp_global ppf sym_name
    | Cint8 _ | Cint16 _ | Cint32 _ | Csingle _ | Cdouble _ | Cvec128 _
    | Cvec256 _ | Cvec512 _ | Csymbol_offset _ | Cstring _ | Cskip _ | Calign _
      ->
      Misc.fatal_error
        "Unimplemented data item encountered in [Llvmize.pp_const_data_item]"

  let pp_typ_and_const ppf (d : Cmm.data_item) =
    fprintf ppf "%a %a" Llvm_typ.pp_t (typ_of_data_item d) pp_const_data_item d

  let data_decl t sym (ds : Cmm.data_item list) =
    line t.ppf "%a = global %a { %a }" pp_global sym Llvm_typ.pp_t
      (Llvm_typ.Struct (List.map typ_of_data_item ds))
      (pp_print_list ~pp_sep:pp_comma pp_typ_and_const)
      ds

  let symbol_decl t sym =
    line t.ppf "%a = global i64 0" pp_global (Cmm_helpers.make_symbol sym)

  let empty_fun_decl t sym =
    line t.ppf "define void %a() { ret void }" pp_global
      (Cmm_helpers.make_symbol sym)
end

let current_compilation_unit = ref None

let llvmir_to_assembly t =
  (* CR-someday gyorsh: add other optimization flags and control which passes to
     perform. *)
  let cmd =
    match !Oxcaml_flags.llvm_path with Some path -> path | None -> Config.asm
  in
  Ccomp.command
    (String.concat " "
       (cmd
       :: [ "-o";
            Filename.quote t.asm_filename;
            "-O3";
            "-S";
            "-x ir";
            Filename.quote t.llvmir_filename ]))

let close_out () =
  match !current_compilation_unit with
  | None -> ()
  | Some t ->
    (* Exception raised during llvmize, keep .ll file. *)
    Out_channel.close t.oc;
    current_compilation_unit := None

let open_out ~asm_filename ~output_prefix =
  (* Create LLVM IR file for the current compilation unit. *)
  let llvmir_filename = output_prefix ^ ".ll" in
  current_compilation_unit := Some (create ~llvmir_filename ~asm_filename)

let fun_attrs _t _codegen_options : string list =
  (* CR gyorsh: translate and communicate to llvm backend *)
  []

(* Mark regs we need to allocate *)
let collect_regs t (cfg : Cfg.t) =
  let body_regs =
    Cfg.fold_blocks cfg
      ~f:(fun _ block regs ->
        let body_regs =
          DLL.fold_left block.body
            ~f:(fun regs (instr : Cfg.basic Cfg.instruction) ->
              Reg.Set.add_seq
                (Seq.append (Array.to_seq instr.res) (Array.to_seq instr.arg))
                regs)
            ~init:Reg.Set.empty
        in
        let terminator_regs = Array.to_seq block.terminator.res in
        Reg.Set.add_seq terminator_regs body_regs |> Reg.Set.union regs)
      ~init:Reg.Set.empty
  in
  let arg_regs = Array.to_seq cfg.fun_args |> Reg.Set.of_seq in
  let temp_regs = Reg.Set.diff body_regs arg_regs in
  !(t.current_fun_info).arg_regs <- arg_regs;
  !(t.current_fun_info).temp_regs <- temp_regs

let alloca_regs t =
  let arg_regs = !(t.current_fun_info).arg_regs in
  let temp_regs = !(t.current_fun_info).temp_regs in
  (* At this point, [t] will have mapped the args to idents. We will be using
     the alloca'd idents instead, so we save them and make [t] forget about the
     old ones. *)
  let old_arg_idents =
    Reg.Set.to_seq arg_regs |> List.of_seq
    |> List.map (fun reg -> reg, get_ident_for_reg t reg)
  in
  Reg.Tbl.clear !(t.current_fun_info).reg2ident;
  Reg.Set.iter (F.alloca t) arg_regs;
  Reg.Set.iter (F.alloca t) temp_regs;
  List.iter (fun (reg, old_ident) -> F.ins_store t old_ident reg) old_arg_idents

let cfg (cl : CL.t) =
  let t = Option.get !current_compilation_unit in
  reset_fun_info t;
  let layout = CL.layout cl in
  let cfg = CL.cfg cl in
  (* CR gyorsh: handle unboxed return type (where is this info? do we need to
     find [Return] instruction to find it? if so, add a field to [Cfg.t] to
     store it explicitly. *)
  let { Cfg.blocks;
        fun_name;
        fun_args;
        fun_codegen_options;
        fun_dbg;
        entry_label;
        fun_contains_calls = _ (* not used at this point *);
        fun_num_stack_slots = _ (* only available after regalloc *);
        fun_poll = _ (* not needed after poll insertion *);
        next_instruction_id = _
      } =
    cfg
  in
  let pp_block label =
    let block = Label.Tbl.find blocks label in
    let preds = Cfg.predecessor_labels block in
    if Label.equal entry_label label && not (Misc.Stdlib.List.is_empty preds)
    then Misc.fatal_errorf "Entry label must not have predecessors";
    F.block_label_with_predecessors t label preds;
    DLL.iter ~f:(F.basic t) block.body;
    F.terminator t block.terminator
  in
  let pp_body () =
    (* last argument id + 1 is reserved, so we cannot use it. we explicitly skip
       it here *)
    let (_ : Ident.t) = Ident.Gen.get_fresh !(t.current_fun_info).ident_gen in
    alloca_regs t;
    F.ins t "br %a" (F.pp_label t) entry_label;
    DLL.iter ~f:pp_block layout
  in
  let fun_attrs = fun_attrs t fun_codegen_options in
  (* fill fields in [t] before emitting *)
  collect_regs t cfg;
  F.define t ~fun_name ~fun_args ~fun_res_type:Cmm.typ_val ~fun_dbg ~fun_attrs
    pp_body

(* CR yusumez: Implement this *)
let data ds =
  let t = Option.get !current_compilation_unit in
  t.data := List.append !(t.data) ds

(* CR yusumez: We do this cumbersome list wrangling since we receive data
   declarations as a flat list. Ideally, [data_item]s would be represented in a
   more structured manner which we can directly pass on to [declare]. *)
let collect_data t ~declare =
  let cur_sym, ds =
    List.fold_left
      (fun (cur_sym, ds) (d : Cmm.data_item) ->
        match d with
        | Cdefine_symbol { sym_name; sym_global = _ } ->
          Option.iter (fun cur_sym -> declare cur_sym ds) cur_sym;
          Some sym_name, []
        | Cint _ | Csymbol_address _ -> cur_sym, ds @ [d]
        | Cint8 _ | Cint16 _ | Cint32 _ | Csingle _ | Cdouble _ | Cvec128 _
        | Cvec256 _ | Cvec512 _ | Csymbol_offset _ | Cstring _ | Cskip _
        | Calign _ ->
          Misc.fatal_error "Unimplemented data item")
      (None, []) !(t.data)
  in
  Option.iter (fun cur_sym -> declare cur_sym ds) cur_sym

let remove_file filename =
  try if Sys.file_exists filename then Sys.remove filename
  with Sys_error _msg -> ()

let begin_assembly () =
  let t = Option.get !current_compilation_unit in
  (* CR yusumez: Source filename needs to get emitted here (it won't work at the
     end), but we don't have access to it here. It isn't required for now. *)
  (* Option.iter (F.source_filename t) sourcefile; *)
  F.line t.ppf "target triple = \"x86_64-redhat-linux-gnu\"";
  F.symbol_decl t "data_begin";
  F.empty_fun_decl t "code_begin";
  Format.pp_print_newline t.ppf ()

let end_assembly ~sourcefile =
  let t = Option.get !current_compilation_unit in
  (* Emit source_file *)
  (* Emit data declarations *)
  collect_data t ~declare:(F.data_decl t);
  F.symbol_decl t "data_end";
  F.empty_fun_decl t "code_end";
  F.symbol_decl t "frametable";
  (* Close channel to .ll file *)
  Out_channel.close t.oc;
  (* Call clang to compile .ll to .s *)
  let ret_code = llvmir_to_assembly t in
  if ret_code <> 0
  then
    raise
      (Error
         (Asm_generation
            ( Option.value ~default:"(no source file specified)" sourcefile,
              ret_code )));
  if not !Oxcaml_flags.dump_llvmir then remove_file t.llvmir_filename;
  current_compilation_unit := None

(* CR-someday gyorsh: currently, llvm backend can be selected at the compilation
   unit granularity only. It could be controlled at the function granularity. *)
(* CR-someday gyorsh: Compiling directly to .o would involve more changes to
   [Asmgen], [Asmlink], and drivers. It would improve compilation speed but not
   as much as avoiding the textual representation entirely by linking in the
   llvm library statically. *)
(* CR-someday gyorsh: we could set [binary_backend_available] but it is
   currently too tightly coupled with the [internal_assembler], especially in
   [asmlink] for shared libraries. *)
(* CR gyorsh: how to emit data_begin/end and code_begin/end symbol? Do we still
   need both of them with ocaml ? yusumez: For now, we are just emitting them as
   global constants. *)
(* CR gyorsh: assume 64-bit architecture *)
(* Error report *)

let report_error ppf = function
  | Asm_generation (fn, ret_code) ->
    Format.fprintf ppf "Error producing assembly code for %s: %d" fn ret_code

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)
