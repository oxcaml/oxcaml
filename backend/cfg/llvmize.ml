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
module String = Misc.Stdlib.String

module List = struct
  include List
  include Misc.Stdlib.List
end

type error = Asm_generation of (string * int)

exception Error of error

module Ident : sig
  (** LLVM identifiers that start with "%".  This includes
      basic blocks, function parameters, and temporaries
      (virtual registers). They can be unnamed or named.  *)
  type t

  val print : Format.formatter -> t -> unit

  val named : string -> t

  module Gen : sig
    (** per-function counter for generating identifiers *)
    type ident = t

    type t

    val create : unit -> t

    val get_fresh : t -> ident
  end
end = struct
  type t =
    | Unnamed of int
    | Named of string

  let named s = Named s

  let print fmt t =
    match t with
    | Unnamed n -> Format.fprintf fmt "%d" n
    | Named s -> Format.fprintf fmt "%s" s

  module Gen = struct
    type ident = t

    type t = { mutable next : int }

    (* Local identifiers are only valid within function scope, so we can reset
       it to 0 every time *)
    let create () = { next = 0 }

    let get_fresh t =
      let res = t.next in
      t.next <- succ res;
      Unnamed res
  end
end

module Llvm_typ = struct
  (** Type representing LLVM types *)
  type t =
    | Int of { width_in_bits : int }
    | Float (* 32-bit *)
    | Double (* 64 bit *)
    | Ptr
    | Struct of t list
    | Array of
        { size : int;
          elem_typ : t
        }

  let i64 = Int { width_in_bits = 64 }

  let i32 = Int { width_in_bits = 32 }

  let i16 = Int { width_in_bits = 16 }

  let i8 = Int { width_in_bits = 8 }

  let float = Float

  let double = Double

  let ptr = Ptr

  let bool = Int { width_in_bits = 1 }

  let of_machtyp_component (c : Cmm.machtype_component) =
    match c with
    | Val | Addr | Int ->
      Int { width_in_bits = 64 }
      (* Cfg allows vals to be assigned to ints and vice versa, so we do this to
         make LLVM happy for now. *)
    | Float -> Double
    | Float32 -> Float
    | Vec128 | Vec256 | Vec512 | Valx2 ->
      Misc.fatal_error "Llvmize.Llvm_typ.of_machtyp_component: not implemented"

  let rec pp_t ppf t =
    let open Format in
    match t with
    | Int { width_in_bits } -> fprintf ppf "i%d" width_in_bits
    | Float -> fprintf ppf "float"
    | Double -> fprintf ppf "double"
    | Ptr -> fprintf ppf "ptr"
    | Struct typs ->
      fprintf ppf "{ %a }"
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_t)
        typs
    | Array { size; elem_typ } -> fprintf ppf "[ %d x %a ]" size pp_t elem_typ

  let to_string t = Format.asprintf "%a" pp_t t

  let rec equal t1 t2 =
    match t1, t2 with
    | Int { width_in_bits = x }, Int { width_in_bits = y } -> x = y
    | Float, Float | Double, Double | Ptr, Ptr -> true
    | Struct xs, Struct ys -> List.equal equal xs ys
    | ( Array { size = size1; elem_typ = typ1 },
        Array { size = size2; elem_typ = typ2 } ) ->
      size1 = size2 && equal typ1 typ2
    | Int _, _ | Float, _ | Double, _ | Ptr, _ | Struct _, _ | Array _, _ ->
      false
end

module Calling_conventions = struct
  type t =
    | Default (* Default C calling convention *)
    | Ocaml (* See backend/<arch>/proc.ml for details *)
    | Ocaml_c_call
  (* Same as [Default] but passes the function address and stack offset (if
     there are stack arguments) through rax and r13. *)

  let to_llvmir_string = function
    | Default -> ""
    | Ocaml -> "cc 104"
    | Ocaml_c_call -> "cc 105"

  let equal t1 t2 =
    match t1, t2 with
    | Default, Default | Ocaml, Ocaml | Ocaml_c_call, Ocaml_c_call -> true
    | Default, _ | Ocaml, _ | Ocaml_c_call, _ -> false
end

(* CR yusumez: Make the rest of this file use this type *)
module Llvm_value = struct
  type t =
    | Global_ident of string
    | Local_ident of Ident.t
    | Immediate of string

  let poison = Immediate "poison"

  let pp_t ppf t =
    let open Format in
    match t with
    | Global_ident s ->
      let encoded = Asm_targets.(Asm_symbol.create s |> Asm_symbol.encode) in
      fprintf ppf "@%s" encoded
    | Local_ident ident -> fprintf ppf "%%%a" Ident.print ident
    | Immediate s -> fprintf ppf "%s" s

  (* let to_string t = Format.asprintf "%a" pp_t t *)
end

(* LLVM-level representation of a function signature. Used to collect and
   declare called functions. *)
type fun_sig =
  { cc : Calling_conventions.t;
    args : Llvm_typ.t list;
    res : Llvm_typ.t option
  }

type trap_block_info =
  { trap_block : Ident.t;
    stacksave_ptr : Ident.t;
    payload : Ident.t
  }

type fun_info =
  { fun_name : string;
    fun_has_try : bool;
        (** Whether the function contains try blocks (which means LLVM will always use a frame pointer) *)
    fun_ret_type : Cmm.machtype;
    ident_gen : Ident.Gen.t;
    reg2ident : Ident.t Reg.Tbl.t;  (** Map register's stamp to identifier  *)
    label2ident : Ident.t Label.Tbl.t;
        (** Map label to identifier. Avoid clashes between pre-existing Cfg labels and unnamed identifiers. *)
    trap_blocks : trap_block_info Label.Tbl.t
        (** Map handler labels to the corresponding pushtrap location on the stack *)
  }

type t =
  { llvmir_filename : string;
    oc : Out_channel.t;
    ppf : Format.formatter;
    ppf_dump : Format.formatter;
    mutable sourcefile : string option; (* gets set in [begin_assembly] *)
    mutable asm_filename : string option; (* gets set in [open_out] *)
    mutable current_fun_info : fun_info;
        (* Maintains the state of the current function (reset for every
           function) *)
    mutable defined_symbols : String.Set.t;
        (* Function symbols defined so far *)
    mutable referenced_symbols : String.Set.t;
        (* Global symbols referenced so far *)
    mutable called_functions : fun_sig String.Map.t;
        (* Names + signatures (args, ret) of functions called so far *)
    mutable emit_exn_intrinsic_decls : bool
        (* Whether to emit declarations / definitions for intrinsics / wrapper
           functions involved in generating try blocks *)
  }

let create_fun_info ~fun_name ~fun_has_try ~fun_ret_type =
  { fun_name;
    fun_has_try;
    fun_ret_type;
    ident_gen = Ident.Gen.create ();
    reg2ident =
      Reg.Tbl.create 37 (* CR yusumez: change this to be more reasonable *);
    label2ident = Label.Tbl.create 37;
    trap_blocks = Label.Tbl.create 37
  }

let create ~llvmir_filename ~ppf_dump =
  let oc = Out_channel.open_text llvmir_filename in
  let ppf = Format.formatter_of_out_channel oc in
  { llvmir_filename;
    asm_filename = None;
    sourcefile = None;
    oc;
    ppf;
    ppf_dump;
    current_fun_info =
      create_fun_info ~fun_name:"<no current function>" ~fun_has_try:false
        ~fun_ret_type:[||];
    defined_symbols = String.Set.empty;
    referenced_symbols = String.Set.empty;
    called_functions = String.Map.empty;
    emit_exn_intrinsic_decls = false
  }

let reset_fun_info t ~fun_name ~fun_has_try ~fun_ret_type =
  t.current_fun_info <- create_fun_info ~fun_name ~fun_has_try ~fun_ret_type

let get_ident_aux table key ~get_ident ~find_opt ~add =
  match find_opt table key with
  | Some ident -> ident
  | None ->
    let ident = get_ident key in
    add table key ident;
    ident

(* We use named identifiers for labels because their original ids are not
   ordered, but LLVM expects them to be ordered if they are unnamed *)
let get_ident_for_label t label =
  get_ident_aux t.current_fun_info.label2ident label
    ~get_ident:(fun label -> "L" ^ Label.to_string label |> Ident.named)
    ~find_opt:Label.Tbl.find_opt ~add:Label.Tbl.add

let get_ident_for_reg t reg =
  get_ident_aux t.current_fun_info.reg2ident reg
    ~get_ident:(fun _ -> Ident.Gen.get_fresh t.current_fun_info.ident_gen)
    ~find_opt:Reg.Tbl.find_opt ~add:Reg.Tbl.add

let fresh_ident t = Ident.Gen.get_fresh t.current_fun_info.ident_gen

(* CR yusumez: Write to this ppf alongside the normal one when -dllvmir is
   passed *)
let _get_ppf_dump t = t.ppf_dump

let add_called_fun t name ~cc ~args ~res =
  (match String.Map.find_opt name t.called_functions with
  | None -> ()
  | Some { cc = cc'; args = args'; res = res' } ->
    let all_equal =
      Calling_conventions.equal cc cc'
      && List.equal Llvm_typ.equal args args'
      && Option.equal Llvm_typ.equal res res'
    in
    if not all_equal
    then
      Misc.fatal_error
        "Llvmize: Same function referenced with incompatible signatures");
  t.called_functions <- String.Map.add name { cc; args; res } t.called_functions

let add_referenced_symbol t sym_name =
  t.referenced_symbols <- String.Set.add sym_name t.referenced_symbols

(* Runtime registers are passed explicitly as arguments to and returned from all
   OCaml functions. They have LLVM type ptr. *)
let domainstate_ident = Ident.named "ds"

let allocation_ident = Ident.named "alloc"

let runtime_regs = [domainstate_ident; allocation_ident]

let make_ret_type ret_types =
  let runtime_reg_types = List.map (fun _ -> Llvm_typ.ptr) runtime_regs in
  let actual_ret_types = List.map Llvm_typ.of_machtyp_component ret_types in
  Llvm_typ.(Struct [Struct runtime_reg_types; Struct actual_ret_types])

(* Regs in domainstate are already stored in the appropriate address as
   arguments or when returned, so we don't touch them while doing a call /
   return *)
(* CR yusumez: We don't expect to get arguments from the stack, but this might
   happen if we have too many arguments. There might be a way to limit this in
   the frontend. *)
let reg_list_for_call regs =
  Array.to_list regs |> List.filter (fun reg -> not (Reg.is_domainstate reg))

module F = struct
  open Format

  let pp_indent ppf () = fprintf ppf "  "

  let pp_comma ppf () = fprintf ppf ", "

  let line ppf = kfprintf (fun ppf -> pp_print_newline ppf ()) ppf

  (* CR gyorsh: emit metadata debuginfo (of the form !dbg <id>). For now just
     emit a comment, to help debug llvmize pass. Emit a block comment in the
     case there is a newline after it.

     yusumez: Multiline comments don't work for some reason... *)

  let do_if_comments_enabled f = if !Oxcaml_flags.dasm_comments then f ()

  let pp_dbginfo ppf dbg =
    if Debuginfo.is_none dbg
    then ()
    else fprintf ppf "[ %a ]" Debuginfo.print_compact dbg

  let pp_dbg_comment ?(newline = true) ppf name dbg =
    do_if_comments_enabled (fun () ->
        match newline with
        | true -> line ppf "; %s %a" name pp_dbginfo dbg
        | false -> fprintf ppf "; %s %a" name pp_dbginfo dbg)

  let pp_dbg_instr_aux pp_instr ppf ins =
    do_if_comments_enabled (fun () ->
        pp_indent ppf ();
        (* Replace newlines since this is a line comment *)
        let instr_str =
          asprintf "%a" pp_instr ins
          |> String.map (function '\n' -> ' ' | c -> c)
        in
        line ppf "; %s %a" instr_str pp_dbginfo ins.Cfg.dbg)

  let pp_dbg_instr_basic = pp_dbg_instr_aux Cfg.print_basic

  let pp_dbg_instr_terminator = pp_dbg_instr_aux Cfg.print_terminator

  let ins t =
    pp_indent t.ppf ();
    kfprintf (fun ppf -> pp_print_newline ppf ()) t.ppf

  let ins_unreachable t = ins t "unreachable"

  let source_filename t s = line t.ppf "source_filename = \"%s\"" s

  let pp_global ppf s =
    (* This is to escape unacceptable symbols (like brackets or commas) in
       global identifiers (which might be the case in e.g. anonymous
       functions) *)
    let encoded = Asm_targets.(Asm_symbol.create s |> Asm_symbol.encode) in
    fprintf ppf "@%s" encoded

  let pp_ident ppf ident = fprintf ppf "%%%a" Ident.print ident

  let pp_label_ident t ppf label = get_ident_for_label t label |> pp_ident ppf

  let pp_label t ppf label = fprintf ppf "label %a" (pp_label_ident t) label

  let pp_label_def t ppf label =
    let ident = get_ident_for_label t label in
    fprintf ppf "%a:" Ident.print ident

  let block_label_with_predecessors t label preds =
    pp_label_def t t.ppf label;
    if not (List.is_empty preds)
    then
      fprintf t.ppf
        "                                                ; preds = %a\n"
        (pp_print_list ~pp_sep:pp_comma (pp_label_ident t))
        preds
    else fprintf t.ppf "\n"

  let pp_reg_ident t ppf (reg : Reg.t) =
    let ident = get_ident_for_reg t reg in
    pp_ident ppf ident

  let pp_fun_arg ppf (typ, ident) =
    fprintf ppf "%a %a" Llvm_typ.pp_t typ pp_ident ident

  let pp_fun_args ppf fun_args =
    pp_print_list ~pp_sep:pp_comma pp_fun_arg ppf fun_args

  let pp_attrs ppf attrs =
    fprintf ppf "%a"
      (pp_print_list ~pp_sep:pp_print_space pp_print_string)
      attrs

  let define t ~fun_name ~fun_args ~fun_ret_type ~fun_dbg ~fun_attrs pp_body =
    pp_dbg_comment t.ppf fun_name fun_dbg;
    let cc_str = Calling_conventions.(to_llvmir_string Ocaml) in
    line t.ppf "define %s %a %a(%a) %a {" cc_str Llvm_typ.pp_t fun_ret_type
      pp_global fun_name pp_fun_args fun_args pp_attrs fun_attrs;
    pp_body ();
    line t.ppf "}";
    line t.ppf ""

  (* == LLVM instructions == *)

  let ins_load t ~src ~dst typ =
    ins t "%a = load %a, ptr %a" pp_ident dst Llvm_typ.pp_t typ pp_ident src

  let ins_store t ~src ~dst typ =
    ins t "store %a %a, ptr %a" Llvm_typ.pp_t typ pp_ident src pp_ident dst

  let ins_store_value t ~src ~dst typ =
    ins t "store %a %a, ptr %a" Llvm_typ.pp_t typ Llvm_value.pp_t src pp_ident
      dst

  let ins_load_from_reg t ident (reg : Reg.t) =
    ins_load t ~src:(get_ident_for_reg t reg) ~dst:ident
      (Llvm_typ.of_machtyp_component reg.typ)

  let ins_store_into_reg t ident (reg : Reg.t) =
    ins_store t ~src:ident ~dst:(get_ident_for_reg t reg)
      (Llvm_typ.of_machtyp_component reg.typ)

  let ins_store_global t sym (reg : Reg.t) =
    ins t "store ptr %a, ptr %a" pp_global sym (pp_reg_ident t) reg

  let ins_store_label_addr t ~label ~dst =
    ins t "store ptr blockaddress(%a, %a), ptr %a" pp_global
      t.current_fun_info.fun_name pp_ident label pp_ident dst

  (* These pairs exist since one takes [Label.t]'s coming from the CFG, while
     the other takes [Ident.t]'s we created in llvmize. *)
  let ins_branch t label = ins t "br %a" (pp_label t) label

  let ins_branch_ident t label = ins t "br label %a" pp_ident label

  let ins_alloca ?comment t ident typ =
    let pp_regname ppf () =
      match comment with
      | None -> ()
      | Some comment -> pp_dbg_comment ~newline:false ppf comment Debuginfo.none
    in
    ins t "%a = alloca %a %a" pp_ident ident Llvm_typ.pp_t typ pp_regname ()

  let ins_branch_cond t cond ifso ifnot =
    ins t "br i1 %a, %a, %a" pp_ident cond (pp_label t) ifso (pp_label t) ifnot

  let ins_branch_cond_ident t cond ifso ifnot =
    ins t "br i1 %a, label %a, label %a" pp_ident cond pp_ident ifso pp_ident
      ifnot

  (* note: `ins_switch` does not take a default label, as switches are assumed
     to be exhaustive. Since it is mandatory in LLVM, we will use a per-function
     label with an `unreachable` instruction, on a per-need basis (See
     `fun_info.needs_unreachable_label` field). *)
  let ins_switch t typ value labels =
    let discr = fresh_ident t in
    let unreachable_label = Cmm.new_label () in
    ins_load t ~src:value ~dst:discr typ;
    ins t "switch %a %a, %a [" Llvm_typ.pp_t typ pp_ident discr (pp_label t)
      unreachable_label;
    Array.iteri
      (fun index label ->
        pp_indent t.ppf ();
        ins t "%a %d, %a" Llvm_typ.pp_t typ index (pp_label t) label)
      labels;
    ins t "]";
    pp_label_def t t.ppf unreachable_label;
    fprintf t.ppf "\n";
    ins_unreachable t

  let ins_conv t op ~src ~dst ~src_typ ~dst_typ =
    ins t "%a = %s %a %a to %a" pp_ident dst op Llvm_typ.pp_t src_typ pp_ident
      src Llvm_typ.pp_t dst_typ

  let ins_binop t op arg1 arg2 res typ =
    ins t "%a = %s %a %a, %a" pp_ident res op Llvm_typ.pp_t typ pp_ident arg1
      pp_ident arg2

  let ins_binop_imm t op arg imm res typ =
    ins t "%a = %s %a %a, %s" pp_ident res op Llvm_typ.pp_t typ pp_ident arg imm

  let ins_unary_op t op arg res typ =
    ins t "%a = %s %a %a" pp_ident res op Llvm_typ.pp_t typ pp_ident arg

  let ins_icmp t cond arg1 arg2 res typ =
    ins t "%a = icmp %s %a %a, %a" pp_ident res cond Llvm_typ.pp_t typ pp_ident
      arg1 pp_ident arg2

  let ins_icmp_imm t cond arg imm res typ =
    ins t "%a = icmp %s %a %a, %s" pp_ident res cond Llvm_typ.pp_t typ pp_ident
      arg imm

  let ins_fcmp t cond arg1 arg2 res typ =
    ins t "%a = fcmp %s %a %a, %a" pp_ident res cond Llvm_typ.pp_t typ pp_ident
      arg1 pp_ident arg2

  let ins_extractvalue t ~arg ~res typ idxs =
    ins t "%a = extractvalue %a %a, %a" pp_ident res Llvm_typ.pp_t typ pp_ident
      arg
      (pp_print_list ~pp_sep:pp_comma pp_print_int)
      idxs

  let ins_insertvalue t ~res ~src ~dst ~src_typ ~dst_typ idxs =
    ins t "%a = insertvalue %a %a, %a %a, %a" pp_ident res Llvm_typ.pp_t dst_typ
      pp_ident dst Llvm_typ.pp_t src_typ pp_ident src
      (pp_print_list ~pp_sep:pp_comma pp_print_int)
      idxs

  (* Auxiliary function to passing non-local-identifier arguments (like poison
     or global idents). *)
  let ins_call_custom_arg ~attrs ~tail ~cc ~pp_name t name args res =
    let pp_arg ppf (typ, arg) =
      fprintf ppf "%a %a" Llvm_typ.pp_t typ Llvm_value.pp_t arg
    in
    let tail_str =
      if tail
      then "musttail" (* This forces LLVM to perform tail call optimisation *)
      else ""
    in
    let cc_str = Calling_conventions.to_llvmir_string cc in
    let pp_call res_typ ppf () =
      fprintf ppf "%s call %s %s %a(%a) %s" tail_str cc_str res_typ pp_name name
        (pp_print_list ~pp_sep:pp_comma pp_arg)
        args attrs
    in
    match res with
    | Some (res_typ, res) ->
      ins t "%a = %a" pp_ident res (pp_call (Llvm_typ.to_string res_typ)) ()
    | None -> ins t "%a" (pp_call "void") ()

  let ins_call ?(attrs = "") ?(tail = false) ~cc ~pp_name t name args res =
    ins_call_custom_arg ~attrs ~tail ~cc ~pp_name t name
      (List.map (fun (typ, arg) -> typ, Llvm_value.Local_ident arg) args)
      res

  (* Calculate offset of identifier and handle conversions. *)
  let do_offset ~arg_is_ptr ~res_is_ptr t ident offset =
    let int_ident =
      match arg_is_ptr with
      | false -> ident
      | true ->
        let ptr = fresh_ident t in
        ins_conv t "ptrtoint" ~src:ident ~dst:ptr ~src_typ:Llvm_typ.ptr
          ~dst_typ:Llvm_typ.i64;
        ptr
    in
    let offset_ident = fresh_ident t in
    ins_binop_imm t "add" int_ident (Int.to_string offset) offset_ident
      Llvm_typ.i64;
    match res_is_ptr with
    | false -> offset_ident
    | true ->
      let ptr = fresh_ident t in
      ins_conv t "inttoptr" ~src:offset_ident ~dst:ptr ~src_typ:Llvm_typ.i64
        ~dst_typ:Llvm_typ.ptr;
      ptr

  let load_reg_to_temp t reg =
    let temp = fresh_ident t in
    ins_load_from_reg t temp reg;
    temp

  let load_domainstate_addr ?(as_ptr = true) ?(offset = 0) t ds_field =
    let ds = fresh_ident t in
    let offset = offset + (Domainstate.idx_of_field ds_field * 8) in
    ins_load t ~src:domainstate_ident ~dst:ds Llvm_typ.i64;
    do_offset ~arg_is_ptr:false ~res_is_ptr:as_ptr t ds offset

  let read_rsp t =
    let ident = fresh_ident t in
    ins t "%a = call i64 @llvm.read_register.i64(metadata !{!\"rsp\\00\"})"
      pp_ident ident;
    ident

  let write_rsp t ident =
    ins t "call void @llvm.write_register.i64(metadata !{!\"rsp\\00\"}, i64 %a)"
      pp_ident ident

  let load_addr t (addr : Arch.addressing_mode) (i : 'a Cfg.instruction) n =
    let offset = Arch.addressing_displacement_for_llvmize addr in
    let arg = load_reg_to_temp t i.arg.(n) in
    let temp = fresh_ident t in
    let res = fresh_ident t in
    ins_binop_imm t "add" arg (Int.to_string offset) temp Llvm_typ.i64;
    ins_conv t "inttoptr" ~src:temp ~dst:res ~src_typ:Llvm_typ.i64
      ~dst_typ:Llvm_typ.ptr;
    res

  let int_comp t (comp : Operation.integer_comparison) (i : 'a Cfg.instruction)
      ~imm =
    let typ = Llvm_typ.of_machtyp_component i.arg.(0).typ in
    let comp_name =
      match comp with
      | Ceq -> "eq"
      | Cne -> "ne"
      | Clt -> "slt"
      | Cgt -> "sgt"
      | Cle -> "sle"
      | Cge -> "sge"
      | Cult -> "ult"
      | Cugt -> "ugt"
      | Cule -> "ule"
      | Cuge -> "uge"
    in
    match imm with
    | None ->
      let arg1 = load_reg_to_temp t i.arg.(0) in
      let arg2 = load_reg_to_temp t i.arg.(1) in
      let res = fresh_ident t in
      ins_icmp t comp_name arg1 arg2 res typ;
      res
    | Some n ->
      let arg = load_reg_to_temp t i.arg.(0) in
      let res = fresh_ident t in
      ins_icmp_imm t comp_name arg (string_of_int n) res typ;
      res

  let float_comp t (comp : Operation.float_comparison) (i : 'a Cfg.instruction)
      typ =
    let comp_name =
      (* CR yusumez: is not (ordered cond) == unordered (not cond)? *)
      match comp with
      | CFeq -> "oeq"
      | CFneq -> "une"
      | CFlt -> "olt"
      | CFnlt -> "uge"
      | CFgt -> "ogt"
      | CFngt -> "ule"
      | CFle -> "ole"
      | CFnle -> "ugt"
      | CFge -> "oge"
      | CFnge -> "ult"
    in
    let arg1 = load_reg_to_temp t i.arg.(0) in
    let arg2 = load_reg_to_temp t i.arg.(1) in
    let res = fresh_ident t in
    ins_fcmp t comp_name arg1 arg2 res typ;
    res

  let not_implemented_aux print_ins ?msg i =
    Misc.fatal_error
      (asprintf "Llvmize: unimplemented instruction: %a %a" print_ins i
         (Format.pp_print_option (fun ppf msg -> fprintf ppf "(%s)" msg))
         msg)

  let not_implemented_basic = not_implemented_aux Cfg.print_basic

  let not_implemented_terminator = not_implemented_aux Cfg.print_terminator

  (* == Cfg instructions == *)
  (* CR-soon yusumez: Add implementations for missing basic and terminator
     instructions *)

  (* CR yusumez: This needs to be refactored. It is really ugly. *)
  (* This will take function arguments as identifiers and return a list of
     identifiers for every return value. This exists to separate runtime
     register threading logic from everything else in [call] *)
  let call_simple ?(attrs = "") ~cc ~pp_name t fn args res_types =
    let args =
      List.map
        (fun ident ->
          let loaded = fresh_ident t in
          (* [ident] is a pointer to a pointer *)
          ins_load t ~src:ident ~dst:loaded Llvm_typ.ptr;
          Llvm_typ.ptr, Llvm_value.Local_ident loaded)
        runtime_regs
      @ args
    in
    let res_type =
      let runtime_reg_types = List.map (fun _ -> Llvm_typ.ptr) runtime_regs in
      Llvm_typ.(Struct [Struct runtime_reg_types; Struct res_types])
    in
    let res_ident = fresh_ident t in
    ins_call_custom_arg ~attrs ~cc ~tail:false ~pp_name t fn args
      (Some (res_type, res_ident));
    List.iteri
      (fun idx reg ->
        let temp = fresh_ident t in
        ins_extractvalue t ~arg:res_ident ~res:temp res_type [0; idx];
        ins_store t ~src:temp ~dst:reg Llvm_typ.ptr)
      runtime_regs;
    List.mapi
      (fun idx _ ->
        let res = fresh_ident t in
        ins_extractvalue t ~arg:res_ident ~res res_type [1; idx];
        res)
      res_types

  let call ?(tail = false) t (i : Cfg.terminator Cfg.instruction)
      (op : Cfg.func_call_operation) =
    (* Prepare arguments *)
    let args_begin, args_end =
      (* [Indirect] has the function in i.arg.(0) *)
      match op with
      | Direct _ -> 0, Array.length i.arg
      | Indirect -> 1, Array.length i.arg - 1
    in
    let arg_regs = Array.sub i.arg args_begin args_end |> reg_list_for_call in
    let args =
      List.map
        (fun ident ->
          let loaded = fresh_ident t in
          (* [ident] is a pointer to a pointer *)
          ins_load t ~src:ident ~dst:loaded Llvm_typ.ptr;
          Llvm_typ.ptr, loaded)
        runtime_regs
      @ List.map
          (fun reg ->
            Llvm_typ.of_machtyp_component reg.Reg.typ, load_reg_to_temp t reg)
          arg_regs
    in
    let arg_types = List.map fst args in
    (* Prepare return *)
    let res_regs =
      Array.to_list i.res
      (* CR yusumez: check whether registers actually get returned in ds...? *)
      |> List.filter (fun reg -> not (Reg.is_domainstate reg))
    in
    let res_type =
      if tail
      then make_ret_type (Array.to_list t.current_fun_info.fun_ret_type)
      else make_ret_type (List.map (fun reg -> reg.Reg.typ) res_regs)
    in
    let do_call ~pp_name fn =
      let res_ident = fresh_ident t in
      (* CR yusumez: Despite frame pointers being disabled and no register
         (including RBP) being callee-saved, the presence of different sized
         alloca's in different branches (which happens if there are any try
         blocks due to [Pushtrap]) causes LLVM to emit code using RBP as a frame
         pointer in such a function. And, unfortunately, LLVM doesn't save RBP
         in that case (perhaps because it is handled specially), so we have to
         do it ourselves ._. *)
      let should_save_rbp = t.current_fun_info.fun_has_try && not tail in
      if should_save_rbp
      then ins t {|call void asm sideeffect "push %%rbp", "~{rsp}"()|};
      ins_call ~tail ~cc:Ocaml ~pp_name t fn args (Some (res_type, res_ident));
      if should_save_rbp
      then ins t {|call void asm sideeffect "pop %%rbp", "~{rsp}"()|};
      res_ident
    in
    (* Do the call *)
    let res_ident =
      match op with
      | Direct { sym_name; sym_global = _ } ->
        add_called_fun t sym_name ~cc:Ocaml ~args:arg_types ~res:(Some res_type);
        do_call ~pp_name:pp_global sym_name
      | Indirect ->
        let fun_temp = load_reg_to_temp t i.arg.(0) in
        let fun_ptr_temp = fresh_ident t in
        (* ...we need this since we treat OCaml values as i64 *)
        ins_conv t "inttoptr" ~src:fun_temp ~dst:fun_ptr_temp
          ~src_typ:Llvm_typ.i64 ~dst_typ:Llvm_typ.ptr;
        do_call ~pp_name:pp_ident fun_ptr_temp
    in
    if tail
    then
      (* Return as is *)
      ins t "ret %a %a" Llvm_typ.pp_t res_type pp_ident res_ident
    else (
      (* Unpack return value *)
      List.iteri
        (fun idx reg ->
          let temp = fresh_ident t in
          ins_extractvalue t ~arg:res_ident ~res:temp res_type [0; idx];
          ins_store t ~src:temp ~dst:reg Llvm_typ.ptr)
        runtime_regs;
      List.iteri
        (fun idx reg ->
          let temp = fresh_ident t in
          ins_extractvalue t ~arg:res_ident ~res:temp res_type [1; idx];
          ins_store_into_reg t temp reg)
        res_regs)

  let return t (i : Cfg.terminator Cfg.instruction) =
    (* Build up return value on the stack and return it. *)
    let res_regs =
      Array.to_list i.arg
      |> List.filter (fun reg -> not (Reg.is_domainstate reg))
    in
    let res_type = make_ret_type (List.map (fun reg -> reg.Reg.typ) res_regs) in
    let insert ~outer_idx idents_and_types into_where =
      let _, res =
        List.fold_left
          (fun (idx, cur) (ident, typ) ->
            let loaded = fresh_ident t in
            let inserted = fresh_ident t in
            ins_load t ~src:ident ~dst:loaded typ;
            ins_insertvalue t ~res:inserted ~src:loaded ~dst:cur ~src_typ:typ
              ~dst_typ:res_type [outer_idx; idx];
            idx + 1, inserted)
          (0, into_where) idents_and_types
      in
      res
    in
    (* Start from poison... *)
    let poison_ident = fresh_ident t in
    ins t "%a = extractvalue { %a } poison, 0" pp_ident poison_ident
      Llvm_typ.pp_t res_type;
    let filled =
      poison_ident
      |> insert ~outer_idx:0 (* Runtime regs *)
           (List.map (fun ident -> ident, Llvm_typ.ptr) runtime_regs)
      |> insert ~outer_idx:1 (* Return regs *)
           (List.map
              (fun (reg : Reg.t) ->
                get_ident_for_reg t reg, Llvm_typ.of_machtyp_component reg.typ)
              res_regs)
    in
    ins t "ret %a %a" Llvm_typ.pp_t res_type pp_ident filled

  let extcall t (i : Cfg.terminator Cfg.instruction) ~func_symbol ~alloc
      ~stack_ofs ~stack_align =
    let get_c_stack () =
      let c_stack_addr = load_domainstate_addr t Domain_c_stack in
      let res = fresh_ident t in
      ins_load t ~src:c_stack_addr ~dst:res Llvm_typ.i64;
      res
    in
    let make_ocaml_c_call caml_c_call_symbol args res_types =
      add_referenced_symbol t caml_c_call_symbol;
      add_referenced_symbol t func_symbol;
      call_simple ~cc:Ocaml_c_call ~pp_name:pp_global t caml_c_call_symbol args
        res_types
      (* CR yusumez: Record frame table here *)
    in
    let call_func args res_types =
      if stack_ofs > 0
      then
        (* [caml_c_call_stack_args_llvm_backend] is a wrapper around
           [caml_c_call_stack_args] which computes the address range of the
           arguments on the stack given the offset. *)
        let args =
          [ Llvm_typ.ptr, Llvm_value.Global_ident func_symbol;
            Llvm_typ.i64, Llvm_value.Immediate (Int.to_string stack_ofs) ]
          @ List.map
              (fun (typ, ident) -> typ, Llvm_value.Local_ident ident)
              args
        in
        let caml_c_call_stack_args =
          "caml_c_call_stack_args_llvm_backend"
          ^
          match (stack_align : Cmm.stack_align) with
          | Align_16 -> ""
          | Align_32 -> "_avx"
          | Align_64 -> "_avx512"
        in
        make_ocaml_c_call caml_c_call_stack_args args res_types
      else if alloc
      then
        (* [caml_c_call] doesn't use the second argument since nothing is passed
           on the stack *)
        let args =
          [ Llvm_typ.ptr, Llvm_value.Global_ident func_symbol;
            Llvm_typ.i64, Llvm_value.poison ]
          @ List.map
              (fun (typ, ident) -> typ, Llvm_value.Local_ident ident)
              args
        in
        make_ocaml_c_call "caml_c_call" args res_types
      else
        (* Prepare stack pointer *)
        let c_sp = get_c_stack () in
        let ocaml_sp = read_rsp t in
        write_rsp t c_sp;
        (* Do the thing (omitting cc defaults to C calling convention) *)
        let res_ident = fresh_ident t in
        let res_type = Llvm_typ.Struct res_types in
        add_called_fun t func_symbol ~cc:Default ~args:(List.map fst args)
          ~res:(Some res_type);
        ins_call ~cc:Default ~pp_name:pp_global t func_symbol args
          (Some (res_type, res_ident));
        (* Recover stack pointer *)
        write_rsp t ocaml_sp;
        List.mapi
          (fun idx _ ->
            let temp = fresh_ident t in
            ins_extractvalue t ~arg:res_ident ~res:temp res_type [idx];
            temp)
          res_types
    in
    (* Prepare arguments + return type *)
    let args =
      Array.to_list i.arg
      |> List.map (fun reg ->
             Llvm_typ.of_machtyp_component reg.Reg.typ, load_reg_to_temp t reg)
    in
    let res_types =
      Array.to_list i.res
      |> List.map (fun reg -> Llvm_typ.of_machtyp_component reg.Reg.typ)
    in
    (* Do the thing *)
    let res_idents = call_func args res_types in
    (* Unpack return values (is it possible to have multiple?) *)
    List.iter2 (ins_store_into_reg t) res_idents (Array.to_list i.res)

  let terminator t (i : Cfg.terminator Cfg.instruction) =
    pp_dbg_instr_terminator t.ppf i;
    match i.desc with
    | Never -> assert false
    | Always lbl -> ins_branch t lbl
    | Parity_test b ->
      (* Check if the argument is even *)
      let arg_temp = load_reg_to_temp t i.arg.(0) in
      let is_odd = fresh_ident t in
      ins_conv t "trunc" ~src:arg_temp ~dst:is_odd
        ~src_typ:(Llvm_typ.of_machtyp_component i.arg.(0).typ)
        ~dst_typ:Llvm_typ.bool;
      (* reverse the branches *)
      ins_branch_cond t is_odd b.ifnot b.ifso
    | Truth_test b ->
      (* Check if the argument is true. *)
      let arg_temp = load_reg_to_temp t i.arg.(0) in
      let is_true = fresh_ident t in
      ins_conv t "trunc" ~src:arg_temp ~dst:is_true
        ~src_typ:(Llvm_typ.of_machtyp_component i.arg.(0).typ)
        ~dst_typ:Llvm_typ.bool;
      ins_branch_cond t is_true b.ifso b.ifnot
    | Return -> return t i
    | Int_test { lt; eq; gt; is_signed; imm } ->
      let open struct
        type comp =
          | Lt
          | Gt
      end in
      let make_comp (comp : comp) : Cmm.integer_comparison =
        match is_signed, comp with
        | Signed, Lt -> Clt
        | Signed, Gt -> Cgt
        | Unsigned, Lt -> Cult
        | Unsigned, Gt -> Cugt
      in
      let is_lt = int_comp t (make_comp Lt) i ~imm in
      let ge = fresh_ident t in
      ins_branch_cond_ident t is_lt (get_ident_for_label t lt) ge;
      line t.ppf "%a:" Ident.print ge;
      let is_gt = int_comp t (make_comp Gt) i ~imm in
      ins_branch_cond t is_gt gt eq
    | Raise raise_kind -> (
      match raise_kind with
      | Raise_notrace | Raise_regular | Raise_reraise ->
        (* CR yusumez: Handle backtraces appropriately once we have frametables
           (calling runtime functions expect a safepoint) *)
        let ds_exn_handler_sp = load_domainstate_addr t Domain_exn_handler in
        let exn_handler_sp =
          let res = fresh_ident t in
          ins_load t ~src:ds_exn_handler_sp ~dst:res Llvm_typ.i64;
          res
        in
        let previous_exn_handler_sp =
          let ptr = fresh_ident t in
          let res = fresh_ident t in
          ins_conv t "inttoptr" ~src:exn_handler_sp ~dst:ptr
            ~src_typ:Llvm_typ.i64 ~dst_typ:Llvm_typ.ptr;
          ins_load t ~src:ptr ~dst:res Llvm_typ.i64;
          res
        in
        let exn_handler_addr =
          let ptr =
            do_offset ~arg_is_ptr:false ~res_is_ptr:true t exn_handler_sp 8
          in
          let res = fresh_ident t in
          ins_load t ~src:ptr ~dst:res Llvm_typ.ptr;
          res
        in
        let exn_payload = load_reg_to_temp t i.arg.(0) in
        let new_sp =
          do_offset ~arg_is_ptr:false ~res_is_ptr:false t exn_handler_sp 16
        in
        ins_store t ~src:previous_exn_handler_sp ~dst:ds_exn_handler_sp
          Llvm_typ.i64;
        write_rsp t new_sp;
        ins t
          {|call void asm sideeffect "movq $0, %%rax; jmpq *$1", "r,r,~{rax}"(i64 %a, ptr %a)|}
          pp_ident exn_payload pp_ident exn_handler_addr;
        ins_unreachable t)
    | Float_test { width; lt; eq; gt; uo } ->
      let typ =
        match width with
        | Float64 -> Llvm_typ.double
        | Float32 -> Llvm_typ.float
      in
      let is_lt = float_comp t Cmm.CFlt i typ in
      let ge = fresh_ident t in
      ins_branch_cond_ident t is_lt (get_ident_for_label t lt) ge;
      line t.ppf "%a:" Ident.print ge;
      let is_gt = float_comp t Cmm.CFgt i typ in
      let eq_or_uo = fresh_ident t in
      ins_branch_cond_ident t is_gt (get_ident_for_label t gt) eq_or_uo;
      line t.ppf "%a:" Ident.print eq_or_uo;
      let is_eq = float_comp t Cmm.CFeq i typ in
      ins_branch_cond t is_eq eq uo
    | Call { op; label_after } ->
      call t i op;
      ins_branch t label_after
    | Prim { op; label_after } -> (
      match op with
      | Probe _ -> not_implemented_terminator ~msg:"probe" i
      | External { func_symbol; alloc; stack_ofs; stack_align; _ } ->
        extcall t i ~func_symbol ~alloc ~stack_ofs ~stack_align;
        ins_branch t label_after)
    | Tailcall_self { destination } -> ins_branch t destination
    | Tailcall_func op -> call ~tail:true t i op
    | Call_no_return { func_symbol; alloc; stack_ofs; stack_align; _ } ->
      extcall t i ~func_symbol ~alloc ~stack_ofs ~stack_align;
      ins t "unreachable"
    | Switch labels ->
      let arg = get_ident_for_reg t i.arg.(0) in
      ins_switch t Llvm_typ.i64 arg labels

  let int_op t (i : Cfg.basic Cfg.instruction)
      (op : Operation.integer_operation) ~imm =
    let typ = Llvm_typ.of_machtyp_component i.res.(0).typ in
    let do_binop op_name =
      match imm with
      | None ->
        let arg1 = load_reg_to_temp t i.arg.(0) in
        let arg2 = load_reg_to_temp t i.arg.(1) in
        let res = fresh_ident t in
        ins_binop t op_name arg1 arg2 res typ;
        res
      | Some n ->
        let arg = load_reg_to_temp t i.arg.(0) in
        let res = fresh_ident t in
        ins_binop_imm t op_name arg (string_of_int n) res typ;
        res
    in
    let do_unary_intrinsic op_name =
      let typ = Llvm_typ.i64 in
      let intrinsic_name = asprintf "llvm.%s.%a" op_name Llvm_typ.pp_t typ in
      let arg = load_reg_to_temp t i.arg.(0) in
      let res = fresh_ident t in
      add_called_fun t intrinsic_name ~cc:Default ~args:[typ] ~res:(Some typ);
      ins_call ~cc:Default ~pp_name:pp_global t intrinsic_name
        [typ, arg]
        (Some (typ, res));
      res
    in
    let res =
      match op with
      | Iadd -> do_binop "add"
      | Isub -> do_binop "sub"
      | Imul -> do_binop "mul"
      | Imulh { signed = _ } -> do_binop "mul"
      | Idiv -> do_binop "sdiv"
      | Imod -> do_binop "srem"
      | Iand -> do_binop "and"
      | Ior -> do_binop "or"
      | Ixor -> do_binop "xor"
      | Ilsl -> do_binop "shl"
      | Ilsr -> do_binop "lshr"
      | Iasr -> do_binop "ashr"
      | Icomp comp ->
        let bool_res = int_comp t comp i ~imm in
        (* convert i1 -> i64 *)
        let int_res = fresh_ident t in
        ins_conv t "zext" ~src:bool_res ~dst:int_res ~src_typ:Llvm_typ.bool
          ~dst_typ:typ;
        int_res
      | Iclz _ -> do_unary_intrinsic "ctlz"
      | Ictz _ -> do_unary_intrinsic "cttz"
      | Ipopcnt -> do_unary_intrinsic "ctpop"
    in
    ins_store_into_reg t res i.res.(0)

  let float_op t (i : Cfg.basic Cfg.instruction) (width : Cmm.float_width)
      (op : Operation.float_operation) =
    let typ =
      match width with Float32 -> Llvm_typ.float | Float64 -> Llvm_typ.double
    in
    let do_binop op_name =
      let arg1 = load_reg_to_temp t i.arg.(0) in
      let arg2 = load_reg_to_temp t i.arg.(1) in
      let res = fresh_ident t in
      ins_binop t op_name arg1 arg2 res typ;
      res
    in
    let res =
      match op with
      | Iaddf -> do_binop "fadd"
      | Isubf -> do_binop "fsub"
      | Imulf -> do_binop "fmul"
      | Idivf -> do_binop "fdiv"
      | Inegf ->
        let arg = load_reg_to_temp t i.arg.(0) in
        let res = fresh_ident t in
        ins_unary_op t "fneg" arg res typ;
        res
      | Iabsf ->
        let arg = load_reg_to_temp t i.arg.(0) in
        let res = fresh_ident t in
        let fabs_name =
          "llvm.fabs." ^ match width with Float32 -> "f32" | Float64 -> "f64"
        in
        add_called_fun t fabs_name ~cc:Default ~args:[typ] ~res:(Some typ);
        ins_call ~cc:Default ~pp_name:pp_global t fabs_name
          [typ, arg]
          (Some (typ, res));
        res
      | Icompf comp ->
        let bool_res = float_comp t comp i typ in
        (* convert i1 -> i64 *)
        let int_res = fresh_ident t in
        ins_conv t "zext" ~src:bool_res ~dst:int_res ~src_typ:Llvm_typ.bool
          ~dst_typ:(Llvm_typ.of_machtyp_component i.res.(0).typ);
        int_res
    in
    ins_store_into_reg t res i.res.(0)

  let basic_op t (i : Cfg.basic Cfg.instruction) (op : Operation.t) =
    match op with
    | Move | Opaque ->
      let temp = load_reg_to_temp t i.arg.(0) in
      ins_store_into_reg t temp i.res.(0)
    | Const_int n ->
      ins_store_value t
        ~src:Llvm_value.(Immediate (Nativeint.to_string n))
        ~dst:(get_ident_for_reg t i.res.(0))
        (Llvm_typ.of_machtyp_component i.res.(0).typ)
    | Const_symbol { sym_name; sym_global = _ } ->
      add_referenced_symbol t sym_name;
      ins_store_global t sym_name i.res.(0)
    | Const_float32 bits ->
      ins_store_value t
        ~src:Llvm_value.(Immediate (sprintf "%#lx" bits))
        ~dst:(get_ident_for_reg t i.res.(0))
        Llvm_typ.float
    | Const_float bits ->
      ins_store_value t
        ~src:Llvm_value.(Immediate (sprintf "%#Lx" bits))
        ~dst:(get_ident_for_reg t i.res.(0))
        Llvm_typ.double
    | Const_vec128 _ | Const_vec256 _ | Const_vec512 _ ->
      not_implemented_basic ~msg:"const" i
    | Load { memory_chunk; addressing_mode; mutability = _; is_atomic = _ } -> (
      (* Q: what do we do with mutability / is_atomic / is_modify? *)
      let src = load_addr t addressing_mode i 0 in
      let basic typ =
        let temp = fresh_ident t in
        ins_load t ~src ~dst:temp typ;
        ins_store_into_reg t temp i.res.(0)
      in
      let extend op typ =
        let temp = fresh_ident t in
        let temp2 = fresh_ident t in
        ins_load t ~src ~dst:temp typ;
        ins_conv t op ~src:temp ~dst:temp2 ~src_typ:typ ~dst_typ:Llvm_typ.i64;
        ins_store_into_reg t temp2 i.res.(0)
      in
      match memory_chunk with
      | Word_int | Word_val -> basic Llvm_typ.i64
      | Byte_unsigned -> extend "zext" Llvm_typ.i8
      | Byte_signed -> extend "sext" Llvm_typ.i8
      | Sixteen_unsigned -> extend "zext" Llvm_typ.i16
      | Sixteen_signed -> extend "sext" Llvm_typ.i16
      | Thirtytwo_unsigned -> extend "zext" Llvm_typ.i32
      | Thirtytwo_signed -> extend "sext" Llvm_typ.i32
      | Single { reg = Float32 } -> basic Llvm_typ.float
      | Double -> basic Llvm_typ.double
      | Single { reg = Float64 } ->
        let loaded = fresh_ident t in
        let extended = fresh_ident t in
        ins_load t ~src ~dst:loaded Llvm_typ.float;
        ins_conv t "fpext" ~src:loaded ~dst:extended ~src_typ:Llvm_typ.float
          ~dst_typ:Llvm_typ.double;
        ins_store_into_reg t extended i.res.(0)
      | Onetwentyeight_unaligned | Onetwentyeight_aligned
      | Twofiftysix_unaligned | Twofiftysix_aligned | Fivetwelve_unaligned
      | Fivetwelve_aligned ->
        not_implemented_basic ~msg:"load" i)
    | Store (chunk, addr, _is_modify) -> (
      let dst = load_addr t addr i 1 in
      let basic typ =
        let temp = fresh_ident t in
        ins_load_from_reg t temp i.arg.(0);
        ins_store t ~src:temp ~dst typ
      in
      let trunc ?(trunc_op = "trunc") dst_typ =
        let reg_typ = i.arg.(0).typ |> Llvm_typ.of_machtyp_component in
        let temp = fresh_ident t in
        let truncated = fresh_ident t in
        ins_load_from_reg t temp i.arg.(0);
        ins_conv t trunc_op ~src:temp ~dst:truncated ~src_typ:reg_typ ~dst_typ;
        ins_store t ~src:truncated ~dst dst_typ
      in
      match chunk with
      | Word_int | Word_val -> basic Llvm_typ.i64
      | Byte_unsigned | Byte_signed -> trunc Llvm_typ.i8
      | Sixteen_unsigned | Sixteen_signed -> trunc Llvm_typ.i16
      | Thirtytwo_signed | Thirtytwo_unsigned -> trunc Llvm_typ.i32
      | Single { reg = Float32 } -> basic Llvm_typ.float
      | Double -> basic Llvm_typ.double
      | Single { reg = Float64 } -> trunc ~trunc_op:"fptrunc" Llvm_typ.float
      | Onetwentyeight_unaligned | Onetwentyeight_aligned
      | Twofiftysix_unaligned | Twofiftysix_aligned | Fivetwelve_unaligned
      | Fivetwelve_aligned ->
        not_implemented_basic ~msg:"store" i)
    | Intop op -> int_op t i op ~imm:None
    | Intop_imm (op, n) -> int_op t i op ~imm:(Some n)
    | Floatop (width, op) -> float_op t i width op
    | Stackoffset _ -> () (* We leave stack handling to LLVM *)
    | Begin_region ->
      let local_sp_addr = load_domainstate_addr t Domain_local_sp in
      let local_sp = fresh_ident t in
      ins_load t ~src:local_sp_addr ~dst:local_sp Llvm_typ.i64;
      ins_store_into_reg t local_sp i.res.(0)
    | End_region ->
      let arg = load_reg_to_temp t i.arg.(0) in
      let local_sp = load_domainstate_addr t Domain_local_sp in
      ins_store t ~src:arg ~dst:local_sp Llvm_typ.i64
    | Alloc { bytes; dbginfo = _; mode } -> (
      match mode with
      | Local ->
        (* Make space in local_sp *)
        let local_sp_ptr = load_domainstate_addr t Domain_local_sp in
        let local_sp = fresh_ident t in
        let new_local_sp = fresh_ident t in
        ins_load t ~src:local_sp_ptr ~dst:local_sp Llvm_typ.i64;
        ins_binop_imm t "sub" local_sp (Int.to_string bytes) new_local_sp
          Llvm_typ.i64;
        ins_store t ~src:new_local_sp ~dst:local_sp_ptr Llvm_typ.i64;
        (* Check if new_local_sp exceeds local_limit *)
        let local_limit_ptr = load_domainstate_addr t Domain_local_limit in
        let local_limit = fresh_ident t in
        let skip_realloc = fresh_ident t in
        let call_realloc = fresh_ident t in
        let after_alloc = fresh_ident t in
        ins_load t ~src:local_limit_ptr ~dst:local_limit Llvm_typ.i64;
        ins_icmp t "slt" local_limit new_local_sp skip_realloc Llvm_typ.i64;
        ins_branch_cond_ident t skip_realloc after_alloc call_realloc;
        (* Call realloc *)
        line t.ppf "%a:" Ident.print call_realloc;
        (* CR yusumez: handle simd regs appropriately once we have them *)
        add_called_fun t "caml_call_local_realloc" ~cc:Default ~args:[]
          ~res:None;
        ins_call ~attrs:"cold" ~cc:Default ~pp_name:pp_global t
          "caml_call_local_realloc" [] None;
        ins_branch_ident t after_alloc;
        (* After alloc *)
        line t.ppf "%a:" Ident.print after_alloc;
        let local_top_ptr = load_domainstate_addr t Domain_local_top in
        let local_top = fresh_ident t in
        let new_local_sp_addr = fresh_ident t in
        let new_local_sp_addr_offset = fresh_ident t in
        ins_load t ~src:local_top_ptr ~dst:local_top Llvm_typ.i64;
        ins_binop t "add" new_local_sp local_top new_local_sp_addr Llvm_typ.i64;
        ins_binop_imm t "add" new_local_sp_addr "8" new_local_sp_addr_offset
          Llvm_typ.i64;
        ins_store_into_reg t new_local_sp_addr_offset i.res.(0)
      | Heap ->
        let alloc_ptr = fresh_ident t in
        let new_alloc_ptr = fresh_ident t in
        ins_load t ~src:allocation_ident ~dst:alloc_ptr Llvm_typ.i64;
        ins_binop_imm t "sub" alloc_ptr (Int.to_string bytes) new_alloc_ptr
          Llvm_typ.i64;
        ins_store t ~src:new_alloc_ptr ~dst:allocation_ident Llvm_typ.i64;
        let domain_young_limit =
          let ptr = load_domainstate_addr t Domain_young_limit in
          let res = fresh_ident t in
          ins_load t ~src:ptr ~dst:res Llvm_typ.i64;
          res
        in
        let should_skip_gc = fresh_ident t in
        let gc_call = fresh_ident t in
        let after_gc_call =
          (* CR yusumez: We need to think about how we create identifiers... *)
          Ident.named ("after." ^ Format.asprintf "%a" Ident.print gc_call)
        in
        ins_icmp t "ult" domain_young_limit new_alloc_ptr should_skip_gc
          Llvm_typ.i64;
        ins_branch_cond_ident t should_skip_gc after_gc_call gc_call;
        line t.ppf "%a:" Ident.print gc_call;
        (* CR yusumez: handle simd regs appropriately once we have them *)
        (* Note that we use [call_simple] here to properly adjust R15. *)
        add_called_fun t "caml_call_gc" ~cc:Ocaml
          ~args:[Llvm_typ.ptr; Llvm_typ.ptr]
          ~res:(Some (make_ret_type []));
        call_simple ~cc:Ocaml ~pp_name:pp_global t "caml_call_gc" [] []
        |> ignore;
        ins_branch_ident t after_gc_call;
        line t.ppf "%a:" Ident.print after_gc_call;
        let alloc_ptr_after_gc = fresh_ident t in
        let res = fresh_ident t in
        ins_load t ~src:allocation_ident ~dst:alloc_ptr_after_gc Llvm_typ.i64;
        ins_binop_imm t "add" alloc_ptr_after_gc "8" res Llvm_typ.i64;
        ins_store_into_reg t res i.res.(0))
    | Spill | Reload -> not_implemented_basic ~msg:"spill / reload" i
    | Intop_atomic _ | Csel _ | Reinterpret_cast _ | Static_cast _
    | Probe_is_enabled _ | Specific _ | Name_for_debugger _ | Dls_get | Poll
    | Pause ->
      not_implemented_basic i

  let basic t (i : Cfg.basic Cfg.instruction) =
    pp_dbg_instr_basic t.ppf i;
    match i.desc with
    | Op op -> basic_op t i op
    | Prologue | Epilogue | Reloadretaddr -> () (* LLVM handles these for us *)
    | Poptrap { lbl_handler } -> (
      match Label.Tbl.find_opt t.current_fun_info.trap_blocks lbl_handler with
      | None -> Misc.fatal_error "Llvmize: unbalanced trap pop"
      | Some { trap_block; stacksave_ptr; payload = _ } ->
        (* Restore previous exn handler sp *)
        let ds_exn_handler_addr = load_domainstate_addr t Domain_exn_handler in
        let prev_sp =
          let res = fresh_ident t in
          ins_load t ~src:trap_block ~dst:res Llvm_typ.i64;
          res
        in
        ins_store t ~src:prev_sp ~dst:ds_exn_handler_addr Llvm_typ.i64;
        (* Pop! *)
        add_called_fun t "llvm.stackrestore" ~cc:Default ~args:[Llvm_typ.ptr]
          ~res:None;
        ins_call ~cc:Default ~pp_name:pp_global t "llvm.stackrestore"
          [Llvm_typ.ptr, stacksave_ptr]
          None)
    | Pushtrap { lbl_handler } -> (
      t.emit_exn_intrinsic_decls <- true;
      (* Call a function that wraps a dummy LLVM intrinsic that always returns
         0. We set the handler address to be right after this call to emulate
         setjmp's behaviour without actually saving the state to a buffer.

         In particular, this ensures registers get spilled (thanks to the OCaml
         calling convention), the handler is not removed as dead code (thanks to
         the branch), and no breaking control flow optimisations are made
         (thanks to the "returns twice" attribute). *)
      call_simple ~attrs:"returns_twice" ~cc:Ocaml ~pp_name:pp_global t
        "wrap_try" [] [Llvm_typ.i32]
      |> ignore (* Note that we don't need the returned identifier here. *);
      (* Record label here - we will jump here for the handler *)
      let pre_try_label = fresh_ident t in
      ins_branch_ident t pre_try_label;
      line t.ppf "%a:" Ident.print pre_try_label;
      (* Extract the result of the call, or the exception payload. *)
      let payload = fresh_ident t in
      ins t {|%a = call i64 asm sideeffect "mov %%rax, $0", "=r"()|} pp_ident
        payload;
      (* If it's nonzero, we have an exception. Otherwise, go to the try
         block. *)
      let zero_check = fresh_ident t in
      let try_block_label = fresh_ident t in
      ins_icmp_imm t "eq" payload "0" zero_check Llvm_typ.i64;
      ins_branch_cond_ident t zero_check try_block_label
        (get_ident_for_label t lbl_handler);
      (* Enter try block from this point onwards. *)
      line t.ppf "%a:" Ident.print try_block_label;
      (* Save state of stack *)
      let stacksave_ptr = fresh_ident t in
      add_called_fun t "llvm.stacksave" ~cc:Default ~args:[]
        ~res:(Some Llvm_typ.ptr);
      ins_call ~cc:Default ~pp_name:pp_global t "llvm.stacksave" []
        (Some (Llvm_typ.ptr, stacksave_ptr));
      let trap_block = fresh_ident t in
      (* [prev_sp; handler_addr; saved_rbp; padding] - we need to save rbp since
         it will get clobbered by the time we get to the handler, and LLVM is
         forced to use rbp since different branches have different stack sizes.
         Padding is to maintain alignment... *)
      ins_alloca ~comment:"push trap" t trap_block
        Llvm_typ.(Struct [i64; i64; i64; i64]);
      let ds_exn_handler_addr = load_domainstate_addr t Domain_exn_handler in
      let ds_exn_handler =
        let res = fresh_ident t in
        ins_load t ~src:ds_exn_handler_addr ~dst:res Llvm_typ.i64;
        res
      in
      let rbp_slot =
        do_offset ~arg_is_ptr:true ~res_is_ptr:true t trap_block 16
      in
      let handler_slot =
        do_offset ~arg_is_ptr:true ~res_is_ptr:true t trap_block 8
      in
      let prev_sp_slot =
        do_offset ~arg_is_ptr:true ~res_is_ptr:true t trap_block 0
      in
      ins t {|call void asm sideeffect "mov %%rbp, ($0)", "r"(ptr %a)|} pp_ident
        rbp_slot;
      ins_store_label_addr t ~label:pre_try_label ~dst:handler_slot;
      ins_store t ~src:ds_exn_handler ~dst:prev_sp_slot Llvm_typ.i64;
      ins_store t ~src:trap_block ~dst:ds_exn_handler_addr Llvm_typ.ptr;
      match Label.Tbl.find_opt t.current_fun_info.trap_blocks lbl_handler with
      | Some _ ->
        Misc.fatal_error "Llvmize: Multiple pushtraps for the same handler"
      | None ->
        Label.Tbl.add t.current_fun_info.trap_blocks lbl_handler
          { trap_block; stacksave_ptr; payload })
    | Stack_check _ ->
      if Config.no_stack_checks || not !Oxcaml_flags.cfg_stack_checks
      then () (* Don't emit stack checks *)
      else not_implemented_basic ~msg:"stack check" i

  (* == Cfg data items == *)

  (* CR yusumez: don't do these matches once we have the structured type for
     [data_item] *)
  let typ_of_data_item (d : Cmm.data_item) =
    match d with
    | Cdefine_symbol _ | Calign _ | Csymbol_offset _ ->
      (* [Calign] and [Csymbol_offset] are never produced *)
      Misc.fatal_error "Llvmize: unexpected data item"
    | Cint _ -> Llvm_typ.i64
    | Cint8 _ -> Llvm_typ.i8
    | Cint16 _ -> Llvm_typ.i16
    | Cint32 _ -> Llvm_typ.i32
    | Csymbol_address _ -> Llvm_typ.Ptr
    | Cstring s ->
      Llvm_typ.Array { size = String.length s; elem_typ = Llvm_typ.i8 }
    | Cskip size -> Llvm_typ.Array { size; elem_typ = Llvm_typ.i8 }
    | Csingle _ -> Llvm_typ.float
    | Cdouble _ -> Llvm_typ.double
    | Cvec128 _ | Cvec256 _ | Cvec512 _ ->
      Misc.fatal_error "Llvmize: vector data items not implemented"

  let pp_const_data_item ppf (d : Cmm.data_item) =
    match d with
    | Cdefine_symbol _ | Calign _ | Csymbol_offset _ ->
      Misc.fatal_error "Llvmize.typ_of_data_item: unexpected data item"
    | Cint n | Cint32 n -> fprintf ppf "%nd" n
    | Cint8 n | Cint16 n -> fprintf ppf "%d" n
    | Csymbol_address { sym_name; sym_global = _ } -> pp_global ppf sym_name
    | Cstring s ->
      fprintf ppf "c\"";
      String.iter (fun c -> fprintf ppf "\\%02x" (Char.code c)) s;
      fprintf ppf "\""
    | Cskip _ -> fprintf ppf "zeroinitializer"
    | Csingle f | Cdouble f ->
      fprintf ppf "%.20f" f
      (* 64-bit floats with at least 17 digits are guaranteed to round trip
         exactly through string conversions by the IEEE 754 standard (9 digits
         for 32-bit floats). *)
    | Cvec128 _ | Cvec256 _ | Cvec512 _ ->
      Misc.fatal_error "Llvmize: vector data item snot implemented"

  let pp_typ_and_const ppf (d : Cmm.data_item) =
    fprintf ppf "%a %a" Llvm_typ.pp_t (typ_of_data_item d) pp_const_data_item d

  let data_decl ?(private_ = false) ?(align = Some 8) t sym
      (ds : Cmm.data_item list) =
    ignore private_;
    (* CR yusumez: If private global variables are not referenced anywhere
       directly, LLVM will delete them in the globaldce (dead global
       elimination) pass, so we don't mark anything as private for now. *)
    line t.ppf "%a = global %a { %a }%s" pp_global sym Llvm_typ.pp_t
      (Llvm_typ.Struct (List.map typ_of_data_item ds))
      (pp_print_list ~pp_sep:pp_comma pp_typ_and_const)
      ds
      (match align with
      | None -> ""
      | Some align -> ", align " ^ Int.to_string align)

  let data_decl_extern t sym =
    line t.ppf "%a = external global %a" pp_global sym Llvm_typ.pp_t
      Llvm_typ.ptr

  let empty_symbol_decl t sym = data_decl t (Cmm_helpers.make_symbol sym) []

  let empty_fun_decl t sym =
    line t.ppf "define void %a() { ret void }" pp_global
      (Cmm_helpers.make_symbol sym)

  let fun_decl t sym { cc; args; res } =
    let res_str =
      match res with Some res -> Llvm_typ.to_string res | None -> "void"
    in
    let cc_str = Calling_conventions.to_llvmir_string cc in
    line t.ppf "declare %s %s %a(%a)" cc_str res_str pp_global sym
      (pp_print_list ~pp_sep:pp_comma Llvm_typ.pp_t)
      args
end

let current_compilation_unit = ref None

let get_current_compilation_unit msg =
  match !current_compilation_unit with
  | Some t -> t
  | None ->
    Misc.fatal_error
      (Format.sprintf "Llvmize: current compilation unit not set (%s)" msg)

(* Create LLVM IR file for the current compilation unit. *)
let init ~output_prefix ~ppf_dump =
  let llvmir_filename = output_prefix ^ ".ll" in
  current_compilation_unit := Some (create ~llvmir_filename ~ppf_dump)

let close_out () =
  match !current_compilation_unit with
  | None -> ()
  | Some t ->
    (* Exception raised during llvmize, keep .ll file. *)
    Out_channel.close t.oc;
    current_compilation_unit := None

let open_out ~asm_filename =
  let t = get_current_compilation_unit "open_out" in
  t.asm_filename <- Some asm_filename

let fun_attrs _t _codegen_options : string list =
  (* CR gyorsh: translate and communicate to llvm backend *)
  []

let collect_body_regs cfg =
  Cfg.fold_blocks cfg
    ~f:(fun _ block regs ->
      let body_regs =
        DLL.fold_left block.body
          ~f:(fun regs (instr : Cfg.basic Cfg.instruction) ->
            Reg.add_set_array regs (Array.append instr.res instr.arg))
          ~init:Reg.Set.empty
      in
      let terminator_regs =
        Array.append block.terminator.res block.terminator.arg
      in
      Reg.add_set_array body_regs terminator_regs |> Reg.Set.union regs)
    ~init:Reg.Set.empty

(* Allocates regs in [cfg] on the stack and fills up the [reg2ident] table in
   [t]. Note that:

   - Arguments are assigned an identifier in the argument list but the body will
   use the alloca'd idents instead.

   - Arguments not passed in registers (e.g. in Domainstate) will point to that
   block of memory instead of being allocated on the stack.

   - Runtime registers themselves (those being registers that contain pointers
   to certain runtime data structures that need to be preserved) are also put on
   the stack.

   - The stack allocations are only in the IR level and LLVM will (hopefully!)
   optimize most of these alloca's out to registers. *)
let make_temps_for_regs t cfg args_and_signature_idents runtime_arg_idents =
  (* First allocate runtime registers (the rest might reference them) *)
  List.iter2
    (fun old_ident new_ident ->
      F.ins_alloca t new_ident Llvm_typ.ptr;
      F.ins_store t ~src:old_ident ~dst:new_ident Llvm_typ.ptr)
    runtime_arg_idents runtime_regs;
  let make_temp (reg : Reg.t) ident_opt =
    match reg.loc with
    (* [Outgoing] is for extcalls only - these will get put on the stack by
       LLVM, so we treat them as normal temporaries. We don't expect OCaml calls
       to use the stack for us... *)
    | Unknown | Reg _ | Stack (Outgoing _) -> (
      (* This will reserve a fresh ident *)
      F.ins_alloca
        ~comment:(Format.asprintf "%a" Printreg.reg reg)
        t (get_ident_for_reg t reg)
        (Llvm_typ.of_machtyp_component reg.typ);
      match ident_opt with
      | None -> () (* not an argument *)
      | Some old_ident -> F.ins_store_into_reg t old_ident reg)
    | Stack (Domainstate idx) ->
      (* Compute pointer to where [reg] is located - we can assume [ds] will not
         change through the run of this function *)
      let res = F.load_domainstate_addr ~offset:idx t Domain_extra_params in
      (* Create entry in the [reg2ident] table *)
      Reg.Tbl.add t.current_fun_info.reg2ident reg res
    | Stack (Local _) | Stack (Incoming _) ->
      Misc.fatal_error "Llvmize: unexpected register location"
  in
  let arg_regs = List.map fst args_and_signature_idents |> Reg.Set.of_list in
  let body_regs = collect_body_regs cfg in
  let temp_regs = Reg.Set.diff body_regs arg_regs in
  List.iter
    (fun (reg, ident_opt) -> make_temp reg ident_opt)
    args_and_signature_idents;
  Reg.Set.iter (fun reg -> make_temp reg None) temp_regs

let trap_handler_entry t (block : Cfg.basic_block) label =
  match[@ocaml.warning "-4"]
    DLL.hd block.body |> Option.map (fun i -> i, i.Cfg.desc)
  with
  | Some (i, Op Move) -> (
    match Label.Tbl.find_opt t.current_fun_info.trap_blocks label with
    | Some { payload; _ } ->
      (* Restore RBP (+ remove padding) *)
      F.ins t {|call void asm sideeffect "pop %%rbp; addq $$8, %%rsp", ""()|};
      (* Restore allocation pointer *)
      let new_alloc_ptr = fresh_ident t in
      F.ins t {|%a = call i64 asm sideeffect "movq %%r15, $0", "=r"()|}
        F.pp_ident new_alloc_ptr;
      F.ins_store t ~src:new_alloc_ptr ~dst:allocation_ident Llvm_typ.i64;
      (* Move payload to appropriate temp *)
      F.ins_store_into_reg t payload i.arg.(0)
    | None -> ())
  | _ ->
    Misc.fatal_error "Llvmize: first instruction of trap handler not a move"

let cfg (cl : CL.t) =
  let t = get_current_compilation_unit "cfg" in
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
        next_instruction_id = _;
        fun_ret_type
      } =
    cfg
  in
  let fun_has_try =
    DLL.exists layout ~f:(fun label ->
        (Label.Tbl.find blocks label).is_trap_handler)
  in
  reset_fun_info t ~fun_name ~fun_has_try ~fun_ret_type;
  t.defined_symbols <- String.Set.add fun_name t.defined_symbols;
  (* Make fresh idents for argument regs since these will be different from
     idents assigned to them later on *)
  let runtime_arg_idents = List.map (fun _ -> fresh_ident t) runtime_regs in
  let fun_args_with_idents =
    Array.to_list fun_args
    |> List.map (fun (arg : Reg.t) ->
           let is_listed_in_signature =
             match arg.loc with
             | Reg _ -> true
             | Stack _ -> false
             | Unknown -> Misc.fatal_error "Llvmize: unknown parameter location"
           in
           if is_listed_in_signature
           then arg, Some (fresh_ident t)
           else arg, None)
  in
  let pp_block label =
    let block = Label.Tbl.find blocks label in
    let preds = Cfg.predecessor_labels block in
    if Label.equal entry_label label && not (List.is_empty preds)
    then Misc.fatal_errorf "Llvmize: entry label must not have predecessors";
    F.block_label_with_predecessors t label preds;
    if block.is_trap_handler then trap_handler_entry t block label;
    DLL.iter ~f:(F.basic t) block.body;
    F.terminator t block.terminator
  in
  let pp_body () =
    (* First unused numbered identifier after the argument list is reserved, so
       we cannot use it. We explicitly skip it here *)
    let (_ : Ident.t) = Ident.Gen.get_fresh t.current_fun_info.ident_gen in
    make_temps_for_regs t cfg fun_args_with_idents runtime_arg_idents;
    F.ins_branch t entry_label;
    DLL.iter ~f:pp_block layout
  in
  let fun_attrs = fun_attrs t fun_codegen_options in
  let fun_args =
    List.map (fun ident -> Llvm_typ.ptr, ident) runtime_arg_idents
    @ List.filter_map
        (fun ((reg : Reg.t), ident) ->
          match ident with
          | None -> None
          | Some ident -> Some (Llvm_typ.of_machtyp_component reg.typ, ident))
        fun_args_with_idents
  in
  let fun_ret_type = make_ret_type (Array.to_list fun_ret_type) in
  F.define t ~fun_name ~fun_args ~fun_ret_type ~fun_dbg ~fun_attrs pp_body

(* CR yusumez: We make some assumptions about the structure of the data items we
   receive and decode it manually here. Ideally, [data_item]s would be
   represented in a more structured manner, but this should do for now. *)
let make_temp_data_symbol =
  let idx = ref 0 in
  fun () ->
    let module_name =
      Compilation_unit.(get_current_or_dummy () |> name |> Name.to_string)
    in
    let res = Format.asprintf ".temp.%s.%d" module_name !idx in
    incr idx;
    res

let data (ds : Cmm.data_item list) =
  let t = get_current_compilation_unit "data" in
  let define_symbol' ~private_ ~header ~symbol contents =
    let symbol =
      match symbol with
      | None -> make_temp_data_symbol ()
      | Some symbol -> symbol
    in
    (match header with
    | None -> ()
    | Some header ->
      let header_sym = ".header." ^ symbol in
      F.data_decl ~private_:true t header_sym [Cint header]);
    F.data_decl ~private_ t symbol contents;
    t.defined_symbols <- String.Set.add symbol t.defined_symbols;
    List.iter
      (fun (d : Cmm.data_item) ->
        match[@warning "-4"] d with
        | Csymbol_address { sym_name; sym_global = _ } ->
          t.referenced_symbols <- String.Set.add sym_name t.referenced_symbols
        | _ -> ())
      contents
  in
  let define_symbol ~private_ ~header ~symbol contents =
    if private_ && List.is_empty contents
    then () (* No need to declare a private symbol with no contents *)
    else define_symbol' ~private_ ~header ~symbol contents
  in
  let peek_int = function[@warning "-4"] Cmm.Cint n -> Some n | _ -> None in
  let peek_define_symbol = function[@warning "-4"]
    | Cmm.Cdefine_symbol { sym_name; sym_global = _ } -> Some sym_name
    | _ -> None
  in
  let eat_if peek = function
    | [] -> None
    | d :: ds -> peek d |> Option.map (fun i -> i, ds)
  in
  let closure_block ds =
    let function_slot ds =
      let header, ds = eat_if peek_int ds |> Option.get in
      let symbol, ds = eat_if peek_define_symbol ds |> Option.get in
      (*= A function slot is either:
          | code pointer | closinfo | (if the function has arity 0 or 1)
          | code pointer | closinfo | second code pointer | (arity >= 2)
          See mlvalues.h for details. *)
      let closinfo = List.nth ds 1 |> peek_int |> Option.get in
      let arity = Nativeint.shift_right_logical closinfo 56 in
      let slot_size = if arity <= 1n then 2 else 3 in
      let slot, tail = List.split_at slot_size ds in
      define_symbol ~private_:false ~header:(Some header) ~symbol:(Some symbol)
        slot;
      let is_last =
        Nativeint.(shift_right_logical (shift_left closinfo 8) (size - 1) = 1n)
      in
      tail, is_last
    in
    let rec iter_slots ds =
      let tail, is_last = function_slot ds in
      if is_last
      then define_symbol ~private_:true ~header:None ~symbol:None tail
      else iter_slots tail
    in
    iter_slots ds
  in
  let block ds =
    match eat_if peek_int ds with
    | Some (i, after_i) -> (
      match eat_if peek_define_symbol after_i with
      | Some (symbol, after_symbol) ->
        (* [i] is a header *)
        if Nativeint.(logand i 0xffn = of_int Obj.closure_tag)
        then closure_block ds
        else
          define_symbol ~private_:false ~header:(Some i) ~symbol:(Some symbol)
            after_symbol
      | None ->
        (* [i] is not a header *)
        define_symbol ~private_:true ~header:None ~symbol:None ds)
    | None -> (
      (* No header *)
      match eat_if peek_define_symbol ds with
      | Some (symbol, after_symbol) ->
        define_symbol ~private_:false ~header:None ~symbol:(Some symbol)
          after_symbol
      | None -> define_symbol ~private_:true ~header:None ~symbol:None ds)
  in
  try block ds
  with _ -> Misc.fatal_error "Llvmize: error while decoding data items"

(* Declare menitoned but not declared data items as extern *)
let declare_data t =
  String.Set.diff t.referenced_symbols t.defined_symbols
  |> String.Set.iter (fun sym -> F.data_decl_extern t sym)

let declare_stack_intrinsics t =
  F.line t.ppf "declare i64 @llvm.read_register.i64(metadata)";
  F.line t.ppf "declare void @llvm.write_register.i64(metadata, i64)"

let declare_exn_intrinsics t =
  if t.emit_exn_intrinsic_decls
  then (
    F.line t.ppf "declare i32 @llvm.eh.ocaml.try()";
    F.line t.ppf "%s"
      {|define private cc 104 {ptr, ptr, i32} @wrap_try(ptr %r14, ptr %r15) returns_twice noinline {
  %1 = call i32 @llvm.eh.ocaml.try()
  %t1 = extractvalue {{ptr, ptr, i32}} poison, 0
  %t2 = insertvalue {ptr, ptr, i32} %t1, ptr %r14, 0
  %t3 = insertvalue {ptr, ptr, i32} %t2, ptr %r15, 1
  %t4 = insertvalue {ptr, ptr, i32} %t3, i32 %1, 2
  ret {ptr, ptr, i32} %t4
}|})

let declare_functions t =
  String.Map.iter
    (fun sym fun_sig ->
      if not (String.Set.mem sym t.defined_symbols)
      then F.fun_decl t sym fun_sig;
      t.defined_symbols <- String.Set.add sym t.defined_symbols)
    t.called_functions;
  declare_stack_intrinsics t;
  declare_exn_intrinsics t

let remove_file filename =
  try if Sys.file_exists filename then Sys.remove filename
  with Sys_error _msg -> ()

let llvmir_to_assembly t =
  (* CR-someday gyorsh: add other optimization flags and control which passes to
     perform. *)
  let cmd =
    match !Oxcaml_flags.llvm_path with Some path -> path | None -> Config.asm
  in
  let fp_flags =
    if Config.with_frame_pointers
    then ["-fno-omit-frame-pointer"]
    else ["-fomit-frame-pointer"; "-momit-leaf-frame-pointer"]
  in
  match t.asm_filename with
  | None -> 0
  | Some asm_filename ->
    Ccomp.command
      (String.concat " "
         ([cmd; "-o"; Filename.quote asm_filename; "-O3"; "-S"]
         @ fp_flags
         @ ["-x ir"; Filename.quote t.llvmir_filename]))

let assemble_file ~asm_filename ~obj_filename =
  let cmd =
    match !Oxcaml_flags.llvm_path with Some path -> path | None -> Config.asm
  in
  Ccomp.command
    (String.concat " "
       [ cmd;
         "-c";
         "-o";
         Filename.quote obj_filename;
         Filename.quote asm_filename ])

let begin_assembly ~sourcefile =
  let t = get_current_compilation_unit "begin_asm" in
  t.sourcefile <- sourcefile;
  (* Source filename needs to get emitted before *)
  Option.iter (F.source_filename t) sourcefile;
  (* CR yusumez: Get target triple *)
  (* F.line t.ppf "target triple = \"x86_64-redhat-linux-gnu\""; *)
  Format.pp_print_newline t.ppf ();
  F.empty_symbol_decl t "data_begin";
  F.empty_fun_decl t "code_begin";
  Format.pp_print_newline t.ppf ()

(* CR yusumez: [begin_assembly] and [end_assembly] emit extra things to the .ll
   file, so they always need to be called. However, this will still generate an
   assembly file if -stop-after simplify_cfg or -stop_after linearization are
   passed, which it shouldn't do. *)

let end_assembly () =
  let t = get_current_compilation_unit "end_asm" in
  (* Declare functions not already defined *)
  declare_functions t;
  Format.pp_print_newline t.ppf ();
  (* Emit data declarations *)
  declare_data t;
  Format.pp_print_newline t.ppf ();
  F.empty_symbol_decl t "data_end";
  F.empty_fun_decl t "code_end";
  F.empty_symbol_decl t "frametable";
  (* Close channel to .ll file *)
  Out_channel.close t.oc;
  (* Call clang to compile .ll to .s *)
  let ret_code = llvmir_to_assembly t in
  if ret_code <> 0
  then
    raise
      (Error
         (Asm_generation
            ( Option.value ~default:"(no source file specified)" t.sourcefile,
              ret_code )));
  if not !Oxcaml_flags.keep_llvmir then remove_file t.llvmir_filename;
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
   need both of them with ocaml ?

   yusumez: For now, we are just emitting them as global constants.
   code_begin/end are empty function decl's so that they end up in the proper
   section. However, we currently don't have control over where they end up in
   the asm file. *)
(* CR gyorsh: assume 64-bit architecture *)
(* CR yusumez: We ignore whether symbols are local/global. *)

(* Error report *)

let report_error ppf = function
  | Asm_generation (fn, ret_code) ->
    Format.fprintf ppf "Error producing assembly code for %s: %d" fn ret_code

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)
