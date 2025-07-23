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

  let to_string t = Format.asprintf "%a" pp_t t
end

type fun_info =
  { ident_gen : Ident.Gen.t;
    reg2ident : Ident.t Reg.Tbl.t;  (** Map register's stamp to identifier  *)
    label2ident : Ident.t Label.Tbl.t
        (** Map label to identifier. Avoid clashes between pre-existing Cfg labels and unnamed identifiers. *)
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
    mutable data : Cmm.data_item list;
        (* Collects data items as they come and processes them at the end *)
    mutable defined_symbols : String.Set.t;
        (* Keeps track of all function symbols defined so far *)
    mutable referenced_symbols : String.Set.t
        (* Keeps track of all global symbols referenced so far *)
  }

let create_fun_info () =
  { ident_gen = Ident.Gen.create ();
    reg2ident =
      Reg.Tbl.create 37 (* CR yusumez: change this to be more reasonable *);
    label2ident = Label.Tbl.create 37
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
    current_fun_info = create_fun_info ();
    data = [];
    defined_symbols = String.Set.empty;
    referenced_symbols = String.Set.empty
  }

let reset_fun_info t = t.current_fun_info <- create_fun_info ()

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

  let source_filename t s = line t.ppf "source_filename = \"%s\"" s

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

  let pp_fun_arg ppf (reg, ident) =
    fprintf ppf "%a %a" pp_machtyp_component reg.Reg.typ pp_ident ident

  let pp_fun_args ppf fun_args =
    pp_print_list ~pp_sep:pp_comma pp_fun_arg ppf fun_args

  let pp_attrs ppf attrs =
    fprintf ppf "%a" (pp_print_list ~pp_sep:pp_comma pp_print_string) attrs

  let define t ~fun_name ~fun_args ~fun_ret_type ~fun_dbg ~fun_attrs pp_body =
    pp_dbg_comment t.ppf fun_name fun_dbg;
    line t.ppf "define %a @%s(%a) %a {" pp_machtyp fun_ret_type fun_name
      pp_fun_args fun_args pp_attrs fun_attrs;
    pp_body ();
    line t.ppf "}";
    line t.ppf ""

  (* == LLVM instructions == *)

  let ins_load t ~src ~dst typ =
    ins t "%a = load %a, ptr %a" pp_ident dst Llvm_typ.pp_t typ pp_ident src

  let ins_store t ~src ~dst typ =
    ins t "store %a %a, ptr %a" Llvm_typ.pp_t typ pp_ident src pp_ident dst

  let ins_load_from_reg t ident (reg : Reg.t) =
    ins_load t ~src:(get_ident_for_reg t reg) ~dst:ident
      (Llvm_typ.of_machtyp_component reg.typ)

  let ins_store_into_reg t ident (reg : Reg.t) =
    ins_store t ~src:ident ~dst:(get_ident_for_reg t reg)
      (Llvm_typ.of_machtyp_component reg.typ)

  let ins_store_global t sym (reg : Reg.t) =
    ins t "store ptr %a, ptr %a" pp_global sym (pp_reg_ident t) reg

  let ins_store_nativeint t n (reg : Reg.t) =
    ins t "store %a %s, ptr %a" pp_machtyp_component reg.typ
      (Nativeint.to_string n) (pp_reg_ident t) reg

  let ins_branch t label = ins t "br %a" (pp_label t) label

  (* [dbg] is used to print the [Reg.t] the allocated identifier corresponds to
     in the preamble *)
  let ins_alloca ?regname t (reg : Reg.t) =
    let pp_regname ppf () =
      match regname with
      | None -> ()
      | Some regname -> pp_dbg_comment ~newline:false ppf regname Debuginfo.none
    in
    ins t "%a = alloca %a %a" (pp_reg_ident t) reg pp_machtyp_component reg.typ
      pp_regname ()

  let ins_branch_cond t cond ifso ifnot =
    ins t "br i1 %a, %a, %a" pp_ident cond (pp_label t) ifso (pp_label t) ifnot

  let ins_branch_cond_ident t cond ifso ifnot =
    ins t "br i1 %a, label %a, label %a" pp_ident cond pp_ident ifso pp_ident
      ifnot

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

  let load_reg_to_temp t reg =
    let temp = fresh_ident t in
    ins_load_from_reg t temp reg;
    temp

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
      | Isigned Ceq | Iunsigned Ceq -> "eq"
      | Isigned Cne | Iunsigned Cne -> "ne"
      | Isigned Clt -> "slt"
      | Isigned Cgt -> "sgt"
      | Isigned Cle -> "sle"
      | Isigned Cge -> "sge"
      | Iunsigned Clt -> "ult"
      | Iunsigned Cgt -> "ugt"
      | Iunsigned Cle -> "ule"
      | Iunsigned Cge -> "ult"
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
    | Return ->
      let temp = load_reg_to_temp t i.arg.(0) in
      ins t "ret %a %a" pp_machtyp_component i.arg.(0).typ pp_ident temp
    | Int_test { lt; eq; gt; is_signed; imm } ->
      let make_comp comp =
        match is_signed with
        | true -> Operation.Isigned comp
        | false -> Operation.Iunsigned comp
      in
      let is_lt = int_comp t (make_comp Cmm.Clt) i ~imm in
      let ge = fresh_ident t in
      ins_branch_cond_ident t is_lt (get_ident_for_label t lt) ge;
      line t.ppf "%a:" Ident.print ge;
      let is_gt = int_comp t (make_comp Cmm.Cgt) i ~imm in
      ins_branch_cond t is_gt gt eq
    | Raise _ ->
      (* CR yusumez: Implement this *)
      ins t "call void @llvm.trap()";
      ins t "unreachable"
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
    | Switch _ | Tailcall_self _ | Tailcall_func _ | Call_no_return _ | Call _
    | Prim _ ->
      not_implemented_terminator i

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
      | Iclz _ | Ictz _ | Ipopcnt -> not_implemented_basic i
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
        let suffix = match width with Float32 -> "f32" | Float64 -> "f64" in
        (* CR yusumez: Do this properly when we have [ins_call] *)
        ins t "%a = call %a @llvm.fabs.%s(%a %a)" pp_ident res Llvm_typ.pp_t typ
          suffix Llvm_typ.pp_t typ pp_ident arg;
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
    | Const_int n -> ins_store_nativeint t n i.res.(0)
    | Const_symbol { sym_name; sym_global = _ } ->
      t.referenced_symbols <- String.Set.add sym_name t.referenced_symbols;
      ins_store_global t sym_name i.res.(0)
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
      | Single { reg = Float64 }
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
      let trunc_int dst_typ =
        let reg_typ = i.arg.(0).typ |> Llvm_typ.of_machtyp_component in
        let temp = fresh_ident t in
        let truncated = fresh_ident t in
        ins_load_from_reg t temp i.arg.(0);
        ins_conv t "trunc" ~src:temp ~dst:truncated ~src_typ:reg_typ ~dst_typ;
        ins_store t ~src:truncated ~dst dst_typ
      in
      match chunk with
      | Word_int | Word_val -> basic Llvm_typ.i64
      | Byte_unsigned | Byte_signed -> trunc_int Llvm_typ.i8
      | Sixteen_unsigned | Sixteen_signed -> trunc_int Llvm_typ.i16
      | Thirtytwo_signed | Thirtytwo_unsigned -> trunc_int Llvm_typ.i32
      | Single { reg = Float32 } -> basic Llvm_typ.float
      | Double -> basic Llvm_typ.double
      | Onetwentyeight_unaligned | Onetwentyeight_aligned
      | Twofiftysix_unaligned | Twofiftysix_aligned | Fivetwelve_unaligned
      | Fivetwelve_aligned
      | Single { reg = Float64 } ->
        not_implemented_basic ~msg:"store" i)
    | Intop op -> int_op t i op ~imm:None
    | Intop_imm (op, n) -> int_op t i op ~imm:(Some n)
    | Floatop (width, op) -> float_op t i width op
    | Spill | Reload | Const_float32 _ | Const_float _ | Const_vec128 _
    | Const_vec256 _ | Const_vec512 _ | Stackoffset _ | Intop_atomic _ | Csel _
    | Reinterpret_cast _ | Static_cast _ | Probe_is_enabled _ | Begin_region
    | End_region | Specific _ | Name_for_debugger _ | Dls_get | Poll | Pause
    | Alloc _ ->
      not_implemented_basic i

  let basic t (i : Cfg.basic Cfg.instruction) =
    pp_dbg_instr_basic t.ppf i;
    match i.desc with
    | Op op -> basic_op t i op
    | Prologue | Reloadretaddr -> ()
    | Poptrap _ | Pushtrap _ | Stack_check _ -> not_implemented_basic i

  (* == Cfg data items == *)

  (* CR yusumez: don't do these matches once we have the structured type for
     [data_item] *)
  let typ_of_data_item (d : Cmm.data_item) =
    match d with
    | Cdefine_symbol _ -> assert false (* cannot happen *)
    | Cint _ -> Llvm_typ.Int { width_in_bits = 64 }
    | Csymbol_address _ -> Llvm_typ.Ptr
    | Cint8 _ | Cint16 _ | Cint32 _ | Csingle _ | Cdouble _ | Cvec128 _
    | Cvec256 _ | Cvec512 _ | Csymbol_offset _ | Cstring _ | Cskip _ | Calign _
      ->
      Misc.fatal_error "Llvmize.typ_of_data_item: not implemented"

  let pp_const_data_item ppf (d : Cmm.data_item) =
    match d with
    | Cdefine_symbol _ -> assert false (* cannot happen *)
    | Cint n -> fprintf ppf "%s" (Nativeint.to_string n)
    | Csymbol_address { sym_name; sym_global = _ } -> pp_global ppf sym_name
    | Cint8 _ | Cint16 _ | Cint32 _ | Csingle _ | Cdouble _ | Cvec128 _
    | Cvec256 _ | Cvec512 _ | Csymbol_offset _ | Cstring _ | Cskip _ | Calign _
      ->
      Misc.fatal_error "Llvmize.pp_const_data_item: not implemented"

  let pp_typ_and_const ppf (d : Cmm.data_item) =
    fprintf ppf "%a %a" Llvm_typ.pp_t (typ_of_data_item d) pp_const_data_item d

  let data_decl t sym (ds : Cmm.data_item list) =
    line t.ppf "%a = global %a { %a }" pp_global sym Llvm_typ.pp_t
      (Llvm_typ.Struct (List.map typ_of_data_item ds))
      (pp_print_list ~pp_sep:pp_comma pp_typ_and_const)
      ds

  let data_decl_extern t sym =
    line t.ppf "%a = external global %a" pp_global sym Llvm_typ.pp_t
      Llvm_typ.ptr

  let symbol_decl t sym =
    line t.ppf "%a = global i64 0" pp_global (Cmm_helpers.make_symbol sym)

  let empty_fun_decl t sym =
    line t.ppf "define void %a() { ret void }" pp_global
      (Cmm_helpers.make_symbol sym)
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

(* Allocates every reg in [cfg] on the stack and fills up the [reg2ident] table
   in [t]. Note that args are assigned an identifier in the argument list but
   the body will use the alloca'd idents instead. *)
let alloca_regs t cfg old_arg_idents =
  let arg_regs = List.map fst old_arg_idents |> Reg.Set.of_list in
  let body_regs = collect_body_regs cfg in
  let temp_regs = Reg.Set.diff body_regs arg_regs in
  let alloca (reg : Reg.t) =
    F.ins_alloca ~regname:(Format.asprintf "%a" Printreg.reg reg) t reg
  in
  Reg.Set.iter alloca arg_regs;
  Reg.Set.iter alloca temp_regs;
  List.iter
    (fun (reg, old_ident) -> F.ins_store_into_reg t old_ident reg)
    old_arg_idents

let cfg (cl : CL.t) =
  let t = get_current_compilation_unit "cfg" in
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
        next_instruction_id = _;
        fun_ret_type
      } =
    cfg
  in
  t.defined_symbols <- String.Set.add fun_name t.defined_symbols;
  (* Make fresh idents for argument regs since these will be different from
     idents assigned to them later on *)
  let fun_args_with_idents =
    Array.to_list fun_args |> List.map (fun arg -> arg, fresh_ident t)
  in
  let pp_block label =
    let block = Label.Tbl.find blocks label in
    let preds = Cfg.predecessor_labels block in
    if Label.equal entry_label label && not (Misc.Stdlib.List.is_empty preds)
    then Misc.fatal_errorf "Llvmize: entry label must not have predecessors";
    F.block_label_with_predecessors t label preds;
    DLL.iter ~f:(F.basic t) block.body;
    F.terminator t block.terminator
  in
  let pp_body () =
    (* First unused numbered identifier after the argument list is reserved, so
       we cannot use it. We explicitly skip it here *)
    let (_ : Ident.t) = Ident.Gen.get_fresh t.current_fun_info.ident_gen in
    alloca_regs t cfg fun_args_with_idents;
    F.ins t "br %a" (F.pp_label t) entry_label;
    DLL.iter ~f:pp_block layout
  in
  let fun_attrs = fun_attrs t fun_codegen_options in
  F.define t ~fun_name ~fun_args:fun_args_with_idents ~fun_ret_type ~fun_dbg
    ~fun_attrs pp_body

(* CR yusumez: Implement this *)
let data ds =
  let t = get_current_compilation_unit "data" in
  t.data <- List.append t.data ds

(* Define menitoned but not declared data items as extern *)
let emit_data_extern t =
  List.iter
    (fun (d : Cmm.data_item) ->
      match d with
      | Cdefine_symbol { sym_name; sym_global = _ } ->
        (* [t.defined_symbols] now tracks all defined symbols *)
        t.defined_symbols <- String.Set.add sym_name t.defined_symbols
      | Csymbol_address { sym_name; sym_global = _ } ->
        t.referenced_symbols <- String.Set.add sym_name t.referenced_symbols
      | Cint _ | Cint8 _ | Cint16 _ | Cint32 _ | Csingle _ | Cdouble _
      | Cvec128 _ | Cvec256 _ | Cvec512 _ | Csymbol_offset _ | Cstring _
      | Cskip _ | Calign _ ->
        ())
    t.data;
  String.Set.diff t.referenced_symbols t.defined_symbols
  |> String.Set.iter (fun sym -> F.data_decl_extern t sym)

(* CR yusumez: We do this cumbersome list wrangling since we receive data
   declarations as a flat list. Ideally, [data_item]s would be represented in a
   more structured manner which we can directly pass on to [declare]. *)
let emit_data t =
  let declare = F.data_decl t in
  let fail msg =
    Misc.fatal_error ("Llvmize: data item not implemented: " ^ msg)
  in
  let cur_sym, ds =
    List.fold_left
      (fun (cur_sym, ds) (d : Cmm.data_item) ->
        match d with
        | Cdefine_symbol { sym_name; sym_global = _ } ->
          Option.iter (fun cur_sym -> declare cur_sym ds) cur_sym;
          Some sym_name, []
        | Cint _ | Csymbol_address _ -> cur_sym, ds @ [d]
        | Calign _ -> fail "align"
        | Cint8 _ | Cint16 _ | Cint32 _ -> fail "int"
        | Csingle _ | Cdouble _ -> fail "float"
        | Cvec128 _ | Cvec256 _ | Cvec512 _ -> fail "vec"
        | Csymbol_offset _ -> fail "symbol offset"
        | Cstring _ -> fail "string"
        | Cskip _ -> fail "skip")
      (None, []) t.data
  in
  Option.iter (fun cur_sym -> declare cur_sym ds) cur_sym;
  emit_data_extern t

let remove_file filename =
  try if Sys.file_exists filename then Sys.remove filename
  with Sys_error _msg -> ()

let llvmir_to_assembly t =
  (* CR-someday gyorsh: add other optimization flags and control which passes to
     perform. *)
  let cmd =
    match !Oxcaml_flags.llvm_path with Some path -> path | None -> Config.asm
  in
  match t.asm_filename with
  | None -> 0
  | Some asm_filename ->
    Ccomp.command
      (String.concat " "
         [ cmd;
           "-o";
           Filename.quote asm_filename;
           "-O3";
           "-S";
           "-x ir";
           Filename.quote t.llvmir_filename ])

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
  F.symbol_decl t "data_begin";
  F.empty_fun_decl t "code_begin";
  Format.pp_print_newline t.ppf ()

(* CR yusumez: [begin_assembly] and [end_assembly] emit extra things to the .ll
   file, so they always need to be called. However, this will still generate an
   assembly file if -stop-after simplify_cfg or -stop_after linearization are
   passed, which it shouldn't do. *)

let end_assembly () =
  let t = get_current_compilation_unit "end_asm" in
  (* Emit data declarations *)
  emit_data t;
  Format.pp_print_newline t.ppf ();
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
