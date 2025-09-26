(******************************************************************************
 *                                  OxCaml                                    *
 *                       Yusuf Onur Üşümez, Jane Street                       *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
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
[@@@ocaml.warning "+a-40-41-42"]

module String = Misc.Stdlib.String

module List = struct
  include List
  include Misc.Stdlib.List
end

(* Convenient ways to print - all nicely packed! *)
module Format = struct
  include Format

  let pp_indent ppf () = fprintf ppf "  "

  let pp_comma ppf () = fprintf ppf ", "

  let pp_space ppf () = fprintf ppf " "

  let pp_line ?(num_indents = 0) ?append ppf =
    (pp_print_list pp_indent) ppf (List.init num_indents (fun _ -> ()));
    kfprintf
      (fun ppf ->
        match append with
        | None -> fprintf ppf "\n"
        | Some comment -> fprintf ppf " %s\n" comment)
      ppf

  let pp_ins ppf = pp_line ~num_indents:1 ppf

  let pp_str_if str ppf b = if b then fprintf ppf "%s" str

  let string_of_pp pp_fun = asprintf "%a" (fun ppf () -> pp_fun ppf) ()

  (* Comments *)

  let do_if_comments_enabled f = if !Oxcaml_flags.dasm_comments then f ()

  let pp_comment ppf =
    kasprintf (fun s -> do_if_comments_enabled (fun () -> fprintf ppf "; %s" s))

  let pp_dbginfo ppf dbg =
    if Debuginfo.is_none dbg
    then ()
    else fprintf ppf "[ %a ]" Debuginfo.print_compact dbg

  let pp_dbg_comment ppf name dbg = pp_comment ppf "%s %a" name pp_dbginfo dbg

  let pp_dbg_instr_aux pp_instr ppf (ins : _ Cfg.instruction) =
    (* Replace newlines since this is a line comment *)
    let instr_str =
      asprintf "%a" pp_instr ins |> String.map (function '\n' -> ' ' | c -> c)
    in
    pp_dbg_comment ppf instr_str ins.dbg

  let pp_dbg_instr_basic = pp_dbg_instr_aux Cfg.print_basic

  let dbg_instr_basic_string i =
    string_of_pp (fun ppf -> pp_dbg_instr_basic ppf i)

  let pp_dbg_instr_terminator = pp_dbg_instr_aux Cfg.print_terminator

  let dbg_instr_terminator_string i =
    string_of_pp (fun ppf -> pp_dbg_instr_terminator ppf i)
end

(* Convenient ways to fail - all nicely spilled! *)

let fail name = Misc.fatal_errorf "Llvmize.%s" name

let fail_msg ?name fmt =
  let name = match name with Some name -> "." ^ name | None -> "" in
  Format.kasprintf (fun msg -> Misc.fatal_errorf "Llvmize%s: %s" name msg) fmt

let fail_if_not ?msg name cond =
  if not cond
  then match msg with None -> fail name | Some msg -> fail_msg ~name "%s" msg

module Type = struct
  type t =
    (* Integral types *)
    | Int of { width_in_bits : int }
    | Ptr of { addrspace : string option }
    (* Floating point types *)
    | Float (* 32-bit *)
    | Double (* 64 bit *)
    (* Aggregate types *)
    | Struct of t list
    | Array of
        { num_of_elems : int;
          elem_type : t
        }
    | Vector of
        { num_of_elems : int;
          elem_type : t
        }
    (* Non-first-class types *)
    | Label
    | Token
    | Metadata

  let i128 = Int { width_in_bits = 128 }

  let i64 = Int { width_in_bits = 64 }

  let i32 = Int { width_in_bits = 32 }

  let i16 = Int { width_in_bits = 16 }

  let i8 = Int { width_in_bits = 8 }

  let i1 = Int { width_in_bits = 1 }

  let float = Float

  let double = Double

  let ptr = Ptr { addrspace = None }

  let val_ptr = Ptr { addrspace = Some "1" }

  let doublex2 = Vector { num_of_elems = 2; elem_type = double }

  let label = Label

  let metadata = Metadata

  let _token = Token

  let of_machtype_component (c : Cmm.machtype_component) =
    match c with
    | Int -> i64
    | Val -> val_ptr
    | Addr ->
      val_ptr
      (* We interpret [Addr]s as [val_ptr]s to let the RS4GC pass in LLVM to
         handle derived pointers for us. *)
    | Float -> double
    | Float32 -> float
    | Vec128 | Vec256 | Vec512 | Valx2 ->
      fail_msg ~name:"Type.of_machtype_component" "not_implemented"

  let of_reg (reg : Reg.t) = of_machtype_component reg.typ

  let of_float_width (width : Cmm.float_width) =
    match width with Float64 -> Double | Float32 -> Float

  let rec pp_t ppf t =
    let open Format in
    match t with
    | Int { width_in_bits } -> fprintf ppf "i%d" width_in_bits
    | Float -> fprintf ppf "float"
    | Double -> fprintf ppf "double"
    | Ptr { addrspace } -> (
      fprintf ppf "ptr";
      match addrspace with
      | Some addrspace -> fprintf ppf " addrspace(%s)" addrspace
      | None -> ())
    | Struct typs ->
      fprintf ppf "{ %a }"
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_t)
        typs
    | Array { num_of_elems; elem_type } ->
      fprintf ppf "[ %d x %a ]" num_of_elems pp_t elem_type
    | Vector { num_of_elems; elem_type } ->
      fprintf ppf "< %d x %a >" num_of_elems pp_t elem_type
    | Label -> fprintf ppf "label"
    | Token -> fprintf ppf "token"
    | Metadata -> fprintf ppf "metadata"

  let to_string t = Format.asprintf "%a" pp_t t

  let rec equal t1 t2 =
    match t1, t2 with
    | Int { width_in_bits = x }, Int { width_in_bits = y } -> x = y
    | Ptr { addrspace = x }, Ptr { addrspace = y } ->
      Option.equal String.equal x y
    | Float, Float | Double, Double -> true
    | Struct xs, Struct ys -> List.equal equal xs ys
    | ( Array { num_of_elems = size1; elem_type = typ1 },
        Array { num_of_elems = size2; elem_type = typ2 } ) ->
      size1 = size2 && equal typ1 typ2
    | ( Vector { num_of_elems = size1; elem_type = typ1 },
        Vector { num_of_elems = size2; elem_type = typ2 } ) ->
      size1 = size2 && equal typ1 typ2
    | Label, Label | Token, Token | Metadata, Metadata -> true
    | ( ( Int _ | Float | Double | Ptr _ | Struct _ | Array _ | Vector _ | Label
        | Token | Metadata ),
        _ ) ->
      false

  let get_struct_elements = function
    | Struct elems -> Some elems
    | Int _ | Ptr _ | Float | Double | Array _ | Vector _ | Label | Token
    | Metadata ->
      None

  let rec extract_struct t indices =
    match indices with
    | [] -> Some t
    | i :: rest -> (
      match get_struct_elements t with
      | Some elems ->
        Option.bind (List.nth_opt elems i) (fun elem ->
            extract_struct elem rest)
      | None -> None)

  let elem_type = function
    | Array { elem_type; _ } | Vector { elem_type; _ } -> Some elem_type
    | Int _ | Ptr _ | Float | Double | Struct _ | Label | Token | Metadata ->
      None

  let is_ptr = function
    | Ptr _ -> true
    | Int _ | Float | Double | Struct _ | Array _ | Vector _ | Label | Token
    | Metadata ->
      false

  let is_int = function
    | Int _ -> true
    | Ptr _ | Float | Double | Struct _ | Array _ | Vector _ | Label | Token
    | Metadata ->
      false

  let is_floating_point = function
    | Float | Double -> true
    | Ptr _ | Int _ | Struct _ | Array _ | Vector _ | Label | Token | Metadata
      ->
      false

  module Or_void = struct
    type nonrec t = t option

    let void = None

    let pp_t ppf t =
      match t with Some t -> pp_t ppf t | None -> Format.fprintf ppf "void"

    let equal = Option.equal equal
  end
end

module Ident = struct
  type t =
    | Local of string
    | Global of string

  let local s = Local s

  let global s = Global s

  let of_label label = local ("L" ^ Label.to_string label)

  let pp_t ppf t =
    let open Format in
    match t with
    | Local s -> fprintf ppf "%%%s" s
    | Global s ->
      let encoded = Asm_targets.(Asm_symbol.create s |> Asm_symbol.encode) in
      fprintf ppf "@%s" encoded

  let to_label_string_exn = function
    | Local s -> s ^ ":"
    | Global _ -> fail "Type.to_label_string_exn"

  let to_string_hum t = match t with Local s -> s | Global s -> s

  let to_string_encoded = function
    | Local s -> s
    | Global s -> Asm_targets.(Asm_symbol.create s |> Asm_symbol.encode)

  module Gen = struct
    type ident = t

    type t = { mutable next : int }

    (* Local identifiers are only valid within function scope, so we can reset
       it to 0 every time *)
    let create () = { next = 0 }

    let get_fresh t =
      let res = t.next in
      t.next <- succ res;
      local (Int.to_string res)
  end
end

module Value = struct
  (* CR yusumez: Split [Immediate] to multiple variants like [Poison],
     [Zeroinitializer], [Int], etc. *)

  type contents =
    | Ident of Ident.t
    | Immediate of string
    | Struct_constant of t list
    | Blockaddress of
        { func : Ident.t;
          block : Ident.t
        }

  and t = Type.t * contents

  let get_type (typ, _) = typ

  let get_ident_exn (_, contents) =
    match contents with
    | Ident ident -> ident
    | Immediate _ | Blockaddress _ | Struct_constant _ ->
      fail "Value.get_ident_exn"

  let get_contents (_, contents) = contents

  let of_ident ~typ ident = typ, Ident ident

  let of_symbol ?(typ = Type.ptr) symbol = typ, Ident (Ident.global symbol)

  let of_int ?(typ = Type.i64) i = typ, Immediate (Int.to_string i)

  let of_nativeint ?(typ = Type.i64) i = typ, Immediate (Nativeint.to_string i)

  let of_float32_bits bits =
    (* Note that "%#x" formats 0 as "0", not "0x0"... *)
    Type.float, Immediate (Format.sprintf "0x%lx" bits)

  let of_float64_bits bits =
    Type.double, Immediate (Format.sprintf "0x%Lx" bits)

  (* CR yusumez: 64-bit floats with at least 17 digits are guaranteed to round
     trip exactly through string conversions by the IEEE 754 standard (9 digits
     for 32-bit floats). However, it would still be nice to prefer the functions
     above whenever possible. *)
  let of_float ~typ f = typ, Immediate (Format.sprintf "%.20f" f)

  let of_label label = of_ident ~typ:Type.label (Ident.of_label label)

  let of_string_constant s =
    let open Format in
    ( Type.Array { num_of_elems = String.length s; elem_type = Type.i8 },
      Immediate
        (asprintf "%a"
           (fun ppf () ->
             fprintf ppf "c\"";
             String.iter (fun c -> fprintf ppf "\\%02x" (Char.code c)) s;
             fprintf ppf "\"")
           ()) )

  let poison typ = typ, Immediate "poison"

  let zeroinitializer typ = typ, Immediate "zeroinitializer"

  let imm typ s = typ, Immediate s

  let struct_constant ts =
    Type.Struct (List.map get_type ts), Struct_constant ts

  let blockaddress ~func ~block = Type.ptr, Blockaddress { func; block }

  let rec pp_contents ppf contents =
    let open Format in
    match contents with
    | Ident ident -> Ident.pp_t ppf ident
    | Immediate s -> fprintf ppf "%s" s
    | Struct_constant ts ->
      fprintf ppf "{ ";
      let pp_elem ppf t =
        match get_contents t with
        | Ident _ | Immediate _ | Blockaddress _ -> pp_t ppf t
        | Struct_constant _ ->
          fail_msg ~name:"Value.pp_value" "struct constants cannot nest"
      in
      (pp_print_list ~pp_sep:pp_comma pp_elem) ppf ts;
      fprintf ppf " }"
    | Blockaddress { func; block } ->
      fprintf ppf "blockaddress(%a, %a)" Ident.pp_t func Ident.pp_t block

  and pp_t ppf (typ, contents) =
    Format.fprintf ppf "%a %a" Type.pp_t typ pp_contents contents
end

module Fn_attr = struct
  type t =
    | Cold
    | Gc of string
    | Gc_leaf_function
    | Noinline
    | Returns_twice
    | Statepoint_id of int

  let to_string = function
    | Cold -> "cold"
    | Gc s -> Format.sprintf {|gc "%s"|} s
    | Gc_leaf_function -> {|"gc-leaf-function"="true"|}
    | Noinline -> "noinline"
    | Returns_twice -> "returns_twice"
    | Statepoint_id i -> Format.sprintf {|"statepoint-id"="%d"|} i

  let pp_t ppf t = Format.pp_print_string ppf (to_string t)

  let pp_t_list = Format.pp_print_list ~pp_sep:Format.pp_space pp_t

  let to_string t = Format.asprintf "%a" pp_t t

  let order = function
    | Cold | Gc_leaf_function | Noinline | Returns_twice | Statepoint_id _ -> 0
    | Gc _ -> 10
  (* [Gc] is not really an attribute, so it must occur after all attributes. It
     is included here because it basically behaves like one. *)

  let compare t1 t2 =
    let order_compare = Int.compare (order t1) (order t2) in
    if order_compare <> 0
    then order_compare
    else String.compare (to_string t1) (to_string t2)
end

module Calling_conventions = struct
  type t =
    | Default (* Default C calling convention *)
    | Oxcaml (* See backend/<arch>/proc.ml for details *)
    | Oxcaml_c_call
      (* Same as [Default] but threads runtime registers through and passes the
         function address through RAX. Used for [caml_c_call] *)
    | Oxcaml_c_call_stack_args
      (* Passes the number of bytes on the stack to be transferred in R12 in
         addition to [Ocaml_c_call]. Used for
         [caml_c_call_stack_args_llvm_backend] *)
    | Oxcaml_alloc
  (* Saves almost all registers (see [Proc.destroyed_at_alloc_or_poll]),
     otherwise behaves like [Ocaml]. Used for [caml_call_gc] and
     [caml_call_local_realloc] *)

  let to_string = function
    | Default -> ""
    | Oxcaml ->
      if Config.with_frame_pointers then "oxcaml_fpcc" else "oxcaml_nofpcc"
    | Oxcaml_c_call -> "oxcaml_ccc"
    | Oxcaml_c_call_stack_args -> "oxcaml_c_stackcc"
    | Oxcaml_alloc -> "oxcaml_alloccc"

  let pp_t ppf t = Format.pp_print_string ppf (to_string t)
end

module Instruction = struct
  type switch_branch =
    { index : Value.t;
      label : Value.t
    }

  type unary_op = Fneg

  type binary_op =
    (* Integer *)
    | Add
    | Sub
    | Mul
    | Udiv
    | Sdiv
    | Urem
    | Srem
    (* Float *)
    | Fadd
    | Fsub
    | Fmul
    | Fdiv
    | Frem
    (* Bitwise *)
    | Shl
    | Lshr
    | Ashr
    | And
    | Or
    | Xor

  type atomicrmw_op =
    | Atomicrmw_add
    | Atomicrmw_sub
    | Atomicrmw_and
    | Atomicrmw_or
    | Atomicrmw_xor
    | Atomicrmw_xchg

  type convert_op =
    (* Int *)
    | Sext
    | Zext
    | Trunc
    (* Float *)
    | Fpext
    | Fptrunc
    (* Int <-> Float *)
    | Fptoui
    | Fptosi
    | Uitofp
    | Sitofp
    (* Ptr *)
    | Inttoptr
    | Ptrtoint
    | Ptrtoaddr
    | Addrspacecast
    (* Magic *)
    | Bitcast

  type icmp_cond =
    | Ieq
    | Ine
    | Iugt
    | Iuge
    | Iult
    | Iule
    | Isgt
    | Isge
    | Islt
    | Isle

  type fcmp_cond =
    | Ffalse
    | Foeq
    | Fogt
    | Foge
    | Folt
    | Fole
    | Fone
    | Ford
    | Fueq
    | Fugt
    | Fuge
    | Fult
    | Fule
    | Fune
    | Funo
    | Ftrue

  let binary_op_to_string = function
    | Add -> "add"
    | Sub -> "sub"
    | Mul -> "mul"
    | Udiv -> "udiv"
    | Sdiv -> "sdiv"
    | Urem -> "urem"
    | Srem -> "srem"
    | Fadd -> "fadd"
    | Fsub -> "fsub"
    | Fmul -> "fmul"
    | Fdiv -> "fdiv"
    | Frem -> "frem"
    | Shl -> "shl"
    | Lshr -> "lshr"
    | Ashr -> "ashr"
    | And -> "and"
    | Or -> "or"
    | Xor -> "xor"

  let unary_op_to_string = function Fneg -> "fneg"

  let convert_op_to_string = function
    | Sext -> "sext"
    | Zext -> "zext"
    | Trunc -> "trunc"
    | Fpext -> "fpext"
    | Fptrunc -> "fptrunc"
    | Fptoui -> "fptoui"
    | Fptosi -> "fptosi"
    | Uitofp -> "uitofp"
    | Sitofp -> "sitofp"
    | Inttoptr -> "inttoptr"
    | Ptrtoint -> "ptrtoint"
    | Ptrtoaddr -> "ptrtoaddr"
    | Addrspacecast -> "addrspacecast"
    | Bitcast -> "bitcast"

  let icmp_cond_to_string = function
    | Ieq -> "eq"
    | Ine -> "ne"
    | Iugt -> "ugt"
    | Iuge -> "uge"
    | Iult -> "ult"
    | Iule -> "ule"
    | Isgt -> "sgt"
    | Isge -> "sge"
    | Islt -> "slt"
    | Isle -> "sle"

  let icmp_cond_of_ocaml (cond : Operation.integer_comparison) =
    match cond with
    | Ceq -> Ieq
    | Cne -> Ine
    | Clt -> Islt
    | Cgt -> Isgt
    | Cle -> Isle
    | Cge -> Isge
    | Cult -> Iult
    | Cugt -> Iugt
    | Cule -> Iule
    | Cuge -> Iuge

  let fcmp_cond_to_string = function
    | Ffalse -> "false"
    | Foeq -> "oeq"
    | Fogt -> "ogt"
    | Foge -> "oge"
    | Folt -> "olt"
    | Fole -> "ole"
    | Fone -> "one"
    | Ford -> "ord"
    | Fueq -> "ueq"
    | Fugt -> "ugt"
    | Fuge -> "uge"
    | Fult -> "ult"
    | Fule -> "ule"
    | Fune -> "une"
    | Funo -> "uno"
    | Ftrue -> "true"

  let fcmp_cond_of_ocaml (cond : Operation.float_comparison) =
    match cond with
    | CFeq -> Foeq
    | CFneq -> Fune
    | CFlt -> Folt
    | CFnlt -> Fuge
    | CFgt -> Fogt
    | CFngt -> Fule
    | CFle -> Fole
    | CFnle -> Fugt
    | CFge -> Foge
    | CFnge -> Fult

  let atomicrmw_op_to_string = function
    | Atomicrmw_add -> "add"
    | Atomicrmw_sub -> "sub"
    | Atomicrmw_and -> "and"
    | Atomicrmw_or -> "or"
    | Atomicrmw_xor -> "xor"
    | Atomicrmw_xchg -> "xchg"

  type op =
    (* Terminator *)
    | Ret of Value.t
    | Br of Value.t
    | Br_cond of
        { cond : Value.t;
          ifso : Value.t;
          ifnot : Value.t
        }
    | Switch of
        { discr : Value.t;
          default : Value.t;
          branches : switch_branch list
        }
    | Unreachable
    (* Basic *)
    | Unary of
        { op : unary_op;
          arg : Value.t
        }
    | Binary of
        { op : binary_op;
          arg1 : Value.t;
          arg2 : Value.t
        }
    | Convert of
        { op : convert_op;
          arg : Value.t;
          to_ : Type.t
        }
    | Icmp of
        { cond : icmp_cond;
          arg1 : Value.t;
          arg2 : Value.t
        }
    | Fcmp of
        { cond : fcmp_cond;
          arg1 : Value.t;
          arg2 : Value.t
        }
    (* Vector ops *)
    | Extractelement of
        { vector : Value.t;
          index : Value.t
        }
    | Insertelement of
        { vector : Value.t;
          index : Value.t;
          to_insert : Value.t
        }
    (* Struct/array ops *)
    | Extractvalue of
        { aggregate : Value.t;
          indices : int list
        }
    | Insertvalue of
        { aggregate : Value.t;
          indices : int list;
          to_insert : Value.t
        }
    (* Memory *)
    | Alloca of
        { typ : Type.t;
          count : Value.t option
        }
    | Load of
        { ptr : Value.t;
          typ : Type.t
        }
    | Store of
        { ptr : Value.t;
          to_store : Value.t
        }
    | Getelementptr of
        { base_type : Type.t;
          base_ptr : Value.t;
          indices : Value.t list
        }
    (* Atomics *)
    (* CR yusumez: Implement ordering constraints instead of hard-coding them *)
    | Cmpxchg of
        { ptr : Value.t;
          compare_with : Value.t;
          set_if_equal : Value.t
        }
    | Atomicrmw of
        { op : atomicrmw_op;
          ptr : Value.t;
          arg : Value.t
        }
    (* Control flow *)
    | Select of
        { cond : Value.t;
          ifso : Value.t;
          ifnot : Value.t
        }
    | Call of
        { func : Ident.t;
          args : Value.t list;
          res_type : Type.Or_void.t;
          attrs : Fn_attr.t list;
          cc : Calling_conventions.t;
          musttail : bool
        }
    | Inline_asm of
        { asm : string;
          constraints : string;
          args : Value.t list;
          res_type : Type.Or_void.t;
          sideeffect : bool;
          attrs : Fn_attr.t list
        }

  type t =
    { op : op;
      res : Ident.t option
    }

  let assert' name cond = fail_if_not ("Instruction." ^ name) cond

  (* [None] when the operation doesn't have a result *)
  let op_res_type = function
    (* Terminators return no value *)
    | Ret _ | Br _ | Br_cond _ | Switch _ | Unreachable -> None
    (* Basic operations *)
    | Unary { arg; _ } -> Some (Value.get_type arg)
    | Binary { arg1; _ } -> Some (Value.get_type arg1)
    | Convert { to_; _ } -> Some to_
    | Icmp _ | Fcmp _ -> Some Type.i1
    (* Vector operations *)
    | Extractelement { vector; _ } -> Type.elem_type (Value.get_type vector)
    | Insertelement { vector; _ } -> Some (Value.get_type vector)
    (* Aggregate operations *)
    | Extractvalue { aggregate; indices } ->
      let typ = Type.extract_struct (Value.get_type aggregate) indices in
      assert' "op_res_type" (Option.is_some typ);
      typ
    | Insertvalue { aggregate; _ } -> Some (Value.get_type aggregate)
    (* Memory operations *)
    | Alloca _typ -> Some Type.ptr
    | Load { typ; _ } -> Some typ
    | Store _ -> None
    | Getelementptr { base_ptr; _ } -> Some (Value.get_type base_ptr)
    (* Atomics *)
    | Cmpxchg { compare_with; _ } ->
      let elem_type = Value.get_type compare_with in
      Some (Type.Struct [elem_type; Type.i1])
    | Atomicrmw { arg; _ } -> Some (Value.get_type arg)
    (* Control flow *)
    | Select { ifso; _ } -> Some (Value.get_type ifso)
    | Call { res_type; _ } -> res_type
    | Inline_asm { res_type; _ } -> res_type

  let with_res op ident =
    assert' "with_res" (Option.is_some (op_res_type op));
    { op; res = Some ident }

  let without_res op =
    assert' "without_res" (Option.is_none (op_res_type op));
    { op; res = None }

  let get_res_value { op; res } =
    match res with
    | Some res ->
      Option.map (fun typ -> Value.of_ident ~typ res) (op_res_type op)
    | None -> None

  (* CR yusumez: add checks as necessary *)

  let ret v = Ret v

  let br v =
    assert' "br" (Value.get_type v |> Type.(equal label));
    Br v

  let br_cond ~cond ~ifso ~ifnot =
    assert' "br_cond" (Value.get_type cond |> Type.(equal i1));
    assert' "br_cond" (Value.get_type ifso |> Type.(equal label));
    assert' "br_cond" (Value.get_type ifnot |> Type.(equal label));
    Br_cond { cond; ifso; ifnot }

  let switch ~discr ~default ~branches =
    assert' "switch" (Value.get_type discr |> Type.is_int);
    assert' "switch" (Value.get_type default |> Type.(equal label));
    assert' "switch"
      (List.for_all
         (fun { index; label } ->
           Value.get_type index |> Type.is_int
           && Value.get_type label |> Type.equal Type.label)
         branches);
    Switch { discr; default; branches }

  let unreachable = Unreachable

  let unary op ~arg = Unary { op; arg }

  let binary op ~arg1 ~arg2 = Binary { op; arg1; arg2 }

  let convert op ~arg ~to_ = Convert { op; arg; to_ }

  let icmp cond ~arg1 ~arg2 =
    let arg1_type = Value.get_type arg1 in
    let arg2_type = Value.get_type arg2 in
    assert' "icmp" (Type.equal arg1_type arg2_type);
    assert' "icmp" (Type.is_int arg1_type);
    Icmp { cond; arg1; arg2 }

  let fcmp cond ~arg1 ~arg2 =
    let arg1_type = Value.get_type arg1 in
    let arg2_type = Value.get_type arg2 in
    assert' "fcmp" (Type.equal arg1_type arg2_type);
    assert' "fcmp" (Type.is_floating_point arg1_type);
    Fcmp { cond; arg1; arg2 }

  let extractelement ~vector ~index =
    assert' "extractelement" (Value.get_type index |> Type.is_int);
    Extractelement { vector; index }

  let insertelement ~vector ~index ~to_insert =
    assert' "insertelement" (Value.get_type index |> Type.is_int);
    Insertelement { vector; index; to_insert }

  let extractvalue ~aggregate ~indices = Extractvalue { aggregate; indices }

  let insertvalue ~aggregate ~indices ~to_insert =
    Insertvalue { aggregate; indices; to_insert }

  let alloca ?count typ = Alloca { typ; count }

  let load ~ptr ~typ =
    assert' "load" (Value.get_type ptr |> Type.is_ptr);
    Load { ptr; typ }

  let store ~ptr ~to_store =
    assert' "store" (Value.get_type ptr |> Type.is_ptr);
    Store { ptr; to_store }

  let getelementptr ~base_type ~base_ptr ~indices =
    assert' "getelementptr" (Value.get_type base_ptr |> Type.is_ptr);
    assert' "getelementptr"
      (List.for_all (fun index -> Value.get_type index |> Type.is_int) indices);
    Getelementptr { base_type; base_ptr; indices }

  let cmpxchg ~ptr ~compare_with ~set_if_equal =
    assert' "cmpxchg" (Value.get_type ptr |> Type.is_ptr);
    let compare_type = Value.get_type compare_with in
    let set_type = Value.get_type set_if_equal in
    assert' "cmpxchg" (Type.equal compare_type set_type);
    Cmpxchg { ptr; compare_with; set_if_equal }

  let atomicrmw op ~ptr ~arg =
    assert' "atomicrmw" (Value.get_type ptr |> Type.is_ptr);
    Atomicrmw { op; ptr; arg }

  let select ~cond ~ifso ~ifnot =
    assert' "select" (Value.get_type cond |> Type.(equal i1));
    let ifso_type = Value.get_type ifso in
    let ifnot_type = Value.get_type ifnot in
    assert' "select" (Type.equal ifso_type ifnot_type);
    Select { cond; ifso; ifnot }

  let call ~func ~args ~res_type ~attrs ~cc ~musttail =
    (* Statepoint insertion breaks musttail checks. We can't mark them as GC
       leaves here, as LLVM might inline them to a position where they aren't
       tail calls anymore and we'd need a statepoint there. So, we make LLVM
       skip `musttail` calls instead. *)
    Call { func; args; res_type; attrs; cc; musttail }

  let inline_asm ~args ~res_type ~asm ~constraints ~sideeffect =
    (* Similarly, it makes no sense to put statepoints for inline asm. *)
    Inline_asm
      { args;
        res_type;
        asm;
        constraints;
        sideeffect;
        attrs = [Fn_attr.Gc_leaf_function]
      }

  (* Note: this function handles indentation and newlines itself *)
  let pp_t ?comment ppf { op; res } =
    let open Format in
    let append =
      match comment with
      | Some str -> Some (string_of_pp (fun ppf -> pp_comment ppf "%s" str))
      | None -> None
    in
    let ins ?(num_indents = 1) fmt = pp_line ~num_indents ?append ppf fmt in
    let ins_res ?(num_indents = 1) fmt =
      pp_line ~num_indents ?append ppf ("%a = " ^^ fmt) Ident.pp_t
        (Option.get res)
    in
    match op with
    | Ret v -> ins "ret %a" Value.pp_t v
    | Br v -> ins "br %a" Value.pp_t v
    | Br_cond { cond; ifso; ifnot } ->
      ins "br %a, %a, %a" Value.pp_t cond Value.pp_t ifso Value.pp_t ifnot
    | Switch { discr; default; branches } ->
      ins "switch %a, %a [" Value.pp_t discr Value.pp_t default;
      List.iter
        (fun { index; label } ->
          ins ~num_indents:2 "%a, %a" Value.pp_t index Value.pp_t label)
        branches;
      ins "]"
    | Unreachable -> ins "unreachable"
    | Unary { op; arg } ->
      ins_res "%s %a" (unary_op_to_string op) Value.pp_t arg
    | Binary { op; arg1; arg2 } ->
      ins_res "%s %a, %a" (binary_op_to_string op) Value.pp_t arg1
        Value.pp_contents (Value.get_contents arg2)
    | Convert { op; arg; to_ } ->
      ins_res "%s %a to %a" (convert_op_to_string op) Value.pp_t arg Type.pp_t
        to_
    | Icmp { cond; arg1; arg2 } ->
      ins_res "icmp %s %a, %a" (icmp_cond_to_string cond) Value.pp_t arg1
        Value.pp_contents (Value.get_contents arg2)
    | Fcmp { cond; arg1; arg2 } ->
      ins_res "fcmp %s %a, %a" (fcmp_cond_to_string cond) Value.pp_t arg1
        Value.pp_contents (Value.get_contents arg2)
    | Extractelement { vector; index } ->
      ins_res "extractelement %a, %a" Value.pp_t vector Value.pp_t index
    | Insertelement { vector; index; to_insert } ->
      ins_res "insertelement %a, %a, %a" Value.pp_t vector Value.pp_t to_insert
        Value.pp_t index
    | Extractvalue { aggregate; indices } ->
      ins_res "extractvalue %a, %a" Value.pp_t aggregate
        (pp_print_list ~pp_sep:pp_comma pp_print_int)
        indices
    | Insertvalue { aggregate; indices; to_insert } ->
      ins_res "insertvalue %a, %a, %a" Value.pp_t aggregate Value.pp_t to_insert
        (pp_print_list ~pp_sep:pp_comma pp_print_int)
        indices
    | Alloca { typ; count } ->
      let pp_count ppf () =
        match count with
        | Some count -> fprintf ppf ", %a" Value.pp_t count
        | None -> ()
      in
      ins_res "alloca %a%a" Type.pp_t typ pp_count ()
    | Load { ptr; typ } -> ins_res "load %a, %a" Type.pp_t typ Value.pp_t ptr
    | Store { ptr; to_store } ->
      ins "store %a, %a" Value.pp_t to_store Value.pp_t ptr
    | Getelementptr { base_type; base_ptr; indices } ->
      ins_res "getelementptr %a, %a, %a" Type.pp_t base_type Value.pp_t base_ptr
        (pp_print_list ~pp_sep:pp_comma Value.pp_t)
        indices
    | Cmpxchg { ptr; compare_with; set_if_equal } ->
      ins_res "cmpxchg %a, %a, %a acq_rel monotonic" Value.pp_t ptr Value.pp_t
        compare_with Value.pp_t set_if_equal
    | Atomicrmw { op; ptr; arg } ->
      ins_res "atomicrmw %s %a, %a acq_rel"
        (atomicrmw_op_to_string op)
        Value.pp_t ptr Value.pp_t arg
    | Select { cond; ifso; ifnot } ->
      ins_res "select %a, %a, %a" Value.pp_t cond Value.pp_t ifso Value.pp_t
        ifnot
    | Call { func; args; res_type; attrs; cc; musttail } -> (
      let pp_call ppf () =
        fprintf ppf "%acall %a %a %a(%a) %a" (pp_str_if "musttail ") musttail
          Calling_conventions.pp_t cc Type.Or_void.pp_t res_type Ident.pp_t func
          (pp_print_list ~pp_sep:pp_comma Value.pp_t)
          args Fn_attr.pp_t_list attrs
      in
      match res with
      | Some _ -> ins_res "%a" pp_call ()
      | None -> ins "%a" pp_call ())
    | Inline_asm { args; res_type; asm; constraints; sideeffect; attrs } -> (
      let pp_call ppf () =
        fprintf ppf {|call %a asm %a "%s", "%s"(%a) %a|} Type.Or_void.pp_t
          res_type (pp_str_if "sideeffect") sideeffect asm constraints
          (pp_print_list ~pp_sep:pp_comma Value.pp_t)
          args Fn_attr.pp_t_list attrs
      in
      match res with
      | Some _ -> ins_res "%a" pp_call ()
      | None -> ins "%a" pp_call ())
end

module Function = struct
  (* CR yusumez: Consider a more structured representation *)
  type slot =
    | Comment of string
    | Label_def of Ident.t
    | Instruction of
        { instr : Instruction.t;
          comment : string option
        }

  type t =
    { name : Ident.t;
      args : (Type.t * Ident.t) list;
          (* Not using [Value.t] to have explicit identifiers *)
      res : Type.Or_void.t;
      cc : Calling_conventions.t;
      attrs : Fn_attr.t list;
      private_ : bool;
      dbg : Debuginfo.t;
      mutable body_rev : slot list
    }

  let add_instruction ?comment t instr =
    t.body_rev <- Instruction { instr; comment } :: t.body_rev

  let add_label_def t label = t.body_rev <- Label_def label :: t.body_rev

  let add_comment t comment = t.body_rev <- Comment comment :: t.body_rev

  let pp_t ppf { name; args; res; cc; attrs; private_; dbg; body_rev } =
    let open Format in
    (* Definition line *)
    do_if_comments_enabled (fun () ->
        pp_dbg_comment ppf (Ident.to_string_hum name) dbg;
        pp_line ppf "");
    let pp_private ppf () = if private_ then fprintf ppf "private" in
    let pp_args =
      pp_print_list ~pp_sep:pp_comma (fun ppf (typ, ident) ->
          fprintf ppf "%a %a" Type.pp_t typ Ident.pp_t ident)
    in
    pp_line ppf "define %a %a %a %a(%a) %a {" pp_private ()
      Calling_conventions.pp_t cc Type.Or_void.pp_t res Ident.pp_t name pp_args
      args Fn_attr.pp_t_list attrs;
    (* Body *)
    let body = List.rev body_rev in
    List.iter
      (function
        | Instruction { instr; comment } -> Instruction.pp_t ?comment ppf instr
        | Comment s -> do_if_comments_enabled (fun () -> pp_ins ppf "%s" s)
        | Label_def ident -> pp_line ppf "%s" (Ident.to_label_string_exn ident))
      body;
    pp_line ppf "}";
    pp_line ppf ""

  (* This module handles the creation of [Function.t]s *)
  module Emitter = struct
    type funcdef = t

    type t =
      { ident_gen : Ident.Gen.t;
        funcdef : funcdef
      }

    let create ~name ~args ~res ~cc ~attrs ~dbg ~private_ =
      let ident_gen = Ident.Gen.create () in
      let name = Ident.global name in
      let args =
        List.map (fun typ -> typ, Ident.Gen.get_fresh ident_gen) args
      in
      (* First unnamed local identifier after parameters is reserved for the
         entry label. *)
      Ident.Gen.get_fresh ident_gen |> ignore;
      let funcdef =
        { name; args; res; cc; attrs; private_; dbg; body_rev = [] }
      in
      { ident_gen; funcdef }

    let get_fun t = t.funcdef

    let get_args_as_values t =
      List.map (fun (typ, ident) -> Value.of_ident ~typ ident) t.funcdef.args

    let get_res_type t = t.funcdef.res

    let get_fun_ident t = t.funcdef.name

    let ins ?comment ?res_ident t op =
      let res_ident =
        match res_ident with
        | Some ident -> ident
        | None -> Ident.Gen.get_fresh t.ident_gen
      in
      let instr = Instruction.with_res op res_ident in
      add_instruction ?comment t.funcdef instr;
      Instruction.get_res_value instr |> Option.get

    let ins_no_res ?comment t op =
      let instr = Instruction.without_res op in
      add_instruction ?comment t.funcdef instr

    let comment t s = add_comment t.funcdef s

    let label_def t ident = add_label_def t.funcdef ident
  end
end

module Fundecl = struct
  (* CR yusumez: add cc, attrs, etc. as needed *)
  type t =
    { name : string;
      args : Type.t list; (* Not using [Value.t] to have explicit identifiers *)
      res : Type.Or_void.t
    }

  let create name args res = { name; args; res }

  let pp_t ppf { name; args; res } =
    let open Format in
    let ident = Ident.global name in
    let pp_args = pp_print_list ~pp_sep:pp_comma Type.pp_t in
    pp_line ppf "declare %a %a(%a)" Type.Or_void.pp_t res Ident.pp_t ident
      pp_args args

  let equal { name; args; res } { name = name'; args = args'; res = res' } =
    String.equal name name'
    && List.equal Type.equal args args'
    && Type.Or_void.equal res res'
end

module Data = struct
  type t =
    | Constant of
        { name : string;
          value : Value.t; (* CR yusumez: check this is actually a constant? *)
          section : string option;
          align : int option;
          private_ : bool
        }
    | External of string

  let constant ?(section = Some ".data") ?(align = Some Arch.size_addr)
      ?(private_ = false) name value =
    Constant { name; value; section; align; private_ }

  let external_ name = External name

  let pp_t ppf = function
    | Constant { name; value; section; align; private_ = _ } ->
      (* CR yusumez: If private global variables are not referenced anywhere
         directly, LLVM will delete them in the globaldce (dead global
         elimination) pass, so we don't mark anything as private for now. *)
      let open Format in
      let pp_section =
        pp_print_option (fun ppf section ->
            fprintf ppf ", section \"%s\"" section)
      in
      let pp_align =
        pp_print_option (fun ppf align -> fprintf ppf ", align %d" align)
      in
      let ident = Ident.global name in
      pp_line ppf "%a = global %a%a%a" Ident.pp_t ident Value.pp_t value
        pp_section section pp_align align
    | External name ->
      (* CR yusumez: We don't need that ptr there... *)
      Format.pp_line ppf "%a = external global ptr" Ident.pp_t
        (Ident.global name)
end
