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

(* CR yusumez: Port documentation here from the .ml file. *)

(** Types in LLVM IR. These include both first-class types (like int or ptr) and
    non-first-class types (like labels or metadata) *)
module Type : sig
  type t =
    | Int of { width_in_bits : int }
    | Ptr of { addrspace : string option }
    | Float
    | Double
    | Struct of t list
    | Array of
        { num_of_elems : int;
          elem_type : t
        }
    | Vector of
        { num_of_elems : int;
          elem_type : t
        }
    | Label
    | Token
    | Metadata

  val i128 : t

  val i64 : t

  val i32 : t

  val i16 : t

  val i8 : t

  val i1 : t

  val float : t

  val double : t

  val ptr : t

  val val_ptr : t

  val doublex2 : t

  val label : t

  val metadata : t

  val _token : t

  val of_machtype_component : Cmm.machtype_component -> t

  val of_reg : Reg.t -> t

  val of_float_width : Cmm.float_width -> t

  val pp_t : Format.formatter -> t -> unit

  val to_string : t -> string

  val equal : t -> t -> bool

  val get_struct_elements : t -> t list option

  val extract_struct : t -> int list -> t option

  val elem_type : t -> t option

  val is_ptr : t -> bool

  val is_int : t -> bool

  val is_floating_point : t -> bool

  module Or_void : sig
    type nonrec t = t option

    val void : 'a option

    val pp_t : Format.formatter -> t -> unit

    val equal : t -> t -> bool
  end
end

(** Identifiers in LLVM IR. Represents both global and local identifiers. *)
module Ident : sig
  type t

  val local : string -> t

  val global : string -> t

  val of_label : Label.t -> t

  val pp_t : Format.formatter -> t -> unit

  val to_label_string_exn : t -> string

  val to_string_hum : t -> string

  val to_string_encoded : t -> string

  module Gen : sig
    type ident = t

    type t = { mutable next : int }

    val create : unit -> t

    val get_fresh : t -> ident
  end
end

(** Values in LLVM IR. These are the main operands to instructions. They consist of a
    type and something that inhabits that type (confusingly called [value] here...).
    The latter can be an immediate, an identifier, among other things. *)
module Value : sig
  type contents

  type t

  val get_type : t -> Type.t

  val get_ident_exn : t -> Ident.t

  val get_contents : t -> contents

  val of_ident : typ:Type.t -> Ident.t -> t

  val of_symbol : ?typ:Type.t -> string -> t

  val of_int : ?typ:Type.t -> int -> t

  val of_nativeint : ?typ:Type.t -> nativeint -> t

  val of_float32_bits : int32 -> t

  val of_float64_bits : int64 -> t

  val of_float : typ:Type.t -> float -> t

  val of_label : Label.t -> t

  val of_string_constant : string -> t

  val poison : Type.t -> t

  val zeroinitializer : Type.t -> t

  val imm : Type.t -> string -> t

  val struct_constant : t list -> t

  val blockaddress : func:Ident.t -> block:Ident.t -> t

  val pp_contents : Format.formatter -> contents -> unit

  val pp_t : Format.formatter -> t -> unit
end

module Fn_attr : sig
  type t =
    | Cold
    | Gc of string
    | Gc_leaf_function
    | Noinline
    | Returns_twice
    | Statepoint_id of int

  val pp_t : Format.formatter -> t -> unit

  val pp_t_list : Format.formatter -> t list -> unit

  val to_string : t -> string

  val compare : t -> t -> int
end

module Calling_conventions : sig
  type t =
    | Default
    | Oxcaml
    | Oxcaml_c_call
    | Oxcaml_c_call_stack_args
    | Oxcaml_alloc

  val to_string : t -> string

  val pp_t : Format.formatter -> t -> unit
end

module Instruction : sig
  type switch_branch =
    { index : Value.t;
      label : Value.t
    }

  type unary_op = Fneg

  type binary_op =
    | Add
    | Sub
    | Mul
    | Udiv
    | Sdiv
    | Urem
    | Srem
    | Fadd
    | Fsub
    | Fmul
    | Fdiv
    | Frem
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
    | Sext
    | Zext
    | Trunc
    | Fpext
    | Fptrunc
    | Fptoui
    | Fptosi
    | Uitofp
    | Sitofp
    | Inttoptr
    | Ptrtoint
    | Ptrtoaddr
    | Addrspacecast
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

  val binary_op_to_string : binary_op -> string

  val unary_op_to_string : unary_op -> string

  val convert_op_to_string : convert_op -> string

  val icmp_cond_to_string : icmp_cond -> string

  val icmp_cond_of_ocaml : Cmm.integer_comparison -> icmp_cond

  val fcmp_cond_to_string : fcmp_cond -> string

  val fcmp_cond_of_ocaml : Cmm.float_comparison -> fcmp_cond

  val atomicrmw_op_to_string : atomicrmw_op -> string

  type op

  type t

  val op_res_type : op -> Type.Or_void.t

  val with_res : op -> Ident.t -> t

  val without_res : op -> t

  val get_res_value : t -> Value.t option

  val ret : Value.t -> op

  val br : Value.t -> op

  val br_cond : cond:Value.t -> ifso:Value.t -> ifnot:Value.t -> op

  val switch :
    discr:Value.t -> default:Value.t -> branches:switch_branch list -> op

  val unreachable : op

  val unary : unary_op -> arg:Value.t -> op

  val binary : binary_op -> arg1:Value.t -> arg2:Value.t -> op

  val convert : convert_op -> arg:Value.t -> to_:Type.t -> op

  val icmp : icmp_cond -> arg1:Value.t -> arg2:Value.t -> op

  val fcmp : fcmp_cond -> arg1:Value.t -> arg2:Value.t -> op

  val extractelement : vector:Value.t -> index:Value.t -> op

  val insertelement : vector:Value.t -> index:Value.t -> to_insert:Value.t -> op

  val extractvalue : aggregate:Value.t -> indices:int list -> op

  val insertvalue :
    aggregate:Value.t -> indices:int list -> to_insert:Value.t -> op

  val alloca : Type.t -> op

  val load : ptr:Value.t -> typ:Type.t -> op

  val store : ptr:Value.t -> to_store:Value.t -> op

  val getelementptr :
    base_type:Type.t -> base_ptr:Value.t -> indices:Value.t list -> op

  val cmpxchg :
    ptr:Value.t -> compare_with:Value.t -> set_if_equal:Value.t -> op

  val atomicrmw : atomicrmw_op -> ptr:Value.t -> arg:Value.t -> op

  val select : cond:Value.t -> ifso:Value.t -> ifnot:Value.t -> op

  val call :
    func:Ident.t ->
    args:Value.t list ->
    res_type:Type.Or_void.t ->
    attrs:Fn_attr.t list ->
    cc:Calling_conventions.t ->
    musttail:bool ->
    op

  val inline_asm :
    args:Value.t list ->
    res_type:Type.Or_void.t ->
    asm:string ->
    constraints:string ->
    sideeffect:bool ->
    op

  val pp_t : ?comment:string -> Format.formatter -> t -> unit
end

module Function : sig
  type t

  val add_instruction : ?comment:string -> t -> Instruction.t -> unit

  val add_label_def : t -> Ident.t -> unit

  val add_comment : t -> string -> unit

  val pp_t : Format.formatter -> t -> unit

  module Emitter : sig
    type funcdef = t

    type t =
      { ident_gen : Ident.Gen.t;
        funcdef : funcdef
      }

    val create :
      name:string ->
      args:Type.t list ->
      res:Type.Or_void.t ->
      cc:Calling_conventions.t ->
      attrs:Fn_attr.t list ->
      dbg:Debuginfo.t ->
      private_:bool ->
      t

    val get_fun : t -> funcdef

    val get_args_as_values : t -> Value.t list

    val get_res_type : t -> Type.Or_void.t

    val get_fun_ident : t -> Ident.t

    val ins :
      ?comment:string -> ?res_ident:Ident.t -> t -> Instruction.op -> Value.t

    val ins_no_res : ?comment:string -> t -> Instruction.op -> unit

    val comment : t -> string -> unit

    val label_def : t -> Ident.t -> unit
  end
end

module Fundecl : sig
  type t =
    { name : string;
      args : Type.t list;
      res : Type.Or_void.t
    }

  val create : string -> Type.t list -> Type.Or_void.t -> t

  val pp_t : Format.formatter -> t -> unit

  val equal : t -> t -> bool
end

module Data : sig
  type t

  val constant :
    ?section:string option ->
    ?align:int option ->
    ?private_:bool ->
    string ->
    Value.t ->
    t

  val external_ : string -> t

  val pp_t : Format.formatter -> t -> unit
end

(** Useful miscellaneous printing functions for LLVM IR fragments *)
module Format : sig
  (* CR yusumez: This is sad. *)
  include module type of Format

  val pp_indent : formatter -> unit -> unit

  val pp_comma : formatter -> unit -> unit

  val pp_space : formatter -> unit -> unit

  val pp_line :
    ?num_indents:int ->
    ?append:string ->
    formatter ->
    ('a, formatter, unit, unit) format4 ->
    'a

  val pp_ins : formatter -> ('a, formatter, unit, unit) format4 -> 'a

  val pp_str_if : string -> formatter -> bool -> unit

  val do_if_comments_enabled : (unit -> unit) -> unit

  val pp_comment : formatter -> ('a, formatter, unit, unit) format4 -> 'a

  val pp_dbg_comment : formatter -> string -> Debuginfo.t -> unit

  val pp_dbg_instr_basic : formatter -> Cfg.basic Cfg.instruction -> unit

  val pp_dbg_instr_terminator :
    formatter -> Cfg.terminator Cfg.instruction -> unit

  val dbg_instr_basic_string : Cfg.basic Cfg.instruction -> tag

  val dbg_instr_terminator_string : Cfg.terminator Cfg.instruction -> tag
end
