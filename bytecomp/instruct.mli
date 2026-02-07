(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The type of the instructions of the abstract machine *)

type structured_constant = Lambda.structured_constant

type raise_kind = Lambda.raise_kind

type comparison =
  | Eq
  | Neq
  | Ltint
  | Gtint
  | Leint
  | Geint
  | Ultint
  | Ugeint

type physical_comparison =
  | CPeq
  | CPneq

(* Structure of compilation environments *)

type closure_entry = Debug_event.closure_entry =
  | Free_variable of int
  | Function of int

type closure_env = Debug_event.closure_env =
  | Not_in_closure
  | In_closure of {
      entries: closure_entry Ident.tbl; (* Offsets of the free variables and
                                           recursive functions from the start of
                                           the block *)
      env_pos: int;                     (* Offset of the current function from
                                           the start of the block *)
    }

type compilation_env = Debug_event.compilation_env =
  { ce_stack: int Ident.tbl;  (* Positions of variables in the stack *)
    ce_closure: closure_env } (* Structure of the heap-allocated env *)

(* The ce_stack component gives locations of variables residing
   in the stack. The locations are offsets w.r.t. the origin of the
   stack frame.
   The ce_closure component gives the positions of variables residing in the
   heap-allocated environment. The env_pos component gives the position of
   the current function from the start of the closure block, and the entries
   component gives the positions of free variables and functions bound by the
   same let rec as the current function, from the start of the closure block.
   These are used by the ENVACC and OFFSETCLOSURE instructions to recover the
   relevant value from the env register (which points to the current function).
*)

(* Debugging events *)

type debug_event = Debug_event.debug_event =
  { mutable ev_pos: int;
    ev_module: string;
    ev_loc: Location.t;
    ev_kind: debug_event_kind;
    ev_defname: string;
    ev_info: debug_event_info;
    ev_typenv: Env.summary;
    ev_typsubst: Subst.t;
    ev_compenv: compilation_env;
    ev_stacksize: int;
    ev_repr: debug_event_repr }

and debug_event_kind = Debug_event.debug_event_kind =
    Event_before
  | Event_after of Types.type_expr
  | Event_pseudo
  | Event_after_untyped

and debug_event_info = Debug_event.debug_event_info =
    Event_function
  | Event_return of int
  | Event_unyielding_call of int
  | Event_other

and debug_event_repr = Debug_event.debug_event_repr =
    Event_none
  | Event_parent of int ref
  | Event_child of int ref

type closure_hint =
  { params : Lambda.layout list;
    return : Lambda.layout;
    inline : Lambda.inline_attribute;
    specialise : Lambda.specialise_attribute;
    is_a_functor : bool }

type ccall_hint =
  | Hint_unsafe
    (* Unsafe array, string or bytes access *)
  | Hint_int of Scalar.any_locality_mode Scalar.Integral.Boxable.Width.t
    (* Comparison between boxed integers *)
  | Hint_bigarray of
      { unsafe : bool;
        elt_kind : Lambda.bigarray_kind;
        layout : Lambda.bigarray_layout }
    (* Bigarray access *)
  | Hint_primitive of Lambda.external_call_description
    (* Primitive call *)
  | Hint_immediate_result
    (* The value returned by the C function is an immediate *)

type optimization_hint =
  | Hint_immutable_block
    (* Allocation of an immutable block *)
  | Hint_arraylength of Lambda.array_kind
    (* Array length *)
  | Hint_closures of closure_hint list
    (* Closure allocations *)
  | Hint_ccall of ccall_hint
    (* C call *)
  | Hint_int_equality_test
    (* Equality test between two immediates *)
  | Hint_immediate
    (* The value produced by the instruction is an immediate *)
  | Hint_variant
    (* The argument of the instruction is a value of a variant type: if it
       is an immediate, it is a constant constructor *)

(* Abstract machine instructions *)

type label = int                        (* Symbolic code labels *)

type instruction =
    Klabel of label
  | Kacc of int
  | Kenvacc of int
  | Kpush
  | Kpop of int
  | Kassign of int
  | Kpush_retaddr of label
  | Kapply of int                       (* number of arguments *)
  | Kappterm of int * int               (* number of arguments, slot size *)
  | Kreturn of int                      (* slot size *)
  | Krestart
  | Kgrab of int                        (* number of arguments *)
  | Kclosure of label * int * closure_hint
  | Kclosurerec of (label * closure_hint) list * int
  | Koffsetclosure of int
  | Kgetglobal of Compilation_unit.t
  | Ksetglobal of Compilation_unit.t
  | Kgetpredef of Ident.t
  | Kconst of structured_constant
  | Kmakeblock of int * int * Asttypes.mutable_flag
  | Kmake_faux_mixedblock of int * int  (* size, tag *)
  (* A "faux" mixed block is not actually represented as a mixed block at
     runtime. It just has the top header byte sent to a sentinel value so
     bytecode knows that the block can't be marshaled to native code, where
     mixed records are represented as true mixed blocks.
  *)
  | Kmakefloatblock of int * Asttypes.mutable_flag
  | Kgetfield of int * Lambda.immediate_or_pointer
  | Ksetfield of int
  | Kgetfloatfield of int
  | Ksetfloatfield of int
  | Kvectlength of Lambda.array_kind
  | Kgetvectitem of Lambda.immediate_or_pointer
  | Ksetvectitem
  | Kgetstringchar
  | Kgetbyteschar
  | Ksetbyteschar
  | Kbranch of label
  | Kbranchif of label
  | Kbranchifnot of label
  | Kstrictbranchif of label
  | Kstrictbranchifnot of label
  | Kswitch of label array * label array
  | Kboolnot
  | Kpushtrap of label
  | Kpoptrap
  | Kraise of raise_kind
  | Kcheck_signals
  | Kccall of string * int * ccall_hint option
  | Knegint | Kaddint | Ksubint | Kmulint | Kdivint | Kmodint
  | Kandint | Korint | Kxorint | Klslint | Klsrint | Kasrint
  | Kintcomp of comparison
  | Kphyscomp of physical_comparison
  | Koffsetint of int
  | Koffsetref of int
  | Kisint of bool (* the argument is a value of a variant type *)
  | Kgetmethod
  | Kgetpubmet of int
  | Kgetdynmet
  | Kevent of debug_event
  | Kperform
  | Kcontinue
  | Kcontinueterm of int
  | Kdiscontinue
  | Kdiscontinueterm of int
  | Kdiscontinue_with_backtrace
  | Kdiscontinue_with_backtraceterm of int
  | Kreperformterm of int
  | Kwith_stack
  | Kwith_stack_preemptible
  | Kstop

val immed_min: int
val immed_max: int
