(******************************************************************************
 *                             flambda-backend                                *
 *                       Vincent Laviron, OCamlPro                            *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 OCamlPro SAS                                   *
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

(** This module provides an engine for performing peephole optimisations on Cmm
    terms. A peephole optimisation is given as a rewriting rule, with a pattern
    on the left-hand side and a rewriting function on the right-hand side. The
    rewriting function takes as parameter an environment from which the
    sub-expressions that were matched to pattern variables can be retrieved.

    Currently the intended use is to call [run] on a newly created expression
    with a sequence of rewriting rules likely to match it, allowing to emulate
    smart constructors for Cmm expressions that would manually pattern-match on
    their arguments. Following that, the implementation only checks whether the
    whole expression matches a given pattern, and does not try to find
    sub-expressions that match.

    However, the API is compatible with a global engine that would apply a
    global set of rules to the whole program. (Although the engine itself is not
    suited for that and would have to be rewritten.) *)

(** Pattern variables usually match sub-expressions, but some expressions have
    integer payloads that may be relevant. This type defines the various cases
    supported by the engine. *)
type _ pattern_kind =
  | Expr : Cmm.expression pattern_kind
  | Int : int pattern_kind
  | Natint : Nativeint.t pattern_kind

(* Pattern variables. The type parameter tracks the type of terms that this
   variable would match. *)
type 'a pattern_var

(* Create a variable with a given name. All variables in a given pattern must be
   distinct. *)
val create_var : 'a pattern_kind -> string -> 'a pattern_var

(* Pre-defined variables for names commonly used in Cmm_helpers patterns. *)
module Default_variables : sig
  val c : Cmm.expression pattern_var

  val c1 : Cmm.expression pattern_var

  val c2 : Cmm.expression pattern_var

  val n : int pattern_var

  val n1 : int pattern_var

  val n2 : int pattern_var
end

(* The type for right-hand side environments. They are created by the engine,
   and accessed through the [Syntax] module defined later. *)
module Env : sig
  type t
end

(** Binary integer operations, each corresponding to exactly one [Cop]. *)
type op =
  | Add
  | Sub
  | Mul
  | And
  | Or
  | Xor
  | Lsl
  | Lsr
  | Asr

val is_commutative : op -> bool

(* Binary operator patterns. Some match a single operations, others can match a
   whole class of operations. *)
type binop =
  | Op of op
  | Comparison
      (** Matches all versions of the [Ccmpi] and [Ccmpf] operations *)
  | Bitwise_op  (** All binary bit-wise operations: [Cand], [Cor], [Cxor] *)

type cmm_pattern =
  | Any of Cmm.expression pattern_var
      (** Wildcard pattern, binding a variable *)
  | Same of Cmm.expression pattern_var
      (** Matches an expression equivalent to the one already bound to the
          variable, provided it is a variable or a constant, so that the
          rewritten result may mention it any number of times. Using an unbound
          variable is a fatal error. *)
  | As of Cmm.expression pattern_var * cmm_pattern
      (** Variable binding with nested pattern *)
  | Const_int_fixed of int  (** Matches [Cconst_int] with a given integer *)
  | Const_int of int pattern_var
      (** Matches any [Cconst_int] and binds the underlying integer *)
  | Const_natint_fixed of Nativeint.t
      (** Matches [Cconst_natint] with a given integer *)
  | Const_natint of Nativeint.t pattern_var
      (** Matches any [Cconst_natint] and binds the underlying integer *)
  | Const_any_fixed of Nativeint.t
      (** Matches [Cconst_int] or [Cconst_natint] with a given integer *)
  | Const_any of Nativeint.t pattern_var
      (** Matches any [Cconst_int] or [Cconst_natint] and binds the underlying
          integer *)
  | Const_same of Nativeint.t pattern_var
      (** Matches a [Cconst_int] or [Cconst_natint] equal to the integer already
          bound to the variable. Using an unbound variable is a fatal error. *)
  | Binop of binop * cmm_pattern * cmm_pattern
      (** Matches the corresponding [Cop] terms *)
  | Binop_comm of binop * cmm_pattern * cmm_pattern
      (** Like [Binop], but also tries matching the operands in the other order.
          Sub-patterns are matched left to right in both cases, so [Same]
          variables in the right sub-pattern may be bound by the left one. *)
  | Guarded of
      { pat : cmm_pattern;
        guard : Env.t -> bool
      }
      (** When [pat] matches, the corresponding environment is passed to
          [guard]. If this returns [true] then the whole pattern matches with
          the same environment, otherwise the pattern doesn't match. *)

(** The type of rewriting rules. Creating rules is done using the [Syntax]
    module below. The type parameter ['a] allows to write clauses that are not
    rewriting rules but compute an arbitrary value of type ['a] by matching on a
    Cmm expression *)
type 'a clause

(** The entry point for the engine. Tries the rules in order, and applies the
    first that matches. If no rules match, returns the original expression. *)
val run : Cmm.expression -> Cmm.expression clause list -> Cmm.expression

(** An extension of the engine allowing to run on arbitrary clauses. The
    [default] parameter is called if none of the clauses match. *)
val run_default :
  default:(Cmm.expression -> 'a) -> Cmm.expression -> 'a clause list -> 'a

module Syntax : sig
  (** Constructor for clauses: [lhs => rhs] *)
  val ( => ) : cmm_pattern -> (Env.t -> 'a) -> 'a clause

  (** Environment accessor: [env#.var] *)
  val ( #. ) : Env.t -> 'a pattern_var -> 'a
end

(** Whether two expressions are known to denote the same machine word. Only
    variables and constants are recognised, so that evaluating one of the two
    expressions instead of both is equivalent. *)
val same_simple_value : Cmm.expression -> Cmm.expression -> bool

(** Check equivalence of Cmm terms for the purpose of checking that the engine
    produces terms equivalent to the ones produced by the original code. *)
module Cmm_comparator : sig
  val equivalent : Cmm.expression -> Cmm.expression -> bool
end

(** Rewriting rules given as data: a left-hand side term, a side condition on
    its constant parameters, and a right-hand side term. The same rule is both
    applied by the engine ([run]) and printed as an SMT-LIB2 problem
    ([print_smt2]) whose expected answer, [unsat], proves that the two sides
    agree for every value of the variables and constants. *)
module Rule : sig
  (** Arithmetic on the constant parameters of a rule, with the semantics of the
      corresponding bit-vector operations. *)
  type int_term =
    | I_var of Nativeint.t pattern_var
    | I_lit of Nativeint.t
    | I_op of op * int_term * int_term

  type 'const term =
    | Var of Cmm.expression pattern_var
    | Lit of Nativeint.t
    | Const of 'const
    | Apply of op * 'const term * 'const term

  (** On the left-hand side, [Const v] matches a constant and binds it to [v].
      Variables and constants occurring more than once must be bound to equal
      terms; repeated variables only match Cmm variables and constants, so that
      the right-hand side may mention them any number of times. *)
  type lhs = Nativeint.t pattern_var term

  (** On the right-hand side, [Const i] is the constant computed by [i]. *)
  type rhs = int_term term

  type cond =
    | True
    | All of cond list
    | Eq of int_term * int_term
    | Slt of int_term * int_term  (** Signed comparison *)
    | Is_defined_shift of int_term  (** [0 <= n < arch_bits] *)

  type t

  val word_bits : int

  (** [create ?cond op (lhs1, lhs2) rhs] is the rule rewriting [op lhs1 lhs2] to
      [rhs] when [cond] holds. Shift amounts on the left-hand side must be
      constants; that they are defined is added to [cond]. The right-hand side
      must only use variables bound by the left-hand side, must not mention a
      variable more often than the left-hand side does, and must contain
      strictly fewer operations. Violations are fatal errors. *)
  val create : ?cond:cond -> op -> lhs * lhs -> rhs -> t

  val root_op : t -> op

  (** [lhs -> rhs if cond], as echoed in the SMT-LIB output. *)
  val name : t -> string

  (** How to build the right-hand side: [const] and [binop] build constants and
      operations, and [sequence e body] evaluates [e] for its effects before
      [body], for the sub-terms matched on the left but absent on the right. *)
  type constructors =
    { const : Nativeint.t -> Cmm.expression;
      binop : op -> Cmm.expression -> Cmm.expression -> Cmm.expression;
      sequence : Cmm.expression -> Cmm.expression -> Cmm.expression
    }

  val to_clause : constructors -> t -> Cmm.expression clause

  val run : constructors -> t list -> Cmm.expression -> Cmm.expression

  val print_smt2_header : Format.formatter -> unit

  val print_smt2 : Format.formatter -> t -> unit
end
