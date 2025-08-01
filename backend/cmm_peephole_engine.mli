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

(** This module provides an engine for performing peephole optimisations on
    Cmm terms. A peephole optimisation is given as a rewriting rule, with
    a pattern on the left-hand side and a rewriting function on the right-hand side.
    The rewriting function takes as parameter an environment from which the sub-expressions
    that were matched to pattern variables can be retrieved.

    Currently the intended use is to call [run] on a newly created expression with a sequence
    of rewriting rules likely to match it, allowing to emulate smart constructors for Cmm
    expressions that would manually pattern-match on their arguments.
    Following that, the implementation only checks whether the whole expression matches
    a given pattern, and does not try to find sub-expressions that match.

    However, the API is compatible with a global engine that would apply a global set of rules
    to the whole program. (Although the engine itself is not suited for that and would have
    to be rewritten.) *)

(** Pattern variables usually match sub-expressions, but some expressions have
    integer payloads that may be relevant.
    This type defines the various cases supported by the engine. *)
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

(* Binary operator patterns. Some match a single operations, others can match a
   whole class of operations. *)
type binop =
  | Add
  | Sub
  | Lsl
  | Lsr
  | Asr
  | Or
  | And
  | Comparison
      (** Matches all versions of the [Ccmpi] and [Ccmpf] operations *)
  | Bitwise_op  (** All binary bit-wise operations: [Cand], [Cor], [Cxor] *)

type cmm_pattern =
  | Any of Cmm.expression pattern_var
      (** Wildcard pattern, binding a variable *)
  | As of Cmm.expression pattern_var * cmm_pattern
      (** Variable binding with nested pattern *)
  | Const_int_fixed of int  (** Matches [Cconst_int] with a given integer *)
  | Const_int of int pattern_var
      (** Matches any [Cconst_int] and binds the underlying integer *)
  | Const_natint_fixed of Nativeint.t
      (** Matches [Cconst_natint] with a given integer *)
  | Const_natint of Nativeint.t pattern_var
      (** Matches any [Cconst_natint] and binds the underlying integer *)
  | Binop of binop * cmm_pattern * cmm_pattern
      (** Matches the corresponding [Cop] terms *)
  | Guarded of
      { pat : cmm_pattern;
        guard : Env.t -> bool
      }
      (** When [pat] matches, the corresponding environment is passed to [guard].
            If this returns [true] then the whole pattern matches with the same environment,
            otherwise the pattern doesn't match. *)

(** The type of rewriting rules. Creating rules is done using the [Syntax] module below.
    The type parameter ['a] allows to write clauses that are not rewriting rules but
    compute an arbitrary value of type ['a] by matching on a Cmm expression *)
type 'a clause

(** The entry point for the engine. Tries the rules in order, and applies the first that
   matches. If no rules match, returns the original expression. *)
val run : Cmm.expression -> Cmm.expression clause list -> Cmm.expression

(** An extension of the engine allowing to run on arbitrary clauses. The [default]
    parameter is called if none of the clauses match. *)
val run_default :
  default:(Cmm.expression -> 'a) -> Cmm.expression -> 'a clause list -> 'a

module Syntax : sig
  (** Constructor for clauses: [lhs => rhs] *)
  val ( => ) : cmm_pattern -> (Env.t -> 'a) -> 'a clause

  (** Environment accessor: [env#.var] *)
  val ( #. ) : Env.t -> 'a pattern_var -> 'a
end

(** Check equivalence of Cmm terms for the purpose of checking that
    the engine produces terms equivalent to the ones produced by the
    original code. *)
module Cmm_comparator : sig
  val equivalent : Cmm.expression -> Cmm.expression -> bool
end
