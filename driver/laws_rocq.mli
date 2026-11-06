(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Jane Street Group LLC                              *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Translation of a compilation unit to Rocq, for proving its laws against a
    program logic for λ_dyn (the HeapLang of Iris).

    The generated file imports [law_prelude] and contains:
    - a semantic type [ty_t : sem_ty] (an inductive predicate on values) for
      each type [t] of the unit, so that law parameters can be typed;
    - a definition [f : val] (or [f : expr], when [f] is not a syntactic value)
      for each value [f] of the unit in the supported fragment, and a comment
      saying why for the others;
    - a definition [l_statement : Prop] for each law [l], so that its proof,
      [Lemma l : l_statement], lives in a separate file.

    Values are encoded as follows: integers, booleans and [()] are literals,
    tuples and records are right-nested pairs, constructor number [i] of a
    variant (in declaration order) with payload [p] is the pair [(#i, p)], and
    functions are curried closures. The items of a module [M] are prefixed with
    [M_], and the items of another unit [U] are [U_x] (the standard library's
    [Stdlib.List.rev] is [List_rev]).

    A clause [e] of a law denotes [spec_true e], i.e. [e] evaluates to [true],
    except that a clause [e1 = e2] denotes the logical equality [e1 =:= e2]. A
    law parameter [(x : t)] becomes [∀ x, ty_t x → ...], and a type variable
    ['a] a parameter [(A : sem_ty)]. *)

val write :
  source_file:string ->
  unit:Compilation_unit.t ->
  Typedtree.implementation ->
  output:string ->
  unit
(** [write ~source_file ~unit impl ~output] writes the Rocq file of the unit
    [unit], compiled from [source_file], to [output]. Raises [Location.Error]
    for a law that cannot be translated. *)
