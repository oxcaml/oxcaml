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

(* Note on updating the engine *)
(* This module was initially merged with support for a subset of the Cmm
   peephole optimisations performed in Cmm_helpers. It was meant to be extended
   incrementally, and the individual commits adding support for the various
   operations were meant to serve as reference for further extensions. These
   individual commits have disappeared from the main history, but they have been
   preserved in the cmm-peephole-engine branch of the main repo, accessible at
   https://github.com/oxcaml/oxcaml/tree/cmm-peephole-engine. *)

(* Variable kinds *)

type _ pattern_kind =
  | Expr : Cmm.expression pattern_kind
  | Int : int pattern_kind
  | Natint : Nativeint.t pattern_kind

type 'a pattern_var =
  { id : int;
    name : string;
    kind : 'a pattern_kind
  }

let var_counter = ref 0

let create_var kind name =
  incr var_counter;
  { id = !var_counter; name; kind }

module IM = Numbers.Int.Map

module Env : sig
  type t

  val empty : t

  val add : t -> 'a pattern_var -> 'a -> t

  val find_exn : t -> 'a pattern_var -> 'a

  val register_phantom_let :
    t ->
    phantom_var:Backend_var.With_provenance.t ->
    defining_expr:Cmm.phantom_defining_expr option ->
    t

  val place_phantom_lets : t -> Cmm.expression -> Cmm.expression
end = struct
  type t =
    { exprs : Cmm.expression IM.t;
      ints : int IM.t;
      natints : Nativeint.t IM.t;
      phantom_lets_rev :
        (Backend_var.With_provenance.t * Cmm.phantom_defining_expr option) list
    }

  let empty =
    { exprs = IM.empty;
      ints = IM.empty;
      natints = IM.empty;
      phantom_lets_rev = []
    }

  let add (type a) env (var : a pattern_var) (expr : a) =
    match var.kind with
    | Expr ->
      if IM.mem var.id env.exprs
      then Misc.fatal_errorf "Duplicate binding for var %s" var.name
      else { env with exprs = IM.add var.id expr env.exprs }
    | Int ->
      if IM.mem var.id env.ints
      then Misc.fatal_errorf "Duplicate binding for var %s" var.name
      else { env with ints = IM.add var.id expr env.ints }
    | Natint ->
      if IM.mem var.id env.natints
      then Misc.fatal_errorf "Duplicate binding for var %s" var.name
      else { env with natints = IM.add var.id expr env.natints }

  let find_exn (type a) env (var : a pattern_var) : a =
    match var.kind with
    | Expr -> IM.find var.id env.exprs
    | Int -> IM.find var.id env.ints
    | Natint -> IM.find var.id env.natints

  let register_phantom_let env ~phantom_var ~defining_expr =
    { env with
      phantom_lets_rev = (phantom_var, defining_expr) :: env.phantom_lets_rev
    }

  let place_phantom_lets env expr =
    List.fold_left
      (fun expr (phantom_var, defining_expr) ->
        Cmm.Cphantom_let (phantom_var, defining_expr, expr))
      expr env.phantom_lets_rev
end

type binop =
  | Add
  | Sub
  | Lsl
  | Lsr
  | Asr
  | Or
  | And
  | Comparison
  | Bitwise_op

type cmm_pattern =
  | Any of Cmm.expression pattern_var
  | As of Cmm.expression pattern_var * cmm_pattern
  | Const_int_fixed of int
  | Const_int of int pattern_var
  | Const_natint_fixed of Nativeint.t
  | Const_natint of Nativeint.t pattern_var
  | Binop of binop * cmm_pattern * cmm_pattern
  | Guarded of
      { pat : cmm_pattern;
        guard : Env.t -> bool
      }

type 'a clause = cmm_pattern * (Env.t -> 'a)

let matches_binop (binop : binop) (cop : Cmm.operation) =
  match binop, cop with
  | Add, Caddi -> true
  | Sub, Csubi -> true
  | Lsl, Clsl -> true
  | Lsr, Clsr -> true
  | Asr, Casr -> true
  | Or, Cor -> true
  | And, Cand -> true
  | Comparison, (Ccmpi _ | Ccmpf _) -> true
  | Bitwise_op, (Cand | Cor | Cxor) -> true
  | _, _ -> false

let match_clauses_in_order ~default ~matches clauses expr =
  let ( let* ) = Option.bind in
  let rec match_one_pattern env pat (expr : Cmm.expression) =
    match expr with
    | Cphantom_let (phantom_var, defining_expr, expr) ->
      let env = Env.register_phantom_let env ~phantom_var ~defining_expr in
      match_one_pattern env pat expr
    | _ -> (
      match pat, expr with
      | Any v, expr -> Some (Env.add env v expr)
      | As (v, pat), expr ->
        let* env = match_one_pattern env pat expr in
        Some (Env.add env v expr)
      | Const_int_fixed n1, Cconst_int (n2, _) ->
        if Int.equal n1 n2 then Some env else None
      | Const_int v, Cconst_int (n, _) -> Some (Env.add env v n)
      | Const_natint_fixed n1, Cconst_natint (n2, _) ->
        if Nativeint.equal n1 n2 then Some env else None
      | Const_natint v, Cconst_natint (n, _) -> Some (Env.add env v n)
      | Binop (binop, pat1, pat2), Cop (cop, [expr1; expr2], _) ->
        if matches_binop binop cop
        then
          let* env = match_one_pattern env pat1 expr1 in
          match_one_pattern env pat2 expr2
        else None
      | Guarded { pat; guard }, expr ->
        let* env = match_one_pattern env pat expr in
        if guard env then Some env else None
      | _, _ -> None)
  in
  let rec find_matching_clause expr = function
    | [] -> default expr
    | (pat, f) :: clauses -> (
      match match_one_pattern Env.empty pat expr with
      | Some env -> matches env (f env)
      | None -> find_matching_clause expr clauses)
  in
  find_matching_clause expr clauses

let run expr clauses =
  match_clauses_in_order ~default:Fun.id ~matches:Env.place_phantom_lets clauses
    expr

let run_default ~default expr clauses =
  match_clauses_in_order ~default ~matches:(fun _env x -> x) clauses expr

module Syntax = struct
  let ( => ) lhs rhs = lhs, rhs

  let ( #. ) = Env.find_exn
end

module Default_variables = struct
  let c = create_var Expr "c"

  let c1 = create_var Expr "c1"

  let c2 = create_var Expr "c2"

  let n = create_var Int "n"

  let n1 = create_var Int "n1"

  let n2 = create_var Int "n2"
end

module Cmm_comparator = struct
  (* Note: some of the equality functions should be implemented properly in Cmm;
     for now we write the simple ones here and use structural equality for the
     more annoying ones. *)
  let equal_exit_label (x : Cmm.exit_label) (y : Cmm.exit_label) =
    (* Use structural equality *)
    x = y

  let equal_ccatch_flag (x : Cmm.ccatch_flag) (y : Cmm.ccatch_flag) =
    (* Use structural equality *)
    x = y

  let equal_trap_action (x : Cmm.trap_action) (y : Cmm.trap_action) =
    (* Use structural equality *)
    x = y

  let equal_operation (x : Cmm.operation) (y : Cmm.operation) =
    (* Use structural equality *)
    x = y

  let equal_phantom_defining_expr (x : Cmm.phantom_defining_expr)
      (y : Cmm.phantom_defining_expr) =
    (* Use structural equality *)
    x = y

  let equal_symbol (s1 : Cmm.symbol) (s2 : Cmm.symbol) =
    String.equal s1.sym_name s2.sym_name
    && Cmm.equal_is_global s1.sym_global s2.sym_global

  let equal_vec128_bits (v1 : Cmm.vec128_bits) (v2 : Cmm.vec128_bits) =
    Int64.equal v1.word0 v2.word0 && Int64.equal v1.word1 v2.word1

  let equal_vec256_bits (v1 : Cmm.vec256_bits) (v2 : Cmm.vec256_bits) =
    Int64.equal v1.word0 v2.word0
    && Int64.equal v1.word1 v2.word1
    && Int64.equal v1.word2 v2.word2
    && Int64.equal v1.word3 v2.word3

  let equal_vec512_bits (v1 : Cmm.vec512_bits) (v2 : Cmm.vec512_bits) =
    Int64.equal v1.word0 v2.word0
    && Int64.equal v1.word1 v2.word1
    && Int64.equal v1.word2 v2.word2
    && Int64.equal v1.word3 v2.word3
    && Int64.equal v1.word4 v2.word4
    && Int64.equal v1.word5 v2.word5
    && Int64.equal v1.word6 v2.word6
    && Int64.equal v1.word7 v2.word7

  (* Checks equivalence of expressions. At the moment this ignores debuginfo and
     phantom expressions. *)
  let rec equivalent (x : Cmm.expression) (y : Cmm.expression) =
    let module V = Backend_var in
    let module VP = Backend_var.With_provenance in
    match x, y with
    | Cconst_int (n1, _), Cconst_int (n2, _) -> Int.equal n1 n2
    | Cconst_natint (n1, _), Cconst_natint (n2, _) -> Nativeint.equal n1 n2
    | Cconst_float32 (f1, _), Cconst_float32 (f2, _) ->
      Int64.(equal (bits_of_float f1) (bits_of_float f2))
    | Cconst_float (f1, _), Cconst_float (f2, _) ->
      Int64.(equal (bits_of_float f1) (bits_of_float f2))
    | Cconst_vec128 (v1, _), Cconst_vec128 (v2, _) -> equal_vec128_bits v1 v2
    | Cconst_vec256 (v1, _), Cconst_vec256 (v2, _) -> equal_vec256_bits v1 v2
    | Cconst_vec512 (v1, _), Cconst_vec512 (v2, _) -> equal_vec512_bits v1 v2
    | Cconst_symbol (s1, _), Cconst_symbol (s2, _) -> equal_symbol s1 s2
    | Cvar v1, Cvar v2 -> V.equal v1 v2
    | Clet (v1, def1, body1), Clet (v2, def2, body2) ->
      (* No alpha-equivalence for now *)
      V.equal (VP.var v1) (VP.var v2)
      && equivalent def1 def2 && equivalent body1 body2
    | Cphantom_let (v1, def1, body1), Cphantom_let (v2, def2, body2) ->
      V.equal (VP.var v1) (VP.var v2)
      && Option.equal equal_phantom_defining_expr def1 def2
      && equivalent body1 body2
    | Ctuple t1, Ctuple t2 -> List.equal equivalent t1 t2
    | Cop (op1, args1, _), Cop (op2, args2, _) ->
      equal_operation op1 op2 && List.equal equivalent args1 args2
    | Csequence (before1, after1), Csequence (before2, after2) ->
      equivalent before1 before2 && equivalent after1 after2
    | ( Cifthenelse (cond1, _, ifso1, _, ifnot1, _),
        Cifthenelse (cond2, _, ifso2, _, ifnot2, _) ) ->
      equivalent cond1 cond2 && equivalent ifso1 ifso2
      && equivalent ifnot1 ifnot2
    | ( Cswitch (scrutinee1, cases1, actions1, _),
        Cswitch (scrutinee2, cases2, actions2, _) ) ->
      equivalent scrutinee1 scrutinee2
      && Misc.Stdlib.Array.equal Int.equal cases1 cases2
      && Misc.Stdlib.Array.equal
           (fun (act1, _) (act2, _) -> equivalent act1 act2)
           actions1 actions2
    | Ccatch (flag1, handlers1, body1), Ccatch (flag2, handlers2, body2) ->
      let equal_handler (lbl1, params1, body1, _, is_cold1)
          (lbl2, params2, body2, _, is_cold2) =
        (* No alpha equivalence *)
        Int.equal lbl1 lbl2
        && List.equal
             (fun (var1, mtype1) (var2, mtype2) ->
               V.equal (VP.var var1) (VP.var var2)
               && Misc.Stdlib.Array.equal Cmm.equal_machtype_component mtype1
                    mtype2)
             params1 params2
        && equivalent body1 body2
        && Bool.equal is_cold1 is_cold2
      in
      equal_ccatch_flag flag1 flag2
      && List.equal equal_handler handlers1 handlers2
      && equivalent body1 body2
    | Cexit (lbl1, args1, traps1), Cexit (lbl2, args2, traps2) ->
      equal_exit_label lbl1 lbl2
      && List.equal equivalent args1 args2
      && List.equal equal_trap_action traps1 traps2
    | ( ( Cconst_int (_, _)
        | Cconst_natint (_, _)
        | Cconst_float32 (_, _)
        | Cconst_float (_, _)
        | Cconst_vec128 (_, _)
        | Cconst_vec256 (_, _)
        | Cconst_vec512 (_, _)
        | Cconst_symbol (_, _)
        | Cvar _
        | Clet (_, _, _)
        | Cphantom_let (_, _, _)
        | Ctuple _
        | Cop (_, _, _)
        | Csequence (_, _)
        | Cifthenelse (_, _, _, _, _, _)
        | Cswitch (_, _, _, _)
        | Ccatch (_, _, _)
        | Cexit (_, _, _) ),
        _ ) ->
      false
end
