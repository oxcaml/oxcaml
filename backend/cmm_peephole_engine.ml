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

  val find_opt : t -> 'a pattern_var -> 'a option

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

  let find_opt (type a) env (var : a pattern_var) : a option =
    match var.kind with
    | Expr -> IM.find_opt var.id env.exprs
    | Int -> IM.find_opt var.id env.ints
    | Natint -> IM.find_opt var.id env.natints

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
    | Cconst_mask (n1, _), Cconst_mask (n2, _) -> Int64.equal n1 n2
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
    | Cname_for_debugger (v1, e1), Cname_for_debugger (v2, e2) ->
      V.equal (VP.var v1) (VP.var v2) && equivalent e1 e2
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
      let equal_handler
          Cmm.
            { label = lbl1;
              params = params1;
              body = body1;
              is_cold = is_cold1;
              _
            }
          Cmm.
            { label = lbl2;
              params = params2;
              body = body2;
              is_cold = is_cold2;
              _
            } =
        (* No alpha equivalence *)
        Static_label.equal lbl1 lbl2
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
    | ( Cinvalid { message = m1; symbol = s1 },
        Cinvalid { message = m2; symbol = s2 } ) ->
      String.equal m1 m2 && equal_symbol s1 s2
    | ( ( Cconst_int (_, _)
        | Cconst_natint (_, _)
        | Cconst_float32 (_, _)
        | Cconst_float (_, _)
        | Cconst_vec128 (_, _)
        | Cconst_vec256 (_, _)
        | Cconst_vec512 (_, _)
        | Cconst_mask (_, _)
        | Cconst_symbol (_, _)
        | Cvar _
        | Clet (_, _, _)
        | Cphantom_let (_, _, _)
        | Cname_for_debugger _ | Ctuple _
        | Cop (_, _, _)
        | Csequence (_, _)
        | Cifthenelse (_, _, _, _, _, _)
        | Cswitch (_, _, _, _)
        | Ccatch (_, _, _)
        | Cexit (_, _, _)
        | Cinvalid _ ),
        _ ) ->
      false
end

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

type binop =
  | Op of op
  | Comparison
  | Bitwise_op

let is_commutative = function
  | Add | Mul | And | Or | Xor -> true
  | Sub | Lsl | Lsr | Asr -> false

type cmm_pattern =
  | Any of Cmm.expression pattern_var
  | Same of Cmm.expression pattern_var
  | As of Cmm.expression pattern_var * cmm_pattern
  | Const_int_fixed of int
  | Const_int of int pattern_var
  | Const_natint_fixed of Nativeint.t
  | Const_natint of Nativeint.t pattern_var
  | Const_any_fixed of Nativeint.t
  | Const_any of Nativeint.t pattern_var
  | Const_same of Nativeint.t pattern_var
  | Binop of binop * cmm_pattern * cmm_pattern
  | Binop_comm of binop * cmm_pattern * cmm_pattern
  | Guarded of
      { pat : cmm_pattern;
        guard : Env.t -> bool
      }

type 'a clause = cmm_pattern * (Env.t -> 'a)

let matches_binop (binop : binop) (cop : Cmm.operation) =
  match binop, cop with
  | Op Add, Caddi -> true
  | Op Sub, Csubi -> true
  | Op Mul, Cmuli -> true
  | Op And, Cand -> true
  | Op Or, Cor -> true
  | Op Xor, Cxor -> true
  | Op Lsl, Clsl -> true
  | Op Lsr, Clsr -> true
  | Op Asr, Casr -> true
  | Comparison, (Ccmpi _ | Ccmpf _) -> true
  | Bitwise_op, (Cand | Cor | Cxor) -> true
  | _, _ -> false

(* Whether two expressions are known to denote the same machine word. Only
   variables and constants are recognised, so that evaluating one of the two
   expressions instead of both is equivalent. *)
let same_simple_value (e1 : Cmm.expression) (e2 : Cmm.expression) =
  match e1, e2 with
  | Cvar v1, Cvar v2 -> Backend_var.same v1 v2
  | Cconst_int (n1, _), Cconst_int (n2, _) -> Int.equal n1 n2
  | Cconst_natint (n1, _), Cconst_natint (n2, _) -> Nativeint.equal n1 n2
  | Cconst_symbol (s1, _), Cconst_symbol (s2, _) ->
    String.equal s1.sym_name s2.sym_name
  | _ -> false

let match_clauses_in_order ~default ~matches clauses expr =
  let const_same env v n ~k =
    match Env.find_opt env v with
    | None -> Misc.fatal_errorf "Const_same on unbound var %s" v.name
    | Some bound -> if Nativeint.equal bound n then k env else None
  in
  let rec match_one_pattern env pat (expr : Cmm.expression) ~k =
    match expr with
    | Cphantom_let (phantom_var, defining_expr, expr) ->
      let env = Env.register_phantom_let env ~phantom_var ~defining_expr in
      match_one_pattern env pat expr ~k
    | Cname_for_debugger (_, body) -> match_one_pattern env pat body ~k
    | _ -> (
      match pat, expr with
      | Any v, expr -> k (Env.add env v expr)
      | Same v, expr -> (
        match Env.find_opt env v with
        | None -> Misc.fatal_errorf "Same on unbound var %s" v.name
        | Some bound -> if same_simple_value bound expr then k env else None)
      | As (v, pat), expr ->
        match_one_pattern env pat expr ~k:(fun env -> k (Env.add env v expr))
      | Const_int_fixed n1, Cconst_int (n2, _) ->
        if Int.equal n1 n2 then k env else None
      | Const_int v, Cconst_int (n, _) -> k (Env.add env v n)
      | Const_natint_fixed n1, Cconst_natint (n2, _) ->
        if Nativeint.equal n1 n2 then k env else None
      | Const_natint v, Cconst_natint (n, _) -> k (Env.add env v n)
      | Const_any_fixed n1, Cconst_int (n2, _) ->
        if Nativeint.equal n1 (Nativeint.of_int n2) then k env else None
      | Const_any_fixed n1, Cconst_natint (n2, _) ->
        if Nativeint.equal n1 n2 then k env else None
      | Const_any v, Cconst_int (n, _) -> k (Env.add env v (Nativeint.of_int n))
      | Const_any v, Cconst_natint (n, _) -> k (Env.add env v n)
      | Const_same v, Cconst_int (n, _) ->
        const_same env v (Nativeint.of_int n) ~k
      | Const_same v, Cconst_natint (n, _) -> const_same env v n ~k
      | Binop (binop, pat1, pat2), Cop (cop, [expr1; expr2], _) ->
        if matches_binop binop cop
        then match_pair env (pat1, expr1) (pat2, expr2) ~k
        else None
      | Binop_comm (binop, pat1, pat2), Cop (cop, [expr1; expr2], _) ->
        if matches_binop binop cop
        then
          match match_pair env (pat1, expr1) (pat2, expr2) ~k with
          | Some _ as result -> result
          | None -> match_pair env (pat1, expr2) (pat2, expr1) ~k
        else None
      | Guarded { pat; guard }, expr ->
        match_one_pattern env pat expr ~k:(fun env ->
            if guard env then k env else None)
      | _, _ -> None)
  and match_pair env (pat1, expr1) (pat2, expr2) ~k =
    match_one_pattern env pat1 expr1 ~k:(fun env ->
        match_one_pattern env pat2 expr2 ~k)
  in
  let rec find_matching_clause expr = function
    | [] -> default expr
    | (pat, f) :: clauses -> (
      match
        match_one_pattern Env.empty pat expr ~k:(fun env ->
            Some (matches env (f env)))
      with
      | Some result -> result
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

module Rule = struct
  type int_term =
    | I_var of Nativeint.t pattern_var
    | I_lit of Nativeint.t
    | I_op of op * int_term * int_term

  type 'const term =
    | Var of Cmm.expression pattern_var
    | Lit of Nativeint.t
    | Const of 'const
    | Apply of op * 'const term * 'const term

  type lhs = Nativeint.t pattern_var term

  type rhs = int_term term

  type cond =
    | True
    | All of cond list
    | Eq of int_term * int_term
    | Slt of int_term * int_term
    | Is_defined_shift of int_term

  type t =
    { name : string;
      root_op : op;
      lhs : lhs;
      cond : cond;
      rhs : rhs;
      pattern : cmm_pattern;
      dropped : Cmm.expression pattern_var list;
      expr_vars : Cmm.expression pattern_var list;
      nat_vars : Nativeint.t pattern_var list
    }

  let word_bits = Arch.size_int * 8

  let var_name (v : _ pattern_var) = v.name

  let root_op t = t.root_op

  let name t = t.name

  (* Printing *)

  let op_symbol = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | And -> "&"
    | Or -> "|"
    | Xor -> "^"
    | Lsl -> "<<"
    | Lsr -> ">>u"
    | Asr -> ">>s"

  let rec print_int_term ppf = function
    | I_var v -> Format.pp_print_string ppf (var_name v)
    | I_lit n -> Format.fprintf ppf "%nd" n
    | I_op (op, a, b) ->
      Format.fprintf ppf "(%a %s %a)" print_int_term a (op_symbol op)
        print_int_term b

  let rec print_term print_const ~top ppf = function
    | Var v -> Format.pp_print_string ppf (var_name v)
    | Lit n -> Format.fprintf ppf "%nd" n
    | Const c -> print_const ppf c
    | Apply (op, a, b) ->
      Format.fprintf ppf
        (if top then "%a %s %a" else "(%a %s %a)")
        (print_term print_const ~top:false)
        a (op_symbol op)
        (print_term print_const ~top:false)
        b

  let print_lhs =
    print_term (fun ppf v -> Format.pp_print_string ppf (var_name v))

  let print_rhs = print_term print_int_term

  let rec print_cond ppf = function
    | True -> Format.pp_print_string ppf "true"
    | All conds ->
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf " && ")
        print_cond ppf conds
    | Eq (a, b) ->
      Format.fprintf ppf "%a = %a" print_int_term a print_int_term b
    | Slt (a, b) ->
      Format.fprintf ppf "%a < %a" print_int_term a print_int_term b
    | Is_defined_shift a ->
      Format.fprintf ppf "0 <= %a < %d" print_int_term a word_bits

  (* Variables *)

  let add_var vars v =
    if List.exists (fun v' -> v'.id = v.id) vars then vars else vars @ [v]

  let rec int_term_vars vars = function
    | I_var v -> add_var vars v
    | I_lit _ -> vars
    | I_op (_, a, b) -> int_term_vars (int_term_vars vars a) b

  let rec cond_vars vars = function
    | True -> vars
    | All conds -> List.fold_left cond_vars vars conds
    | Eq (a, b) | Slt (a, b) -> int_term_vars (int_term_vars vars a) b
    | Is_defined_shift a -> int_term_vars vars a

  let rec term_vars ~const_vars (expr_vars, nat_vars) = function
    | Var v -> add_var expr_vars v, nat_vars
    | Lit _ -> expr_vars, nat_vars
    | Const c -> expr_vars, const_vars nat_vars c
    | Apply (_, a, b) ->
      term_vars ~const_vars (term_vars ~const_vars (expr_vars, nat_vars) a) b

  let lhs_vars = term_vars ~const_vars:add_var ([], [])

  let rhs_vars = term_vars ~const_vars:int_term_vars ([], [])

  let rec count_var v = function
    | Var v' -> if v'.id = v.id then 1 else 0
    | Lit _ | Const _ -> 0
    | Apply (_, a, b) -> count_var v a + count_var v b

  let rec size = function
    | Var _ | Lit _ | Const _ -> 0
    | Apply (_, a, b) -> 1 + size a + size b

  let rec shift_amounts acc = function
    | Var _ | Lit _ | Const _ -> acc
    | Apply (op, a, b) -> (
      let acc = shift_amounts (shift_amounts acc a) b in
      match op with
      | Lsl | Lsr | Asr -> b :: acc
      | Add | Sub | Mul | And | Or | Xor -> acc)

  (* Pattern derived from the left-hand side. Repeated variables become
     [Same]/[Const_same]; commutative operations match both operand orders. *)

  let lhs_to_pattern lhs =
    let rec go (seen_exprs, seen_nats) = function
      | Var v ->
        if List.mem v.id seen_exprs
        then Same v, (seen_exprs, seen_nats)
        else Any v, (v.id :: seen_exprs, seen_nats)
      | Lit n -> Const_any_fixed n, (seen_exprs, seen_nats)
      | Const v ->
        if List.mem v.id seen_nats
        then Const_same v, (seen_exprs, seen_nats)
        else Const_any v, (seen_exprs, v.id :: seen_nats)
      | Apply (op, a, b) ->
        let pa, seen = go (seen_exprs, seen_nats) a in
        let pb, seen = go seen b in
        ( (if is_commutative op
           then Binop_comm (Op op, pa, pb)
           else Binop (Op op, pa, pb)),
          seen )
    in
    let pat, _ = go ([], []) lhs in
    pat

  (* Evaluation of constant parameters, matching the SMT-LIB semantics of the
     corresponding bit-vector operations. *)

  let is_defined_shift n =
    Nativeint.unsigned_compare n (Nativeint.of_int word_bits) < 0

  let rec eval_int env = function
    | I_var v -> Env.find_exn env v
    | I_lit n -> n
    | I_op (op, a, b) -> (
      let a = eval_int env a in
      let b = eval_int env b in
      match op with
      | Add -> Nativeint.add a b
      | Sub -> Nativeint.sub a b
      | Mul -> Nativeint.mul a b
      | And -> Nativeint.logand a b
      | Or -> Nativeint.logor a b
      | Xor -> Nativeint.logxor a b
      | Lsl ->
        if is_defined_shift b
        then Nativeint.shift_left a (Nativeint.to_int b)
        else 0n
      | Lsr ->
        if is_defined_shift b
        then Nativeint.shift_right_logical a (Nativeint.to_int b)
        else 0n
      | Asr ->
        if is_defined_shift b
        then Nativeint.shift_right a (Nativeint.to_int b)
        else if Nativeint.compare a 0n < 0
        then -1n
        else 0n)

  let rec eval_cond env = function
    | True -> true
    | All conds -> List.for_all (eval_cond env) conds
    | Eq (a, b) -> Nativeint.equal (eval_int env a) (eval_int env b)
    | Slt (a, b) -> Nativeint.compare (eval_int env a) (eval_int env b) < 0
    | Is_defined_shift a -> is_defined_shift (eval_int env a)

  let conjunction conds =
    match
      List.concat_map
        (function
          | True -> []
          | All conds -> conds
          | (Eq _ | Slt _ | Is_defined_shift _) as cond -> [cond])
        conds
    with
    | [] -> True
    | [cond] -> cond
    | conds -> All conds

  let create ?(cond = True) root_op (lhs1, lhs2) rhs =
    let lhs = Apply (root_op, lhs1, lhs2) in
    let rule =
      Format.asprintf "%a -> %a" (print_lhs ~top:true) lhs (print_rhs ~top:true)
        rhs
    in
    let fatal fmt =
      Misc.fatal_errorf ("Cmm peephole rule [%s]: " ^^ fmt) rule
    in
    let expr_vars, nat_vars = lhs_vars lhs in
    let rhs_expr_vars, rhs_nat_vars = rhs_vars rhs in
    let cond_nat_vars = cond_vars [] cond in
    let unbound vars bound =
      List.filter
        (fun v -> not (List.exists (fun v' -> v'.id = v.id) bound))
        vars
    in
    (match
       ( unbound rhs_expr_vars expr_vars,
         unbound (rhs_nat_vars @ cond_nat_vars) nat_vars )
     with
    | [], [] -> ()
    | v :: _, _ ->
      fatal "variable %s is not bound by the left-hand side" (var_name v)
    | [], v :: _ ->
      fatal "constant %s is not bound by the left-hand side" (var_name v));
    let names = List.map var_name expr_vars @ List.map var_name nat_vars in
    if List.length (List.sort_uniq String.compare names) <> List.length names
    then fatal "variable names must be distinct";
    List.iter
      (fun v ->
        if count_var v rhs > count_var v lhs
        then
          fatal "variable %s is duplicated by the right-hand side" (var_name v))
      expr_vars;
    if size rhs >= size lhs
    then fatal "the right-hand side must be strictly smaller";
    let shift_conds =
      List.map
        (function
          | Const v -> Is_defined_shift (I_var v)
          | Lit n -> Is_defined_shift (I_lit n)
          | Var _ | Apply _ ->
            fatal "shift amounts on the left-hand side must be constants")
        (shift_amounts [] lhs)
    in
    let cond =
      conjunction (List.sort_uniq Stdlib.compare shift_conds @ [cond])
    in
    let name =
      match cond with
      | True -> rule
      | All _ | Eq _ | Slt _ | Is_defined_shift _ ->
        Format.asprintf "%s if %a" rule print_cond cond
    in
    let pattern = lhs_to_pattern lhs in
    let pattern =
      match cond with
      | True -> pattern
      | All _ | Eq _ | Slt _ | Is_defined_shift _ ->
        Guarded { pat = pattern; guard = (fun env -> eval_cond env cond) }
    in
    let dropped = List.filter (fun v -> count_var v rhs = 0) expr_vars in
    { name; root_op; lhs; cond; rhs; pattern; dropped; expr_vars; nat_vars }

  (* Application *)

  type constructors =
    { const : Nativeint.t -> Cmm.expression;
      binop : op -> Cmm.expression -> Cmm.expression -> Cmm.expression;
      sequence : Cmm.expression -> Cmm.expression -> Cmm.expression
    }

  let to_clause constructors t : Cmm.expression clause =
    ( t.pattern,
      fun env ->
        let rec build = function
          | Var v -> Env.find_exn env v
          | Lit n -> constructors.const n
          | Const i -> constructors.const (eval_int env i)
          | Apply (op, a, b) -> constructors.binop op (build a) (build b)
        in
        List.fold_right
          (fun v body -> constructors.sequence (Env.find_exn env v) body)
          t.dropped (build t.rhs) )

  let run constructors rules expr =
    run expr (List.map (to_clause constructors) rules)

  (* SMT-LIB output *)

  let smt_op = function
    | Add -> "bvadd"
    | Sub -> "bvsub"
    | Mul -> "bvmul"
    | And -> "bvand"
    | Or -> "bvor"
    | Xor -> "bvxor"
    | Lsl -> "bvshl"
    | Lsr -> "bvlshr"
    | Asr -> "bvashr"

  let smt_lit n = Printf.sprintf "#x%016Lx" (Int64.of_nativeint n)

  let rec smt_int_term = function
    | I_var v -> var_name v
    | I_lit n -> smt_lit n
    | I_op (op, a, b) ->
      Printf.sprintf "(%s %s %s)" (smt_op op) (smt_int_term a) (smt_int_term b)

  let rec smt_term smt_const = function
    | Var v -> var_name v
    | Lit n -> smt_lit n
    | Const c -> smt_const c
    | Apply (op, a, b) ->
      Printf.sprintf "(%s %s %s)" (smt_op op) (smt_term smt_const a)
        (smt_term smt_const b)

  let smt_lhs = smt_term var_name

  let smt_rhs = smt_term smt_int_term

  let smt_defined_shift amount =
    Printf.sprintf "(bvult %s %s)" amount (smt_lit (Nativeint.of_int word_bits))

  let rec smt_cond = function
    | True -> "true"
    | All conds ->
      Printf.sprintf "(and %s)" (String.concat " " (List.map smt_cond conds))
    | Eq (a, b) -> Printf.sprintf "(= %s %s)" (smt_int_term a) (smt_int_term b)
    | Slt (a, b) ->
      Printf.sprintf "(bvslt %s %s)" (smt_int_term a) (smt_int_term b)
    | Is_defined_shift a -> smt_defined_shift (smt_int_term a)

  let print_smt2_header ppf =
    Format.fprintf ppf
      "; Generated from Cmm_peephole_rules by\n\
       ; oxcaml/tests/backend/cmm_peephole/gen_smt.exe. Do not edit.\n\
       ; Every (check-sat) is expected to answer unsat.\n\
       (define-sort word () (_ BitVec %d))\n"
      word_bits

  let print_smt2 ppf t =
    Format.fprintf ppf "@\n(push)@\n(echo \"%s\")@\n" t.name;
    List.iter
      (fun (v : Cmm.expression pattern_var) ->
        Format.fprintf ppf "(declare-const %s word)@\n" (var_name v))
      t.expr_vars;
    List.iter
      (fun (v : Nativeint.t pattern_var) ->
        Format.fprintf ppf "(declare-const %s word)@\n" (var_name v))
      t.nat_vars;
    (match t.cond with
    | True -> ()
    | All _ | Eq _ | Slt _ | Is_defined_shift _ ->
      Format.fprintf ppf "(assert %s)@\n" (smt_cond t.cond));
    let equation = Printf.sprintf "(= %s %s)" (smt_lhs t.lhs) (smt_rhs t.rhs) in
    let goal =
      match shift_amounts [] t.rhs with
      | [] -> equation
      | amounts ->
        Printf.sprintf "(and %s %s)"
          (String.concat " "
             (List.map (fun a -> smt_defined_shift (smt_rhs a)) amounts))
          equation
    in
    Format.fprintf ppf
      "(assert (not %s))@\n(check-sat)@\n(echo \"\")@\n(pop)@\n" goal
end
