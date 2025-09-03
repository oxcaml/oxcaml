(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Env = To_cmm_env
module P = Flambda_primitive
module FV = To_cmm_free_vars

let simple env res s =
  Simple.pattern_match' s
    ~symbol:(fun sym ~coercion:_ ->
      let sym_name = Symbol.linkage_name_as_string sym in
      let expr = Some (Cmm.Cphantom_const_symbol sym_name) in
      expr, env, res, FV.empty)
    ~const:(fun const ->
      match[@warning "-4"] Reg_width_const.descr const with
      | Tagged_immediate i ->
        let machine_width = Target_system.Machine_width.Sixty_four in
        let targetint_32_64 = Target_ocaml_int.to_targetint machine_width i in
        let targetint =
          Targetint.of_int64 (Targetint_32_64.to_int64 targetint_32_64)
        in
        let expr = Some (Cmm.Cphantom_const_int targetint) in
        expr, env, res, FV.empty
      | _ -> None, env, res, FV.empty)
    ~var:(fun v ~coercion:_ ->
      let Env.{ env; res; var = cmm_var_opt } =
        Env.get_variable_for_phantom_expr env res v
      in
      let expr, fv =
        match cmm_var_opt with
        | Some backend_var ->
          ( Some (Cmm.Cphantom_var backend_var),
            FV.add ~mode:Phantom backend_var FV.empty )
        | None -> None, FV.empty
      in
      expr, env, res, fv)

let simple_block_field env res s =
  Simple.pattern_match' s
    ~symbol:(fun _ ~coercion:_ -> env, res, None)
    ~const:(fun _ -> env, res, None)
    ~var:(fun v ~coercion:_ ->
      let Env.{ env; res; var = cmm_var_opt } =
        Env.get_variable_for_phantom_expr env res v
      in
      env, res, cmm_var_opt)

(* Contrary to what we need to do in {To_cmm_primitive}, the order of
   environment lookups here should not matter *)
let rec block_field_list env res = function
  | [] -> env, res, Some [], FV.empty
  | s :: args -> (
    let env, res, cmm_var_opt = simple_block_field env res s in
    match cmm_var_opt with
    | None -> env, res, None, FV.empty
    | Some v ->
      let env, res, r, fv = block_field_list env res args in
      let ret, fv =
        match r with
        | None -> None, FV.empty
        | Some l -> Some (v :: l), FV.add ~mode:Phantom v fv
      in
      env, res, ret, fv)

let prim env res _dbg p =
  match[@warning "-4"] (p : P.t) with
  | Unary (Block_load { kind = _; mut = _; field }, arg) -> (
    let env, res, var_opt = simple_block_field env res arg in
    match var_opt with
    | None -> None, env, res, FV.empty
    | Some var ->
      let fv = FV.add ~mode:Phantom var FV.empty in
      let field =
        Targetint_32_64.to_int_checked Target_system.Machine_width.Sixty_four
          (Target_ocaml_int.to_targetint Target_system.Machine_width.Sixty_four
             field)
      in
      let expr = Some (Cmm.Cphantom_read_field { var; field }) in
      expr, env, res, fv)
  | Variadic (Make_block (block_kind, _mut, _alloc_mode), args) ->
    let tag =
      match (block_kind : P.Block_kind.t) with
      | Values (tag, _) -> Tag.Scannable.to_int tag
      | Naked_floats -> Tag.to_int Tag.double_array_tag
      | Mixed (tag, _) -> Tag.Scannable.to_int tag
    in
    let env, res, args, fv = block_field_list env res args in
    let expr =
      match args with
      | None -> None
      | Some fields -> Some (Cmm.Cphantom_block { tag; fields })
    in
    expr, env, res, fv
  | _ -> None, env, res, FV.empty
