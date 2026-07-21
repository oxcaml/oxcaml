(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Env = To_cmm_env
module EO = Exported_offsets
module FV = To_cmm_free_vars
module P = Flambda_primitive

let simple env res s =
  Simple.pattern_match' s
    ~symbol:(fun sym ~coercion:_ ->
      let cmm_sym = To_cmm_result.symbol res sym in
      let symbol_usable_for_phantom_let =
        match cmm_sym.sym_global with
        | Global -> true
        | Local ->
          (* The data for a local symbol may never be emitted, for example if
             the corresponding lifted constant was deduplicated against a
             structurally-equal one. (References to such symbols from normal
             code will have been simplified away, but references from phantom
             lets may remain.) Such symbols cannot be referenced from DWARF:
             local symbols are referenced via assembler-temporary labels, which
             must be defined. *)
          To_cmm_result.data_symbol_is_defined res cmm_sym
      in
      let expr =
        if symbol_usable_for_phantom_let
        then Some (Cmm.Cphantom_const_symbol cmm_sym)
        else None
      in
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
            FV.singleton ~mode:Phantom backend_var )
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

(* Contrary to what we need to do in [To_cmm_primitive], the order of
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
      let fv = FV.singleton ~mode:Phantom var in
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
  | Unary (Project_value_slot { project_from; value_slot }, arg) -> (
    let env, res, var_opt = simple_block_field env res arg in
    match var_opt with
    | None -> None, env, res, FV.empty
    | Some var -> (
      match
        ( EO.value_slot_offset (Env.exported_offsets env) value_slot,
          EO.function_slot_offset (Env.exported_offsets env) project_from )
      with
      | ( Some (EO.Live_value_slot { offset; _ }),
          Some (EO.Live_function_slot { offset = base; _ }) ) ->
        let fv = FV.singleton ~mode:Phantom var in
        let expr =
          Some (Cmm.Cphantom_read_field { var; field = offset - base })
        in
        expr, env, res, fv
      | _ ->
        (* The slot may have been removed from the (optimised-out) closure; the
           projected variable is then itself presented as optimised out. *)
        None, env, res, FV.empty))
  | Unary (Project_function_slot { move_from; move_to }, arg) -> (
    let env, res, var_opt = simple_block_field env res arg in
    match var_opt with
    | None -> None, env, res, FV.empty
    | Some var -> (
      match
        ( EO.function_slot_offset (Env.exported_offsets env) move_from,
          EO.function_slot_offset (Env.exported_offsets env) move_to )
      with
      | ( Some (EO.Live_function_slot { offset = c1; _ }),
          Some (EO.Live_function_slot { offset = c2; _ }) ) ->
        let fv = FV.singleton ~mode:Phantom var in
        let expr =
          Some (Cmm.Cphantom_offset_var { var; offset_in_words = c2 - c1 })
        in
        expr, env, res, fv
      | _ -> None, env, res, FV.empty))
  | Nullary (Optimised_out _) ->
    (* The variable is bound to a phantom let with no defining expression. This
       still materialises a backend variable for it, so that any subsequent
       phantom lets reading from it -- for example a [Project_value_slot] or
       [Project_function_slot] from an optimised-out [my_closure] -- can refer
       to it rather than being dropped. *)
    None, env, res, FV.empty
  | _ -> None, env, res, FV.empty
