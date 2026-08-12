(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let fprintf = Format.fprintf

module Function_call = struct
  type t =
    | Direct of Code_id.t
    | Indirect_unknown_arity
    | Indirect_known_arity of Code_id.Set.t Or_unknown.t

  let print ppf call =
    match call with
    | Direct code_id ->
      fprintf ppf "@[<hov 1>(Direct %a)@]" Code_id.print code_id
    | Indirect_unknown_arity -> fprintf ppf "Indirect_unknown_arity"
    | Indirect_known_arity code_ids ->
      fprintf ppf "@[<hov 1>(Indirect_known_arity@ %a)@]"
        (Or_unknown.print Code_id.Set.print)
        code_ids
end

module Method_kind = struct
  type t =
    | Self
    | Public
    | Cached

  let print ppf t =
    match t with
    | Self -> fprintf ppf "Self"
    | Public -> fprintf ppf "Public"
    | Cached -> fprintf ppf "Cached"

  let from_lambda (kind : Lambda.meth_kind) =
    match kind with Self -> Self | Public -> Public | Cached -> Cached

  let to_lambda t : Lambda.meth_kind =
    match t with Self -> Self | Public -> Public | Cached -> Cached
end

module Effect = struct
  type t =
    | Perform of { eff : Simple.t }
    | Reperform of
        { eff : Simple.t;
          cont : Simple.t;
          last_fiber : Simple.t
        }
    | With_stack of
        { valuec : Simple.t;
          exnc : Simple.t;
          effc : Simple.t;
          f : Simple.t;
          arg : Simple.t
        }
    | With_stack_preemptible of
        { valuec : Simple.t;
          exnc : Simple.t;
          effc : Simple.t;
          handle_tick : Simple.t;
          f : Simple.t;
          arg : Simple.t
        }
    | Continue of
        { cont : Simple.t;
          value : Simple.t
        }
    | Discontinue of
        { cont : Simple.t;
          exn : Simple.t
        }
    | Discontinue_with_backtrace of
        { cont : Simple.t;
          exn : Simple.t;
          bt : Simple.t
        }

  let print ppf t =
    match t with
    | Perform { eff } ->
      fprintf ppf "@[<hov 1>(%tPerform%t@ %a)@]" Flambda_colours.effect_
        Flambda_colours.pop Simple.print eff
    | Reperform { eff; cont; last_fiber } ->
      fprintf ppf
        "@[<hov 1>(%tReperform%t@ (eff@ %a)@ (cont@ %a)@ (last_fiber@ %a))@]"
        Flambda_colours.effect_ Flambda_colours.pop Simple.print eff
        Simple.print cont Simple.print last_fiber
    | With_stack { valuec; exnc; effc; f; arg } ->
      fprintf ppf
        "@[<hov 1>(%tWith_stack%t (valuec@ %a)@ (exnc@ %a)@ (effc@ %a)@ (f@ \
         %a)@ (arg@ %a))@]"
        Flambda_colours.effect_ Flambda_colours.pop Simple.print valuec
        Simple.print exnc Simple.print effc Simple.print f Simple.print arg
    | With_stack_preemptible { valuec; exnc; effc; handle_tick; f; arg } ->
      fprintf ppf
        "@[<hov 1>(%tWith_stack_preemptible%t (valuec@ %a)@ (exnc@ %a)@ (effc@ \
         %a)@ (handle_tick@ %a)@ (f@ %a)@ (arg@ %a))@]"
        Flambda_colours.effect_ Flambda_colours.pop Simple.print valuec
        Simple.print exnc Simple.print effc Simple.print handle_tick
        Simple.print f Simple.print arg
    | Continue { cont; value } ->
      fprintf ppf "@[<hov 1>(%tContinue%t (cont@ %a)@ (value@ %a))@]"
        Flambda_colours.effect_ Flambda_colours.pop Simple.print cont
        Simple.print value
    | Discontinue { cont; exn } ->
      fprintf ppf "@[<hov 1>(%tDiscontinue%t (cont@ %a)@ (exn@ %a))@]"
        Flambda_colours.effect_ Flambda_colours.pop Simple.print cont
        Simple.print exn
    | Discontinue_with_backtrace { cont; exn; bt } ->
      fprintf ppf
        "@[<hov 1>(%tDiscontinue_with_backtrace%t (cont@ %a)@ (exn@ %a)@ (bt@ \
         %a))@]"
        Flambda_colours.effect_ Flambda_colours.pop Simple.print cont
        Simple.print exn Simple.print bt

  let perform ~eff = Perform { eff }

  let reperform ~eff ~cont ~last_fiber = Reperform { eff; cont; last_fiber }

  let with_stack ~valuec ~exnc ~effc ~f ~arg =
    With_stack { valuec; exnc; effc; f; arg }

  let with_stack_preemptible ~valuec ~exnc ~effc ~handle_tick ~f ~arg =
    With_stack_preemptible { valuec; exnc; effc; handle_tick; f; arg }

  let continue ~cont ~value = Continue { cont; value }

  let discontinue ~cont ~exn = Discontinue { cont; exn }

  let discontinue_with_backtrace ~cont ~exn ~bt =
    Discontinue_with_backtrace { cont; exn; bt }

  let free_names t =
    match t with
    | Perform { eff } -> Simple.free_names eff
    | Reperform { eff; cont; last_fiber } ->
      Name_occurrences.union (Simple.free_names eff)
        (Name_occurrences.union (Simple.free_names cont)
           (Simple.free_names last_fiber))
    | With_stack { valuec; exnc; effc; f; arg } ->
      Name_occurrences.union (Simple.free_names valuec)
        (Name_occurrences.union (Simple.free_names exnc)
           (Name_occurrences.union (Simple.free_names effc)
              (Name_occurrences.union (Simple.free_names f)
                 (Simple.free_names arg))))
    | With_stack_preemptible { valuec; exnc; effc; handle_tick; f; arg } ->
      Name_occurrences.union (Simple.free_names valuec)
        (Name_occurrences.union (Simple.free_names exnc)
           (Name_occurrences.union (Simple.free_names effc)
              (Name_occurrences.union
                 (Simple.free_names handle_tick)
                 (Name_occurrences.union (Simple.free_names f)
                    (Simple.free_names arg)))))
    | Continue { cont; value } ->
      Name_occurrences.union (Simple.free_names cont) (Simple.free_names value)
    | Discontinue { cont; exn } ->
      Name_occurrences.union (Simple.free_names cont) (Simple.free_names exn)
    | Discontinue_with_backtrace { cont; exn; bt } ->
      Name_occurrences.union (Simple.free_names cont)
        (Name_occurrences.union (Simple.free_names exn) (Simple.free_names bt))

  let apply_renaming t renaming =
    match t with
    | Perform { eff } ->
      let eff' = Simple.apply_renaming eff renaming in
      if eff == eff' then t else Perform { eff = eff' }
    | Reperform { eff; cont; last_fiber } ->
      let eff' = Simple.apply_renaming eff renaming in
      let cont' = Simple.apply_renaming cont renaming in
      let last_fiber' = Simple.apply_renaming last_fiber renaming in
      if eff == eff' && cont == cont' && last_fiber == last_fiber'
      then t
      else Reperform { eff = eff'; cont = cont'; last_fiber = last_fiber' }
    | With_stack { valuec; exnc; effc; f; arg } ->
      let valuec' = Simple.apply_renaming valuec renaming in
      let exnc' = Simple.apply_renaming exnc renaming in
      let effc' = Simple.apply_renaming effc renaming in
      let f' = Simple.apply_renaming f renaming in
      let arg' = Simple.apply_renaming arg renaming in
      if
        valuec == valuec' && exnc == exnc' && effc == effc' && f == f'
        && arg == arg'
      then t
      else
        With_stack
          { valuec = valuec'; exnc = exnc'; effc = effc'; f = f'; arg = arg' }
    | With_stack_preemptible { valuec; exnc; effc; handle_tick; f; arg } ->
      let valuec' = Simple.apply_renaming valuec renaming in
      let exnc' = Simple.apply_renaming exnc renaming in
      let effc' = Simple.apply_renaming effc renaming in
      let handle_tick' = Simple.apply_renaming handle_tick renaming in
      let f' = Simple.apply_renaming f renaming in
      let arg' = Simple.apply_renaming arg renaming in
      if
        valuec == valuec' && exnc == exnc' && effc == effc'
        && handle_tick == handle_tick'
        && f == f' && arg == arg'
      then t
      else
        With_stack_preemptible
          { valuec = valuec';
            exnc = exnc';
            effc = effc';
            handle_tick = handle_tick';
            f = f';
            arg = arg'
          }
    | Continue { cont; value } ->
      let cont' = Simple.apply_renaming cont renaming in
      let value' = Simple.apply_renaming value renaming in
      if cont == cont' && value == value'
      then t
      else Continue { cont = cont'; value = value' }
    | Discontinue { cont; exn } ->
      let cont' = Simple.apply_renaming cont renaming in
      let exn' = Simple.apply_renaming exn renaming in
      if cont == cont' && exn == exn'
      then t
      else Discontinue { cont = cont'; exn = exn' }
    | Discontinue_with_backtrace { cont; exn; bt } ->
      let cont' = Simple.apply_renaming cont renaming in
      let exn' = Simple.apply_renaming exn renaming in
      let bt' = Simple.apply_renaming bt renaming in
      if cont == cont' && exn == exn' && bt == bt'
      then t
      else Discontinue_with_backtrace { cont = cont'; exn = exn'; bt = bt' }

  let ids_for_export t =
    match t with
    | Perform { eff } -> Ids_for_export.from_simple eff
    | Reperform { eff; cont; last_fiber } ->
      Ids_for_export.union
        (Ids_for_export.from_simple eff)
        (Ids_for_export.union
           (Ids_for_export.from_simple cont)
           (Ids_for_export.from_simple last_fiber))
    | With_stack { valuec; exnc; effc; f; arg } ->
      Ids_for_export.union
        (Ids_for_export.from_simple valuec)
        (Ids_for_export.union
           (Ids_for_export.from_simple exnc)
           (Ids_for_export.union
              (Ids_for_export.from_simple effc)
              (Ids_for_export.union
                 (Ids_for_export.from_simple f)
                 (Ids_for_export.from_simple arg))))
    | With_stack_preemptible { valuec; exnc; effc; handle_tick; f; arg } ->
      Ids_for_export.union
        (Ids_for_export.from_simple valuec)
        (Ids_for_export.union
           (Ids_for_export.from_simple exnc)
           (Ids_for_export.union
              (Ids_for_export.from_simple effc)
              (Ids_for_export.union
                 (Ids_for_export.from_simple handle_tick)
                 (Ids_for_export.union
                    (Ids_for_export.from_simple f)
                    (Ids_for_export.from_simple arg)))))
    | Continue { cont; value } ->
      Ids_for_export.union
        (Ids_for_export.from_simple cont)
        (Ids_for_export.from_simple value)
    | Discontinue { cont; exn } ->
      Ids_for_export.union
        (Ids_for_export.from_simple cont)
        (Ids_for_export.from_simple exn)
    | Discontinue_with_backtrace { cont; exn; bt } ->
      Ids_for_export.union
        (Ids_for_export.from_simple cont)
        (Ids_for_export.union
           (Ids_for_export.from_simple exn)
           (Ids_for_export.from_simple bt))
end

type t =
  | Function of { function_call : Function_call.t }
  | Method of
      { kind : Method_kind.t;
        obj : Simple.t
      }
  | C_call of
      { needs_caml_c_call : bool;
        is_c_builtin : bool;
        effects : Effects.t;
        coeffects : Coeffects.t
      }
  | Effect of Effect.t

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Function { function_call; } ->
    fprintf ppf "@[<hov 1>(Function@ \
        @[<hov 1>(function_call@ %a)@]\
        )@]"
      Function_call.print function_call
  | Method { kind; obj } ->
    fprintf ppf "@[<hov 1>(Method@ \
        @[<hov 1>(obj@ %a)@]@ \
        @[<hov 1>(kind@ %a)@]\
        )@]"
      Simple.print obj
      Method_kind.print kind
  | C_call { needs_caml_c_call; is_c_builtin; effects; coeffects } ->
    fprintf ppf "@[<hov 1>(C@ \
        @[<hov 1>(needs_caml_c_call@ %b)@]@ \
        @[<hov 1>(is_c_builtin@ %b)@]@ \
        @[<hov 1>(effects@ %a)@]@ \
        @[<hov 1>(coeffects@ %a)@]\
        )@]"
      needs_caml_c_call
      is_c_builtin
      Effects.print effects
      Coeffects.print coeffects
  | Effect effect_op -> Effect.print ppf effect_op

let direct_function_call code_id = Function { function_call = Direct code_id }

let indirect_function_call_unknown_arity =
  Function { function_call = Indirect_unknown_arity }

let indirect_function_call_known_arity ~code_ids =
  Function { function_call = Indirect_known_arity code_ids }

let method_call kind ~obj = Method { kind; obj }

let c_call ~needs_caml_c_call ~is_c_builtin ~effects ~coeffects =
  C_call { needs_caml_c_call; is_c_builtin; effects; coeffects }

let effect_ eff = Effect eff

let free_names t =
  match t with
  | Function { function_call = Direct code_id } ->
    Name_occurrences.add_code_id Name_occurrences.empty code_id Name_mode.normal
  | Function { function_call = Indirect_known_arity (Known code_ids) } ->
    Code_id.Set.fold
      (fun code_id free_names ->
        Name_occurrences.add_code_id free_names code_id Name_mode.normal)
      code_ids Name_occurrences.empty
  | Function { function_call = Indirect_unknown_arity }
  | Function { function_call = Indirect_known_arity Unknown } ->
    Name_occurrences.empty
  | C_call
      { needs_caml_c_call = _; is_c_builtin = _; effects = _; coeffects = _ } ->
    Name_occurrences.empty
  | Method { kind = _; obj } -> Simple.free_names obj
  | Effect op -> Effect.free_names op

let apply_renaming t renaming =
  match t with
  | Function { function_call = Direct code_id } ->
    let code_id' = Renaming.apply_code_id renaming code_id in
    if code_id == code_id'
    then t
    else Function { function_call = Direct code_id' }
  | Function { function_call = Indirect_known_arity (Known code_ids) } ->
    let code_ids' =
      Code_id.Set.map (Renaming.apply_code_id renaming) code_ids
    in
    if Code_id.Set.equal code_ids code_ids'
    then t
    else Function { function_call = Indirect_known_arity (Known code_ids') }
  | Function
      { function_call = Indirect_unknown_arity | Indirect_known_arity Unknown }
    ->
    t
  | C_call
      { needs_caml_c_call = _; is_c_builtin = _; effects = _; coeffects = _ } ->
    t
  | Method { kind; obj } ->
    let obj' = Simple.apply_renaming obj renaming in
    if obj == obj' then t else Method { kind; obj = obj' }
  | Effect op ->
    let op' = Effect.apply_renaming op renaming in
    if op == op' then t else Effect op'

let ids_for_export t =
  match t with
  | Function { function_call = Direct code_id } ->
    Ids_for_export.add_code_id Ids_for_export.empty code_id
  | Function { function_call = Indirect_known_arity (Known code_ids) } ->
    Code_id.Set.fold
      (fun code_id ids_for_export ->
        Ids_for_export.add_code_id ids_for_export code_id)
      code_ids Ids_for_export.empty
  | Function { function_call = Indirect_unknown_arity }
  | Function { function_call = Indirect_known_arity Unknown } ->
    Ids_for_export.empty
  | C_call
      { needs_caml_c_call = _; is_c_builtin = _; effects = _; coeffects = _ } ->
    Ids_for_export.empty
  | Method { kind = _; obj } -> Ids_for_export.from_simple obj
  | Effect op -> Effect.ids_for_export op
