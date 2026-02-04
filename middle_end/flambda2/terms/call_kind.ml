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
    | With_stack_bind of
        { valuec : Simple.t;
          exnc : Simple.t;
          effc : Simple.t;
          dyn : Simple.t;
          bind : Simple.t;
          f : Simple.t;
          arg : Simple.t
        }
    | Resume of
        { cont : Simple.t;
          f : Simple.t;
          arg : Simple.t
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
    | With_stack_bind { valuec; exnc; effc; dyn; bind; f; arg } ->
      fprintf ppf
        "@[<hov 1>(%tWith_stack_bind%t (valuec@ %a)@ (exnc@ %a)@ (effc@ %a)@ \
         (dyn@ %a)@ (bind@ %a)@ (f@ %a)@ (arg@ %a))@]"
        Flambda_colours.effect_ Flambda_colours.pop Simple.print valuec
        Simple.print exnc Simple.print effc Simple.print dyn Simple.print bind
        Simple.print f Simple.print arg
    | Resume { cont; f; arg } ->
      fprintf ppf "@[<hov 1>(%tResume%t (cont@ %a)@ (f@ %a)@ (arg@ %a))@]"
        Flambda_colours.effect_ Flambda_colours.pop Simple.print cont
        Simple.print f Simple.print arg

  let perform ~eff = Perform { eff }

  let reperform ~eff ~cont ~last_fiber = Reperform { eff; cont; last_fiber }

  let with_stack ~valuec ~exnc ~effc ~f ~arg =
    With_stack { valuec; exnc; effc; f; arg }

  let with_stack_bind ~valuec ~exnc ~effc ~dyn ~bind ~f ~arg =
    With_stack_bind { valuec; exnc; effc; dyn; bind; f; arg }

  let resume ~cont ~f ~arg = Resume { cont; f; arg }

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
    | With_stack_bind { valuec; exnc; effc; dyn; bind; f; arg } ->
      Name_occurrences.union (Simple.free_names valuec)
        (Name_occurrences.union (Simple.free_names exnc)
           (Name_occurrences.union (Simple.free_names effc)
              (Name_occurrences.union (Simple.free_names dyn)
                 (Name_occurrences.union (Simple.free_names bind)
                    (Name_occurrences.union (Simple.free_names f)
                       (Simple.free_names arg))))))
    | Resume { cont; f; arg } ->
      Name_occurrences.union (Simple.free_names cont)
        (Name_occurrences.union (Simple.free_names f) (Simple.free_names arg))

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
    | With_stack_bind { valuec; exnc; effc; dyn; bind; f; arg } ->
      let valuec' = Simple.apply_renaming valuec renaming in
      let exnc' = Simple.apply_renaming exnc renaming in
      let effc' = Simple.apply_renaming effc renaming in
      let dyn' = Simple.apply_renaming dyn renaming in
      let bind' = Simple.apply_renaming bind renaming in
      let f' = Simple.apply_renaming f renaming in
      let arg' = Simple.apply_renaming arg renaming in
      if
        valuec == valuec' && exnc == exnc' && effc == effc' && dyn == dyn'
        && bind == bind' && f == f' && arg == arg'
      then t
      else
        With_stack_bind
          { valuec = valuec';
            exnc = exnc';
            effc = effc';
            dyn = dyn';
            bind = bind';
            f = f';
            arg = arg'
          }
    | Resume { cont; f; arg } ->
      let cont' = Simple.apply_renaming cont renaming in
      let f' = Simple.apply_renaming f renaming in
      let arg' = Simple.apply_renaming arg renaming in
      if cont == cont' && f == f' && arg == arg'
      then t
      else Resume { cont = cont'; f = f'; arg = arg' }

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
    | With_stack_bind { valuec; exnc; effc; dyn; bind; f; arg } ->
      Ids_for_export.union
        (Ids_for_export.from_simple valuec)
        (Ids_for_export.union
           (Ids_for_export.from_simple exnc)
           (Ids_for_export.union
              (Ids_for_export.from_simple effc)
              (Ids_for_export.union
                 (Ids_for_export.from_simple dyn)
                 (Ids_for_export.union
                    (Ids_for_export.from_simple bind)
                    (Ids_for_export.union
                       (Ids_for_export.from_simple f)
                       (Ids_for_export.from_simple arg))))))
    | Resume { cont; f; arg } ->
      Ids_for_export.union
        (Ids_for_export.from_simple cont)
        (Ids_for_export.union
           (Ids_for_export.from_simple f)
           (Ids_for_export.from_simple arg))
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
