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

module Result_continuation = struct
  type t =
    | Return of Continuation.t
    | Never_returns

  let [@ocamlformat "disable"] print fmt = function
    | Return k -> Continuation.print fmt k
    | Never_returns -> Format.fprintf fmt "∅"
end

module Return = struct
  type t =
    | Returns_to of
        { cont : Continuation.t;
          arity : [`Unarized] Flambda_arity.t
        }
    | Tail_forwards_to_caller of Continuation.t
    | Never_returns of { arity : Result_arity.t }

  let equal t1 t2 =
    match t1, t2 with
    | ( Returns_to { cont = cont1; arity = arity1 },
        Returns_to { cont = cont2; arity = arity2 } ) ->
      Continuation.equal cont1 cont2 && Flambda_arity.equal_exact arity1 arity2
    | Tail_forwards_to_caller cont1, Tail_forwards_to_caller cont2 ->
      Continuation.equal cont1 cont2
    | Never_returns { arity = arity1 }, Never_returns { arity = arity2 } ->
      Result_arity.equal_exact arity1 arity2
    | Returns_to _, (Tail_forwards_to_caller _ | Never_returns _)
    | Tail_forwards_to_caller _, (Returns_to _ | Never_returns _)
    | Never_returns _, (Returns_to _ | Tail_forwards_to_caller _) ->
      false

  let free_names = function
    | Returns_to { cont; arity = _ } | Tail_forwards_to_caller cont ->
      Name_occurrences.singleton_continuation cont
    | Never_returns _ -> Name_occurrences.empty

  let apply_renaming t renaming =
    match t with
    | Returns_to { cont; arity } ->
      let cont' = Renaming.apply_continuation renaming cont in
      if cont == cont' then t else Returns_to { cont = cont'; arity }
    | Tail_forwards_to_caller cont ->
      let cont' = Renaming.apply_continuation renaming cont in
      if cont == cont' then t else Tail_forwards_to_caller cont'
    | Never_returns _ -> t

  let ids_for_export = function
    | Returns_to { cont; arity = _ } | Tail_forwards_to_caller cont ->
      Ids_for_export.singleton_continuation cont
    | Never_returns _ -> Ids_for_export.empty

  let create (continuation : Result_continuation.t) (arity : Result_arity.t) =
    match continuation, arity with
    | Return cont, Ok arity -> Returns_to { cont; arity }
    | Return cont, Unknown -> Tail_forwards_to_caller cont
    | Return _, Bottom -> Never_returns { arity = Bottom }
    | Never_returns, ((Ok _ | Unknown | Bottom) as arity) ->
      Never_returns { arity }

  let continuation t : Result_continuation.t =
    match t with
    | Returns_to { cont; arity = _ } | Tail_forwards_to_caller cont ->
      Return cont
    | Never_returns _ -> Never_returns

  let arity t : Result_arity.t =
    match t with
    | Returns_to { cont = _; arity } -> Ok arity
    | Tail_forwards_to_caller _ -> Unknown
    | Never_returns { arity } -> arity

  let with_continuation t cont =
    match t with
    | Returns_to { cont = _; arity } -> Returns_to { cont; arity }
    | Tail_forwards_to_caller _ -> Tail_forwards_to_caller cont
    | Never_returns _ -> t
end

module Position = struct
  type t =
    | Normal
    | Nontail

  let equal t1 t2 =
    match t1, t2 with
    | Normal, Normal -> true
    | Normal, Nontail -> false
    | Nontail, Normal -> false
    | Nontail, Nontail -> true
end

type t =
  { callee : Simple.t option;
    return : Return.t;
    exn_continuation : Exn_continuation.t;
    args : Simple.t list;
    args_arity : [`Complex] Flambda_arity.t;
    call_kind : Call_kind.t;
    return_mode : Alloc_mode.For_applications.t;
    dbg : Debuginfo.t;
    inlined : Inlined_attribute.t;
    inlining_state : Inlining_state.t;
    probe : Probe.t;
    position : Position.t;
    relative_history : Inlining_history.Relative.t
  }

let [@ocamlformat "disable"] print_inlining_paths ppf relative_history =
  if !Oxcaml_flags.dump_inlining_paths then
    Format.fprintf ppf "@[<hov 1>(relative_history@ %a)@]@ "
      Inlining_history.Relative.print relative_history

let [@ocamlformat "disable"] print_normal ppf
    { callee; return; exn_continuation; args; args_arity;
      call_kind; return_mode; dbg; inlined; inlining_state; probe;
      position; relative_history } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(%a\u{3008}%a\u{3009}\u{300a}%a\u{300b}\
      (%a))@]@ \
      @[<hov 1>(args_arity@ %a)@]@ \
      @[<hov 1>(return_arity@ %a)@]@ \
      @[<hov 1>(call_kind@ %a)@]@ \
      @[<hov 1>(return_mode@ %a)@]@ \
      @[<hov 1>%t(dbg@ %a)%t@]@ \
      @[<hov 1>(inline@ %a)@]@ \
      @[<hov 1>(inlining_state@ %a)@]@ \
      %a\
      @[<hov 1>(probe@ %a)@]@ \
      @[<hov 1>(position@ %a)@]\
      )@]"
    (Misc.Stdlib.Option.print Simple.print) callee
    Result_continuation.print (Return.continuation return)
    Exn_continuation.print exn_continuation
    Simple.List.print args
    Flambda_arity.print args_arity
    Result_arity.print (Return.arity return)
    Call_kind.print call_kind
    Alloc_mode.For_applications.print return_mode
    Flambda_colours.debuginfo
    Debuginfo.print_compact dbg
    Flambda_colours.pop
    Inlined_attribute.print inlined
    Inlining_state.print inlining_state
    print_inlining_paths relative_history
    Probe.print probe
    (fun ppf position ->
       match position with
       | Position.Normal -> Format.pp_print_string ppf "Normal"
       | Position.Nontail -> Format.pp_print_string ppf "Nontail")
    position

let [@ocamlformat "disable"] print_effect ppf
    { callee = _; return; exn_continuation; args = _; args_arity = _;
      call_kind; return_mode; dbg; inlined = _; inlining_state = _;
      probe = _; position; relative_history = _ } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>%a@]@ \
      @[<hov 1>(return_mode %a)@]@ \
      @[<hov 1>\u{3008}%a\u{3009}\u{300a}%a\u{300b}@]@ \
      @[<hov 1>%t(dbg@ %a)%t@]@ \
      @[<hov 1>(position@ %a)@]\
      )@]"
    Call_kind.print call_kind
    Alloc_mode.For_applications.print return_mode
    Result_continuation.print (Return.continuation return)
    Exn_continuation.print exn_continuation
    Flambda_colours.debuginfo
    Debuginfo.print_compact dbg
    Flambda_colours.pop
    (fun ppf position ->
       match position with
       | Position.Normal -> Format.pp_print_string ppf "Normal"
       | Position.Nontail -> Format.pp_print_string ppf "Nontail")
    position

let print ppf t =
  match t.call_kind with
  | Function _ | Method _ | C_call _ -> print_normal ppf t
  | Effect _ -> print_effect ppf t

let invariant
    ({ callee;
       return;
       exn_continuation = _;
       args;
       args_arity;
       call_kind;
       return_mode = _;
       dbg = _;
       inlined = _;
       inlining_state = _;
       probe = _;
       position = _;
       relative_history = _
     } as t) =
  (match callee with
  | Some _ -> ()
  | None -> (
    match[@ocaml.warning "-fragile-match"] call_kind with
    | Function { function_call = Direct _; _ } | Effect _ -> ()
    | _ -> Misc.fatal_errorf "Missing callee:@ %a" print t));
  (match call_kind with
  | Function _ | Method _ -> ()
  | C_call _ -> (
    (match callee with
    | Some callee when Simple.is_symbol callee -> ()
    | None | Some _ ->
      (* CR-someday mshinwell: We could expose indirect C calls at the source
         language level. *)
      Misc.fatal_errorf
        "For [C_call] applications the callee must be directly specified as a \
         [Symbol]:@ %a"
        print t);
    match Return.arity return with
    | Ok arity -> (
      match Flambda_arity.unarized_components arity with
      | [] | [_] | [_; _] ->
        (* CR xclerc: we currently support only pairs as unboxed return
           values. *)
        ()
      | _ :: _ :: _ ->
        Misc.fatal_errorf "Illegal return arity for C call:@ %a"
          Flambda_arity.print arity)
    | Unknown | Bottom ->
      Misc.fatal_errorf "Illegal unknown/bottom return arity for C call:@ %a"
        print t)
  | Effect _ -> (
    match callee, args with
    | None, [] -> ()
    | Some _, [] | (None | Some _), _ :: _ ->
      Misc.fatal_errorf
        "Algebraic effect operations in [Apply_expr] must have no callee and \
         no arguments; all data are specified in the [Call_kind]:@ %a"
        print t));
  if List.compare_lengths args (Flambda_arity.unarize args_arity) <> 0
  then
    Misc.fatal_errorf
      "Length of argument and arity lists disagree in [Apply]:@ %a" print t

let create ~callee ~return exn_continuation ~args ~args_arity
    ~(call_kind : Call_kind.t) ~return_mode dbg ~inlined ~inlining_state ~probe
    ~position ~relative_history =
  let t =
    { callee;
      return;
      exn_continuation;
      args;
      args_arity;
      call_kind;
      return_mode;
      dbg;
      inlined;
      inlining_state;
      probe;
      position;
      relative_history
    }
  in
  invariant t;
  t

let callee t = t.callee

let return t = t.return

let continuation t = Return.continuation t.return

let exn_continuation t = t.exn_continuation

let args t = t.args

let call_kind t = t.call_kind

let return_mode t = t.return_mode

let dbg t = t.dbg

let inlined t = t.inlined

let inlining_state t = t.inlining_state

let relative_history t = t.relative_history

let position t = t.position

let free_names_without_exn_continuation
    { callee;
      return;
      exn_continuation = _;
      args;
      args_arity = _;
      call_kind;
      return_mode;
      dbg = _;
      inlined = _;
      inlining_state = _;
      probe = _;
      position = _;
      relative_history = _
    } =
  Name_occurrences.union_list
    [ (match callee with
      | None -> Name_occurrences.empty
      | Some callee -> Simple.free_names callee);
      Return.free_names return;
      Simple.List.free_names args;
      Call_kind.free_names call_kind;
      Alloc_mode.For_applications.free_names return_mode ]

let free_names_except_callee
    { callee = _;
      return;
      exn_continuation;
      args;
      args_arity = _;
      call_kind;
      return_mode;
      dbg = _;
      inlined = _;
      inlining_state = _;
      probe = _;
      position = _;
      relative_history = _
    } =
  Name_occurrences.union_list
    [ Return.free_names return;
      Exn_continuation.free_names exn_continuation;
      Simple.List.free_names args;
      Call_kind.free_names call_kind;
      Alloc_mode.For_applications.free_names return_mode ]

let free_names t =
  Name_occurrences.union
    (match t.callee with
    | None -> Name_occurrences.empty
    | Some callee -> Simple.free_names callee)
    (free_names_except_callee t)

let apply_renaming
    ({ callee;
       return;
       exn_continuation;
       args;
       args_arity;
       call_kind;
       return_mode;
       dbg;
       inlined;
       inlining_state;
       probe;
       position;
       relative_history
     } as t) renaming =
  let return' = Return.apply_renaming return renaming in
  let exn_continuation' =
    Exn_continuation.apply_renaming exn_continuation renaming
  in
  let callee' =
    match callee with
    | None -> None
    | Some orig_callee ->
      let new_callee = Simple.apply_renaming orig_callee renaming in
      if orig_callee == new_callee then callee else Some new_callee
  in
  let args' = Simple.List.apply_renaming args renaming in
  let call_kind' = Call_kind.apply_renaming call_kind renaming in
  let return_mode' =
    Alloc_mode.For_applications.apply_renaming return_mode renaming
  in
  if
    return == return'
    && exn_continuation == exn_continuation'
    && callee == callee' && args == args' && call_kind == call_kind'
    && return_mode == return_mode'
  then t
  else
    { callee = callee';
      return = return';
      exn_continuation = exn_continuation';
      args = args';
      args_arity;
      call_kind = call_kind';
      return_mode = return_mode';
      dbg;
      inlined;
      inlining_state;
      probe;
      position;
      relative_history
    }

let ids_for_export
    { callee;
      return;
      exn_continuation;
      args;
      args_arity = _;
      call_kind;
      return_mode;
      dbg = _;
      inlined = _;
      inlining_state = _;
      probe = _;
      position = _;
      relative_history = _
    } =
  let callee_ids =
    match callee with
    | None -> Ids_for_export.empty
    | Some callee -> Ids_for_export.from_simple callee
  in
  let callee_and_args_ids =
    List.fold_left
      (fun ids arg -> Ids_for_export.add_simple ids arg)
      callee_ids args
  in
  let call_kind_ids = Call_kind.ids_for_export call_kind in
  let alloc_mode_ids = Alloc_mode.For_applications.ids_for_export return_mode in
  let result_continuation_ids = Return.ids_for_export return in
  let exn_continuation_ids = Exn_continuation.ids_for_export exn_continuation in
  Ids_for_export.union
    (Ids_for_export.union callee_and_args_ids
       (Ids_for_export.union call_kind_ids alloc_mode_ids))
    (Ids_for_export.union result_continuation_ids exn_continuation_ids)

let erase_callee t = { t with callee = None }

let with_return t return =
  let t = { t with return } in
  invariant t;
  t

let with_return_and_exn_continuation t return exn_continuation =
  let t = { t with return; exn_continuation } in
  invariant t;
  t

let with_exn_continuation t exn_continuation = { t with exn_continuation }

let with_call_kind t call_kind =
  let t = { t with call_kind } in
  invariant t;
  t

let with_args t args ~args_arity = { t with args; args_arity }

let inlining_arguments t = inlining_state t |> Inlining_state.arguments

let probe t = t.probe

let returns t =
  match t.return with
  | Returns_to _ | Tail_forwards_to_caller _ -> true
  | Never_returns _ -> false

let args_arity t = t.args_arity

let return_arity t = Return.arity t.return

let with_inlined_attribute t inlined = { t with inlined }
