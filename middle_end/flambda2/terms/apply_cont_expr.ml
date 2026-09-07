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

type t =
  { k : Continuation.t;
    args : Simple.t list;
    trap_action : Trap_action.Option.t;
    check_actions : Check_action.t list;
    dbg : Debuginfo.t
  }

let print_or_elide_debuginfo ppf dbg =
  if Debuginfo.is_none dbg
  then Format.pp_print_string ppf ""
  else (
    Format.pp_print_string ppf " ";
    Debuginfo.print_compact ppf dbg)

include Container_types.Make (struct
  type nonrec t = t

  let [@ocamlformat "disable"] print ppf
      { k; args; check_actions; trap_action; dbg; } =
    let name, trap_action =
      match Continuation.sort k, trap_action, args with
      | Normal_or_exn, None, [] -> "goto", None
      | Normal_or_exn, None, _::_ -> "apply_cont", None
      | Normal_or_exn, Some (Push _ ), [] ->
        "goto", trap_action
      | Normal_or_exn, Some (Push _ ), _::_ ->
        "apply_cont", trap_action
      | Normal_or_exn, Some (Pop { exn_handler; _ }), _ ->
        if Continuation.equal k exn_handler then "raise", trap_action
        else
          begin match args with
          | [] -> "goto", trap_action
          | _::_ -> "apply_cont", trap_action
          end
      | Return, None, [] -> "return", None
      | Return, None, _::_ -> "return", None
      | Return, Some trap_action, [] -> "return", Some trap_action
      | Return, Some trap_action, _::_ -> "return", Some trap_action
      | Define_root_symbol, None, [] ->
        "apply_cont", None
      | Define_root_symbol, None, _::_ ->
        "apply_cont", None
      | Define_root_symbol, Some trap_action, [] ->
        "apply_cont", Some trap_action
      | Define_root_symbol, Some trap_action, _::_ ->
        "apply_cont", Some trap_action
      | Toplevel_return, None, [] ->
        "module_init_end", None
      | Toplevel_return, None, _::_ ->
        "module_init_end", None
      | Toplevel_return, Some trap_action, [] ->
        "module_init_end", Some trap_action
      | Toplevel_return, Some trap_action, _::_ ->
        "module_init_end", Some trap_action
    in
    let print_check_actions ppf check_actions =
      List.iter
        (fun check_action ->
          Format.fprintf ppf "%a@ %tthen%t@ " Check_action.print check_action
            Flambda_colours.expr_keyword Flambda_colours.pop)
        check_actions
    in
    Format.fprintf ppf "@[<hov 1>%a%a%t%s%t %a"
      print_check_actions check_actions
      Trap_action.Option.print trap_action
      Flambda_colours.expr_keyword
      name
      Flambda_colours.pop
      Continuation.print k;
    begin match args with
    | [] -> ()
    | args ->
      Format.fprintf ppf " %a" Simple.List.print args
    end;
    Format.fprintf ppf "%t%a%t@]"
      Flambda_colours.elide
      print_or_elide_debuginfo dbg
      Flambda_colours.pop

  let hash _ = Misc.fatal_error "Not yet implemented"

  let compare
      { k = k1;
        args = args1;
        check_actions = check_actions1;
        trap_action = trap_action1;
        dbg = dbg1
      }
      { k = k2;
        args = args2;
        check_actions = check_actions2;
        trap_action = trap_action2;
        dbg = dbg2
      } =
    let c = Continuation.compare k1 k2 in
    if c <> 0
    then c
    else
      let c = Misc.Stdlib.List.compare Simple.compare args1 args2 in
      if c <> 0
      then c
      else
        let c =
          Misc.Stdlib.List.compare Check_action.compare check_actions1
            check_actions2
        in
        if c <> 0
        then c
        else
          let c =
            Option.compare Trap_action.compare trap_action1 trap_action2
          in
          if c <> 0 then c else Debuginfo.compare dbg1 dbg2

  let equal t1 t2 = compare t1 t2 = 0
end)

(* CR mshinwell: Check the sort of [k]. *)
let create ?trap_action ?(check_actions = []) k ~args ~dbg =
  { k; args; trap_action; check_actions; dbg }

let goto k =
  { k; args = []; trap_action = None; check_actions = []; dbg = Debuginfo.none }

let continuation t = t.k

let args t = t.args

let trap_action t = t.trap_action

let check_actions t = t.check_actions

let debuginfo t = t.dbg

let free_names { k; args; trap_action; check_actions; dbg = _ } =
  let default = Simple.List.free_names args in
  let default =
    List.fold_left
      (fun names check_action ->
        Name_occurrences.union names (Check_action.free_names check_action))
      default check_actions
  in
  match trap_action with
  | None -> Name_occurrences.add_continuation default k ~has_traps:false
  | Some trap_action ->
    Name_occurrences.add_continuation
      (Name_occurrences.union default (Trap_action.free_names trap_action))
      k ~has_traps:true

let apply_renaming ({ k; args; trap_action; check_actions; dbg } as t) renaming
    =
  let k' = Renaming.apply_continuation renaming k in
  let args' = Simple.List.apply_renaming args renaming in
  let trap_action' = Trap_action.Option.apply_renaming trap_action renaming in
  let check_actions' =
    Misc.Stdlib.List.map_sharing
      (fun check_action -> Check_action.apply_renaming check_action renaming)
      check_actions
  in
  if
    k == k' && args == args'
    && trap_action == trap_action'
    && check_actions == check_actions'
  then t
  else
    { k = k';
      args = args';
      trap_action = trap_action';
      check_actions = check_actions';
      dbg
    }

let ids_for_export { k; args; trap_action; check_actions; dbg = _ } =
  let ids =
    List.fold_left
      (fun ids arg -> Ids_for_export.add_simple ids arg)
      (Ids_for_export.add_continuation
         (Trap_action.Option.ids_for_export trap_action)
         k)
      args
  in
  List.fold_left
    (fun ids check_action ->
      Ids_for_export.union ids (Check_action.ids_for_export check_action))
    ids check_actions

let with_continuation t continuation = { t with k = continuation }

let with_continuation_and_args t cont ~args =
  if Continuation.equal t.k cont && args == t.args
  then t
  else { t with k = cont; args }

let update_args t ~args = if args == t.args then t else { t with args }

let with_check_actions t check_actions =
  (* Physical equality suffices to preserve sharing in the common case where
     both lists are empty. *)
  if t.check_actions == check_actions then t else { t with check_actions }

let add_check_actions t check_actions =
  if List.is_empty check_actions
  then t
  else { t with check_actions = t.check_actions @ check_actions }

let prepend_check_actions t check_actions =
  if List.is_empty check_actions
  then t
  else { t with check_actions = check_actions @ t.check_actions }

let with_debuginfo t ~dbg = if dbg == t.dbg then t else { t with dbg }

let no_args t = match args t with [] -> true | _ :: _ -> false

let is_goto t = no_args t && Option.is_none (trap_action t)

let is_raise t =
  match t.trap_action with
  | Some (Pop { exn_handler; _ }) -> Continuation.equal t.k exn_handler
  | Some (Push _) | None -> false

let clear_trap_action t = { t with trap_action = None }

let to_one_arg_without_trap_action t =
  match t.trap_action with
  | Some _ -> None
  | None -> ( match t.args with [arg] -> Some arg | [] | _ :: _ :: _ -> None)
