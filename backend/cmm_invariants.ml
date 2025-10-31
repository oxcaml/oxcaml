(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2017 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare


(* Check a number of continuation-related invariants *)

module Env : sig
  type t

  val init : unit -> t

  val handler : t -> cont:Static_label.t -> arg_num:int -> t

  val jump : t -> exit_label:Cmm.exit_label -> arg_num:int -> unit

  val report : Format.formatter -> bool
end = struct
  type t = {
    bound_handlers : int Static_label.Map.t;
  }

  type error =
    | Unbound_handler of { cont: Static_label.t }
    | Multiple_handlers of { cont: Static_label.t; }
    | Wrong_arguments_number of
        { cont: Static_label.t; handler_args: int; jump_args: int; }

  module Error = struct
    type t = error

    let compare = Stdlib.compare
  end

  module ErrorSet = Set.Make(Error)

  type persistent_state = {
    mutable all_handlers : Static_label.Set.t;
    mutable errors : ErrorSet.t;
  }

  let state = {
    all_handlers = Static_label.Set.empty;
    errors = ErrorSet.empty;
  }

  let record_error error =
    state.errors <- ErrorSet.add error state.errors

  let unbound_handler cont =
    record_error (Unbound_handler { cont; })

  let multiple_handler cont =
    record_error (Multiple_handlers { cont; })

  let wrong_arguments cont handler_args jump_args =
    record_error (Wrong_arguments_number { cont; handler_args; jump_args; })

  let init () =
    state.all_handlers <- Static_label.Set.empty;
    state.errors <- ErrorSet.empty;
    {
      bound_handlers = Static_label.Map.empty;
    }

  let handler t ~cont ~arg_num =
    if Static_label.Set.mem cont state.all_handlers then multiple_handler cont;
    state.all_handlers <- Static_label.Set.add cont state.all_handlers;
    let bound_handlers = Static_label.Map.add cont arg_num t.bound_handlers in
    { bound_handlers; }

  let jump t ~exit_label ~arg_num =
    match (exit_label : Cmm.exit_label) with
    | Return_lbl -> ()
    | Lbl cont ->
      match Static_label.Map.find cont t.bound_handlers with
      | handler_args ->
        if arg_num <> handler_args then
          wrong_arguments cont handler_args arg_num
      | exception Not_found -> unbound_handler cont

  let print_error ppf error =
    match error with
    | Unbound_handler { cont } ->
      if Static_label.Set.mem cont state.all_handlers then
        Format.fprintf ppf
          "Continuation %a was used outside the scope of its handler"
          Static_label.format cont
      else
        Format.fprintf ppf
          "Continuation %a was used but never bound"
          Static_label.format cont
    | Multiple_handlers { cont; } ->
      Format.fprintf ppf
        "Continuation %a was declared in more than one handler"
        Static_label.format cont
    | Wrong_arguments_number { cont; handler_args; jump_args } ->
      Format.fprintf ppf
        "Continuation %a was declared with %d arguments but called with %d"
        Static_label.format cont
        handler_args
        jump_args

  let print_error_newline ppf error =
    Format.fprintf ppf "%a@." print_error error

  let report ppf =
    if ErrorSet.is_empty state.errors then false
    else begin
      ErrorSet.iter (fun err -> print_error_newline ppf err) state.errors;
      true
    end
end

let rec check env (expr : Cmm.expression) =
  match expr with
  | Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
  | Cconst_symbol _ | Cconst_vec128 _ | Cconst_vec256 _ | Cconst_vec512 _
  | Cvar _ ->
    ()
  | Clet (_, expr, body) ->
    check env expr;
    check env body
  | Cphantom_let (_, _, expr) ->
    check env expr
  | Cname_for_debugger expr ->
    check env expr
  | Ctuple exprs ->
    List.iter (check env) exprs
  | Cop (_, args, _) ->
    List.iter (check env) args;
  | Csequence (expr1, expr2) ->
    check env expr1;
    check env expr2
  | Cifthenelse (test, _, ifso, _, ifnot, _) ->
    check env test;
    check env ifso;
    check env ifnot
  | Cswitch (body, _, branches, _) ->
    check env body;
    Array.iter (fun (expr, _) -> check env expr) branches
  | Ccatch (flag, handlers, body) ->
    let env_extended =
      List.fold_left
        (fun env Cmm.{label = cont; params = args; _} ->
           Env.handler env ~cont:cont ~arg_num:(List.length args))
        env
        handlers
    in
    check env_extended body;
    let env_handler =
      match flag with
      | Recursive -> env_extended
      | Normal | Exn_handler -> env
    in
    List.iter (fun Cmm.{body = handler; _} -> check env_handler handler) handlers
  | Cexit (exit_label, args, _trap_actions) ->
    Env.jump env ~exit_label ~arg_num:(List.length args)

let run ppf (fundecl : Cmm.fundecl) =
  let env = Env.init () in
  check env fundecl.fun_body;
  Env.report ppf
