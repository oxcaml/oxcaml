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

<<<<<<< HEAD:backend/cmm_invariants.ml
||||||| 23e84b8c4d:asmcomp/cmm_invariants.ml
module Int = Numbers.Int
=======
module V = Backend_var
module VP = Backend_var.With_provenance
module Int = Numbers.Int
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a:asmcomp/cmm_invariants.ml

(* Check a number of invariants around continuation and variable uses *)

type mutability = Mutable | Immutable

let equal_mutability m1 m2 =
  match m1, m2 with
  | Mutable, Mutable | Immutable, Immutable -> true
  | Mutable, Immutable | Immutable, Mutable -> false

let mutability_to_string m =
  match m with
  | Mutable -> "mutable"
  | Immutable -> "immutable"

module Env : sig
  type t

  val init : unit -> t

  val handler : t -> cont:Static_label.t -> arg_num:int -> t

  val jump : t -> exit_label:Cmm.exit_label -> arg_num:int -> unit

  val bind_var : t -> V.t -> mutability -> t

  val bind_params : t -> (VP.t * _) list -> t

  val use_var : t -> V.t -> mutability -> unit

  val report : Format.formatter -> bool
end = struct
  type t = {
<<<<<<< HEAD:backend/cmm_invariants.ml
    bound_handlers : int Static_label.Map.t;
||||||| 23e84b8c4d:asmcomp/cmm_invariants.ml
    bound_handlers : int Int.Map.t;
=======
    bound_handlers : int Int.Map.t;
    bound_variables : mutability V.Map.t;
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a:asmcomp/cmm_invariants.ml
  }

  type error =
    | Unbound_handler of { cont: Static_label.t }
    | Multiple_handlers of { cont: Static_label.t; }
    | Wrong_arguments_number of
<<<<<<< HEAD:backend/cmm_invariants.ml
        { cont: Static_label.t; handler_args: int; jump_args: int; }
||||||| 23e84b8c4d:asmcomp/cmm_invariants.ml
        { cont: int; handler_args: int; jump_args: int; }
=======
        { cont: int; handler_args: int; jump_args: int; }
    | Unbound_variable of { var : V.t; mut : mutability }
    | Wrong_mutability of
        { var : V.t; binding_mut : mutability; use_mut : mutability }
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a:asmcomp/cmm_invariants.ml

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
<<<<<<< HEAD:backend/cmm_invariants.ml
      bound_handlers = Static_label.Map.empty;
||||||| 23e84b8c4d:asmcomp/cmm_invariants.ml
      bound_handlers = Int.Map.empty;
=======
      bound_handlers = Int.Map.empty;
      bound_variables = V.Map.empty;
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a:asmcomp/cmm_invariants.ml
    }

  let handler t ~cont ~arg_num =
<<<<<<< HEAD:backend/cmm_invariants.ml
    if Static_label.Set.mem cont state.all_handlers then multiple_handler cont;
    state.all_handlers <- Static_label.Set.add cont state.all_handlers;
    let bound_handlers = Static_label.Map.add cont arg_num t.bound_handlers in
    { bound_handlers; }
||||||| 23e84b8c4d:asmcomp/cmm_invariants.ml
    if Int.Set.mem cont state.all_handlers then multiple_handler cont;
    state.all_handlers <- Int.Set.add cont state.all_handlers;
    let bound_handlers = Int.Map.add cont arg_num t.bound_handlers in
    { bound_handlers; }
=======
    if Int.Set.mem cont state.all_handlers then multiple_handler cont;
    state.all_handlers <- Int.Set.add cont state.all_handlers;
    let bound_handlers = Int.Map.add cont arg_num t.bound_handlers in
    { t with bound_handlers; }
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a:asmcomp/cmm_invariants.ml

  let jump t ~exit_label ~arg_num =
    match (exit_label : Cmm.exit_label) with
    | Return_lbl -> ()
    | Lbl cont ->
      match Static_label.Map.find cont t.bound_handlers with
      | handler_args ->
        if arg_num <> handler_args then
          wrong_arguments cont handler_args arg_num
      | exception Not_found -> unbound_handler cont

  let bind_var t var mut =
    let bound_variables = V.Map.add var mut t.bound_variables in
    { t with bound_variables }

  let bind_params t params =
    let bound_variables =
        List.fold_left (fun bound_vars (var, _) ->
            V.Map.add (VP.var var) Immutable bound_vars)
        t.bound_variables params
    in
    { t with bound_variables }

  let use_var t var use_mut =
    match V.Map.find_opt var t.bound_variables with
    | Some binding_mut ->
      if equal_mutability use_mut binding_mut
      then ()
      else record_error (Wrong_mutability { var; binding_mut; use_mut })
    | None ->
      record_error (Unbound_variable { var; mut = use_mut })

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
    | Unbound_variable { var; mut } ->
      Format.fprintf ppf
        "Variable %a (%s) was unbound or used outside the scope of its binder"
        V.print var (mutability_to_string mut)
    | Wrong_mutability { var; binding_mut; use_mut } ->
      Format.fprintf ppf
        "Variable %a was bound as %s but used as %s"
        V.print var
        (mutability_to_string binding_mut)
        (mutability_to_string use_mut)

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
<<<<<<< HEAD:backend/cmm_invariants.ml
  | Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
  | Cconst_symbol _ | Cconst_vec128 _ | Cconst_vec256 _ | Cconst_vec512 _
  | Cvar _ | Cinvalid _ ->
||||||| 23e84b8c4d:asmcomp/cmm_invariants.ml
  | Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _
  | Cvar _ | Creturn_addr ->
=======
  | Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _
  | Creturn_addr ->
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a:asmcomp/cmm_invariants.ml
    ()
<<<<<<< HEAD:backend/cmm_invariants.ml
  | Clet (_, expr, body) ->
||||||| 23e84b8c4d:asmcomp/cmm_invariants.ml
  | Clet (_, expr, body)
  | Clet_mut (_, _, expr, body) ->
=======
  | Cvar id ->
    Env.use_var env id Immutable
  | Cvar_mut id ->
    Env.use_var env id Mutable
  | Clet (id, expr, body) ->
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a:asmcomp/cmm_invariants.ml
    check env expr;
    check (Env.bind_var env (VP.var id) Immutable) body
  | Clet_mut (id, _, expr, body) ->
    check env expr;
    check (Env.bind_var env (VP.var id) Mutable) body
  | Cphantom_let (_, _, expr) ->
    check env expr
<<<<<<< HEAD:backend/cmm_invariants.ml
||||||| 23e84b8c4d:asmcomp/cmm_invariants.ml
  | Cassign (_, expr) ->
    check env expr
=======
  | Cassign (id, expr) ->
    Env.use_var env id Mutable;
    check env expr
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a:asmcomp/cmm_invariants.ml
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
<<<<<<< HEAD:backend/cmm_invariants.ml
    List.iter (fun Cmm.{body = handler; _} -> check env_handler handler) handlers
  | Cexit (exit_label, args, _trap_actions) ->
    Env.jump env ~exit_label ~arg_num:(List.length args)
||||||| 23e84b8c4d:asmcomp/cmm_invariants.ml
    List.iter (fun (_, _, handler, _) -> check env_handler handler) handlers
  | Cexit (cont, args) ->
    Env.jump env ~cont ~arg_num:(List.length args)
  | Ctrywith (body, _, handler, _) ->
    (* Jumping from inside a trywith body to outside isn't very nice,
       but it's handled correctly by Linearize, as it happens
       when compiling match ... with exception ..., for instance, so it is
       not reported as an error. *)
    check env body;
    check env handler
=======
    List.iter (fun (_, args, handler, _) ->
        let env_handler = Env.bind_params env_handler args in
        check env_handler handler)
      handlers
  | Cexit (cont, args) ->
    Env.jump env ~cont ~arg_num:(List.length args)
  | Ctrywith (body, id, handler, _) ->
    (* Jumping from inside a trywith body to outside isn't very nice,
       but it's handled correctly by Linearize, as it happens
       when compiling match ... with exception ..., for instance, so it is
       not reported as an error. *)
    check env body;
    check (Env.bind_var env (VP.var id) Immutable) handler
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a:asmcomp/cmm_invariants.ml

let run ppf (fundecl : Cmm.fundecl) =
  let env = Env.bind_params (Env.init ()) fundecl.fun_args in
  check env fundecl.fun_body;
  Env.report ppf
