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

  val register_width_of_variable :
    t ->
    Backend_var.With_provenance.t ->
    width:int ->
    t

  val width_of_variable_if_known : t -> Backend_var.t -> int option

  val handler : t -> cont:Static_label.t -> width:int -> t

  val jump : t -> exit_label:Cmm.exit_label -> width_opt:int option -> unit

  val report : Format.formatter -> bool
end = struct
  type t = {
    bound_handlers : int Static_label.Map.t;
    known_widths_of_variables : int Backend_var.Map.t;
  }

  type error =
    | Unbound_handler of { cont: Static_label.t }
    | Multiple_handlers of { cont: Static_label.t; }
    | Wrong_width of
        { cont: Static_label.t; handler_width: int; jump_width: int; }

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

  let wrong_width cont ~handler_width ~jump_width =
    record_error (Wrong_width { cont; handler_width; jump_width; })

  let init () =
    state.all_handlers <- Static_label.Set.empty;
    state.errors <- ErrorSet.empty;
    { bound_handlers = Static_label.Map.empty;
      known_widths_of_variables = Backend_var.Map.empty;
    }

  let register_width_of_variable t var ~width =
    { t with
      known_widths_of_variables =
        let var = Backend_var.With_provenance.var var in
        Backend_var.Map.add var width t.known_widths_of_variables;
    }

  let width_of_variable_if_known t var =
    Backend_var.Map.find_opt var t.known_widths_of_variables

  let handler t ~cont ~width =
    if Static_label.Set.mem cont state.all_handlers then multiple_handler cont;
    state.all_handlers <- Static_label.Set.add cont state.all_handlers;
    let bound_handlers = Static_label.Map.add cont width t.bound_handlers in
    { t with bound_handlers }

  let jump t ~exit_label ~width_opt =
    match (exit_label : Cmm.exit_label) with
    | Return_lbl -> ()
    | Lbl cont ->
      match Static_label.Map.find cont t.bound_handlers, width_opt with
      | handler_width, Some jump_width ->
        if handler_width <> jump_width then
          wrong_width cont ~handler_width ~jump_width
      | _, None -> ()
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
    | Wrong_width { cont; handler_width; jump_width } ->
      Format.fprintf ppf
        "Continuation %a was declared with %d components but called with %d"
        Static_label.format cont
        handler_width
        jump_width

  let print_error_newline ppf error =
    Format.fprintf ppf "%a@." print_error error

  let report ppf =
    if ErrorSet.is_empty state.errors then false
    else begin
      ErrorSet.iter (fun err -> print_error_newline ppf err) state.errors;
      true
    end
end

let rec syntactic_width env (expr : Cmm.expression) : int option =
  match expr with
  | Cconst_float _
  | Cconst_float32 _
  | Cconst_int _
  | Cconst_mask _
  | Cconst_natint _
  | Cconst_symbol _
  | Cconst_vec128 _
  | Cconst_vec256 _
  | Cconst_vec512 _
    -> Some 1
  | Ccatch _
  | Cexit _
  | Cifthenelse _
  | Cinvalid _
  | Cswitch _
    -> None
  | Cname_for_debugger (_, body)
  | Cphantom_let (_, _, body)
  | Csequence (_, body)
    -> syntactic_width env body
  | Clet (var, defining_expr, body)
    -> syntactic_width
         (register_syntactic_width_of_assignment env var defining_expr)
         body
  | Ctuple exprs (* This is an *unboxed* tuple. *)
    -> syntactic_width_of_unboxed_product env exprs
  | Cvar var
    -> Env.width_of_variable_if_known env var
  | Cop (op, _, _)
    -> Some (Array.length (Select_utils.oper_result_type op))

and syntactic_width_of_unboxed_product env =
  List.fold_left
    (fun acc expr ->
      match acc, syntactic_width env expr with
      | Some acc, Some width -> Some (acc + width)
      | None, _ | _, None -> None)
    (Some 0)

and register_syntactic_width_of_assignment env var expr =
  match syntactic_width env expr with
  | Some width -> Env.register_width_of_variable env var ~width
  | None -> env

let register_syntactic_width_of_params env :
    (_ * Cmx_format.machtype_component array) list -> Env.t =
  List.fold_left
    (fun acc (var, machtypes) ->
      Env.register_width_of_variable acc var ~width:(Array.length machtypes))
    env

let width_of_params : (_ * Cmx_format.machtype_component array) list -> int =
  List.fold_left (fun acc (_, machtypes) -> acc + Array.length machtypes) 0

let rec check env (expr : Cmm.expression) =
  match expr with
  | Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
  | Cconst_symbol _ | Cconst_vec128 _ | Cconst_vec256 _ | Cconst_vec512 _
  | Cconst_mask _ | Cvar _ | Cinvalid _ ->
    ()
  | Clet (var, expr, body) ->
    check env expr;
    check (register_syntactic_width_of_assignment env var expr) body
  | Cphantom_let (_, _, expr) ->
    check env expr
  | Cname_for_debugger (_, expr) ->
    check env expr
  | Ctuple exprs ->
    List.iter (check env) exprs
  | Cop (_, args, _) ->
    List.iter (check env) args;
  | Csequence (expr1, expr2) ->
    check env expr1;
    check env expr2
  | Cifthenelse (test, _, _, ifso, _, _, ifnot, _) ->
    check env test;
    check env ifso;
    check env ifnot
  | Cswitch (body, _, branches, _) ->
    check env body;
    Array.iter (fun (expr, _, _) -> check env expr) branches
  | Ccatch (flag, handlers, body) ->
    let env_extended =
      List.fold_left
        (fun env Cmm.{label = cont; params; _} ->
           Env.handler env ~cont ~width:(width_of_params params))
        env
        handlers
    in
    check env_extended body;
    let env_handler =
      match flag with
      | Recursive -> env_extended
      | Normal | Exn_handler -> env
    in
    List.iter
      (fun Cmm.{body = handler; params; _} ->
        check (register_syntactic_width_of_params env_handler params) handler)
      handlers
  | Cexit (exit_label, args, _trap_actions) ->
    Env.jump env ~exit_label
      ~width_opt:(syntactic_width_of_unboxed_product env args)

let run ppf (fundecl : Cmm.fundecl) =
  let env = Env.init () in
  check env fundecl.fun_body;
  Env.report ppf
