(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2026--2026 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Flambda.Import

module Env = struct

  type t = {
    defined_symbols : Symbol.Set.t;

  }

  let empty = { defined_symbols = Symbol.Set.empty; }

  let check_uses env free_names =
    let used_symbols = Name_occurrences.symbols free_names in
    let symbols_used_but_not_defined =
      Symbol.Set.diff used_symbols env.defined_symbols
    in
    Symbol.Set.iter (fun symbol ->
        (* Symbols from other compilation units (or external symbols, or predef symbols)
           will not have their definition seen *)
        if Compilation_unit.is_current (Symbol.compilation_unit symbol) then
          Misc.fatal_errorf "The following symbol is used but not defined:@ %a"
            Symbol.print symbol)
      symbols_used_but_not_defined

  let check_simple env simple =
    check_uses env (Simple.free_names simple)

  let add_definitions { defined_symbols; } being_defined =
    let new_symbols = Name_occurrences.symbols being_defined in
    let defined_twice = Symbol.Set.inter defined_symbols new_symbols in
    if not (Symbol.Set.is_empty defined_twice) then
      Misc.fatal_errorf "The following symbols are defined twice:@ %a"
        Symbol.Set.print defined_twice
    else
      let defined_symbols = Symbol.Set.union defined_symbols new_symbols in
      { defined_symbols; }

  let inter t t' =
    let defined_symbols = Symbol.Set.inter t.defined_symbols t'.defined_symbols in
    { defined_symbols; }

end

module Res = struct

  type t = {
    calls : Env.t list Continuation.Map.t;
  }

  let empty = { calls = Continuation.Map.empty; }

  let get t cont =
    match Continuation.Map.find_opt cont t.calls with
    | None -> []
    | Some l -> l

  let add t cont env =
    let l = get t cont in
    let calls = Continuation.Map.add cont (env :: l) t.calls in
    { calls }

  let clear t cont =
    let calls = Continuation.Map.remove cont t.calls in
    { calls; }

  let pop t cont =
    get t cont, clear t cont

end


module Check = struct

  let rec expr env res e : Res.t =
    match Expr.descr e with
    | Let e' -> let_expr env res e'
    | Let_cont e' -> let_cont env res e'
    | Apply e' -> apply_expr env res e'
    | Apply_cont e' -> apply_cont env res e'
    | Switch e' -> switch env res e'
    | Invalid { message } -> invalid env res ~message

  and invalid _env res ~message:_ = res

  (* Symbols are allowed to be recursive, and we only check symbols here, so we can
     add the bound symbols to the env *before* checking the uses in the defining expr. *)
  and let_expr env res e =
    Let.pattern_match e ~f:(fun bound_pattern ~body ->
        (* update the env *)
        let being_defined = Bound_pattern.free_names bound_pattern in
        let env = Env.add_definitions env being_defined in
        (* check the defining expr after, see comment above *)
        let defining_expr = Let.defining_expr e in
        let uses_of_defining_expr = Named.free_names defining_expr in
        Env.check_uses env uses_of_defining_expr;
        expr env res body)

  (* Let-bound continuations are stored in the env to be checked later, when they are called *)
  and let_cont env res e =
    let recursive, handlers, body =
      match (e : Flambda.let_cont_expr) with
      | Non_recursive { handler; _ } ->
          Non_recursive_let_cont_handler.pattern_match handler ~f:(fun cont ~body ->
              let h = Non_recursive_let_cont_handler.handler handler in
              false, Continuation.Lmap.singleton cont h, body)
      | Recursive rec_handlers ->
          Recursive_let_cont_handlers.pattern_match rec_handlers
            ~f:(fun ~invariant_params:_ ~body cont_handlers ->
                true, Continuation_handlers.to_map cont_handlers, body)
    in
    let res = expr env res body in
    Continuation.Lmap.fold (fun cont handler res ->
        let calls, res = Res.pop res cont in
        match calls with
        | [] ->
          (* CR gbury: this means the continuation handler is unreachable, we should
             probably have cleaned it up ? *)
            res
        | env :: others ->
          let common_env = List.fold_left Env.inter env others in
          let res =
            Continuation_handler.pattern_match handler
              ~f:(fun _ ~handler -> expr common_env res handler)
          in
          (* For recursive continuations, clear the recursive calls *)
          let res = if recursive then Res.clear res cont else res in
          res
      ) handlers res

  and apply_expr env res e =
    List.iter (Env.check_simple env) (Apply.args e);
    let res =
      match Apply.continuation e with
      | Never_returns -> res
      | Return cont -> Res.add res cont env
    in
    let res =
      let exn_cont = Apply.exn_continuation e in
      List.iter (fun (simple, _) -> Env.check_simple env simple) (Exn_continuation.extra_args exn_cont);
      Res.add res (Exn_continuation.exn_handler exn_cont) env
    in
    res

  and switch env res e =
    Env.check_simple env (Switch.scrutinee e);
    Target_ocaml_int.Map.fold (fun _discriminant ac res ->
        apply_cont env res ac
      ) (Switch.arms e) res

  and apply_cont env res e =
    List.iter (Env.check_simple env) (Apply_cont.args e);
    Res.add res (Apply_cont.continuation e) env

end



let check body =
  let env = Env.empty in
  let res = Res.empty in
  let _res = Check.expr env res body in
  ()

