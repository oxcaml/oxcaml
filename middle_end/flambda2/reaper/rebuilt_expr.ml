(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaëlle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type continuation_handler =
  { handler : Flambda.Continuation_handler.t;
    free_names : Name_occurrences.t;
    code_size : Code_size.t
  }

type continuation_handlers =
  { handlers : Flambda.Continuation_handler.t Continuation.Lmap.t;
    free_names : Name_occurrences.t;
    code_size : Code_size.t
  }

type t =
  { expr : Flambda.Expr.t;
    free_names : Name_occurrences.t;
    code_size : Code_size.t
  }

let create_let bound_pattern defining_expr ~size_of_defining_expr ~body =
  let free_names =
    Name_occurrences.diff
      (Name_occurrences.union
         (Flambda.Named.free_names defining_expr)
         body.free_names)
      ~without:(Bound_pattern.free_names bound_pattern)
  in
  let let_expr =
    Flambda.Let_expr.create bound_pattern defining_expr ~body:body.expr
      ~free_names_of_body:(Known body.free_names)
  in
  let code_size =
    if Name_mode.is_phantom (Bound_pattern.name_mode bound_pattern)
    then body.code_size
    else Code_size.( + ) body.code_size size_of_defining_expr
  in
  let expr = Flambda.Expr.create_let let_expr in
  { expr; free_names; code_size }

let create_continuation_handler bound_parameters ~handler ~is_exn_handler
    ~is_cold =
  let free_names =
    Name_occurrences.diff handler.free_names
      ~without:(Bound_parameters.free_names bound_parameters)
  in
  let code_size = handler.code_size in
  let handler =
    Flambda.Continuation_handler.create bound_parameters ~handler:handler.expr
      ~free_names_of_handler:(Known handler.free_names) ~is_exn_handler ~is_cold
  in
  { handler; free_names; code_size }

let create_continuation_handlers handlers =
  let (code_size, free_names), handlers =
    Continuation.Lmap.fold_left_map
      (fun (code_size, free_names) _cont (handler : continuation_handler) ->
        let code_size = Code_size.( + ) code_size handler.code_size in
        let free_names = Name_occurrences.union free_names handler.free_names in
        (code_size, free_names), handler.handler)
      (Code_size.zero, Name_occurrences.empty)
      handlers
  in
  { handlers; free_names; code_size }

let create_non_recursive_let_cont cont (cont_handler : continuation_handler)
    ~body =
  let expr =
    Flambda.Let_cont_expr.create_non_recursive cont cont_handler.handler
      ~body:body.expr ~free_names_of_body:(Known body.free_names)
  in
  let free_names =
    Name_occurrences.union
      (Name_occurrences.remove_continuation body.free_names ~continuation:cont)
      cont_handler.free_names
  in
  let code_size = Code_size.( + ) body.code_size cont_handler.code_size in
  { expr; free_names; code_size }

let create_recursive_let_cont ~invariant_params handlers0 ~body =
  let handlers = create_continuation_handlers handlers0 in
  let expr =
    Flambda.Let_cont_expr.create_recursive ~invariant_params handlers.handlers
      ~body:body.expr
  in
  let handlers_free_names =
    Name_occurrences.diff handlers.free_names
      ~without:(Bound_parameters.free_names invariant_params)
  in
  let free_names =
    Name_occurrences.union body.free_names
      (Name_occurrences.increase_counts handlers_free_names)
  in
  let free_names =
    Continuation.Lmap.fold
      (fun cont _ free_names ->
        Name_occurrences.remove_continuation free_names ~continuation:cont)
      handlers0 free_names
  in
  let code_size = Code_size.( + ) body.code_size handlers.code_size in
  { expr; free_names; code_size }

let from_expr ~expr ~free_names ~code_size = { expr; free_names; code_size }
