(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaelle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type cont_kind = Normal of Variable.t list

type should_preserve_direct_calls =
  | Yes
  | No
  | Auto

type t =
  { parent : Rev_expr.rev_expr_holed;
    conts : cont_kind Continuation.Map.t;
    current_code_id : Code_id.t option;
    should_preserve_direct_calls : should_preserve_direct_calls;
    le_monde_exterieur : Name.t;
    all_constants : Name.t
  }

let create ~parent ~conts ~current_code_id ~should_preserve_direct_calls
    ~le_monde_exterieur ~all_constants =
  { parent;
    conts;
    current_code_id;
    should_preserve_direct_calls;
    le_monde_exterieur;
    all_constants
  }

let parent t = t.parent

let current_code_id t = t.current_code_id

let should_preserve_direct_calls t = t.should_preserve_direct_calls

let le_monde_exterieur t = t.le_monde_exterieur

let all_constants t = t.all_constants

let with_parent t parent = { t with parent }

let find_cont t cont =
  match Continuation.Map.find_opt cont t.conts with
  | Some cont_kind -> cont_kind
  | None ->
    Misc.fatal_errorf "[Env.find_cont]: continuation %a not found in env"
      Continuation.print cont

let add_cont t cont cont_kind =
  { t with conts = Continuation.Map.add cont cont_kind t.conts }
