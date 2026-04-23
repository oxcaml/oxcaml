open Std

module Mode_info = struct
  type t = Mode.Value.l

  let to_string ~verbosity mode =
    (* Zap mode variables to floor. *)
    let snap = Btype.snapshot () in
    let const = Mode.Value.zap_to_floor mode in
    Btype.backtrack snap;
    (* Convert modes into a list of modes. *)
    let verbose =
      match verbosity with
      | Mconfig.Verbosity.Lvl n -> n > 0
      | Smart -> false
    in
    let maybe_print (type t) (module Axis : Mode_intf.Const with type t = t)
        (mode : t) =
      if (not verbose) && Axis.equal Axis.legacy mode then None
      else Some (Format_doc.asprintf "%a" Axis.print mode)
    in
    (* Exhaustively match so that we pick up new modes. *)
    let ({ areality;
           portability;
           contention;
           visibility;
           statefulness;
           uniqueness;
           linearity;
           forkable;
           yielding;
           staticity
         }
          : Mode.Value.Const.t) =
      const
    in
    let modes =
      List.filter_map
        [ maybe_print (module Mode.Regionality.Const) areality;
          maybe_print (module Mode.Portability.Const) portability;
          maybe_print (module Mode.Contention.Const) contention;
          maybe_print (module Mode.Visibility.Const) visibility;
          maybe_print (module Mode.Statefulness.Const) statefulness;
          maybe_print (module Mode.Uniqueness.Const) uniqueness;
          maybe_print (module Mode.Linearity.Const) linearity;
          maybe_print (module Mode.Forkable.Const) forkable;
          maybe_print (module Mode.Yielding.Const) yielding;
          maybe_print (module Mode.Staticity.Const) staticity
        ]
        ~f:Fun.id
    in

    match modes with
    | [] -> "<default>"
    | modes -> "@ " ^ String.concat ~sep:" " modes
end

let from_node (_env, node) =
  let open Browse_raw in
  match node with
  | Expression { exp_desc = Texp_ident { mode; _ }; exp_loc; _ } ->
    Some (exp_loc, mode)
  | Pattern { pat_desc = Tpat_var { mode; _ }; pat_loc; _ } ->
    Some (pat_loc, mode)
  | _ -> None

let from_mbrowse (mbrowse : Mbrowse.t) = List.filter_map ~f:from_node mbrowse
