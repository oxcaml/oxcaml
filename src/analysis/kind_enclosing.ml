open Std
open Type_utils

module Kind_info = struct
  type t = { kind : Types.jkind_l; env : Env.t }

  let mk ~kind ~env = { kind; env }

  let from_type ~env ty = { kind = Ctype.estimate_type_jkind env ty; env }

  let to_string ~(verbosity : Mconfig.Verbosity.t) { kind; env } =
    let kind =
      Jkind.normalize ~mode:Require_best
        ~context:(Ctype.mk_jkind_context_check_principal env)
        kind
    in
    Printtyp.wrap_printing_env ~verbosity env (fun () ->
        let format_jkind =
          match verbosity with
          | Smart | Lvl 0 -> Jkind.format
          | _ -> Jkind.format_expanded
        in
        Format.asprintf "%a" format_jkind kind)
end

let loc_contains_cursor (loc : Location.t) ~cursor =
  Lexing.compare_pos loc.loc_start cursor < 0
  && Lexing.compare_pos cursor loc.loc_end < 0

let enclosings_of_node ~cursor (env, (node : Browse_raw.node)) :
    (Location.t * Kind_info.t) list =
  match node with
  | Pattern pattern ->
    [ (pattern.pat_loc, Kind_info.from_type ~env pattern.pat_type) ]
  | Expression expr ->
    [ (expr.exp_loc, Kind_info.from_type ~env expr.exp_type) ]
  | Core_type core_type ->
    let constr_enclosings =
      match core_type.ctyp_desc with
      | Ttyp_constr (path, ident, _) when loc_contains_cursor ident.loc ~cursor
        ->
        (* TODO: The env here contains placeholder jkinds for types declared in the same
           recursive block, which causes under-approximations to be returned in some
           cases. *)
        let decl = Env.find_type path env in
        [ (ident.loc, Kind_info.mk ~kind:decl.type_jkind ~env) ]
      | _ -> []
    in
    constr_enclosings
    @ [ (core_type.ctyp_loc, Kind_info.from_type ~env core_type.ctyp_type) ]
  | Type_declaration decl ->
    [ (decl.typ_loc, Kind_info.mk ~kind:decl.typ_type.type_jkind ~env) ]
  | _ -> []

let from_mbrowse mbrowse ~cursor : (Location.t * Kind_info.t) list =
  List.concat_map mbrowse ~f:(enclosings_of_node ~cursor)
