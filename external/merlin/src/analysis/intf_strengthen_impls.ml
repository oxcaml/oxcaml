open Std

let { Logger.log } = Logger.for_section "intf-strengthen-impls"

module Abstract = Intf_strengthen.Abstract
module Helpers = Module_implementation_query_helpers
module Impls = Query_protocol.Module_type_impls
module Intf_weakness = Query_protocol.Intf_weakness

let all_or_none options =
  let rec aux acc = function
    | [] -> Some (List.rev acc)
    | None :: _ -> None
    | Some x :: rest -> aux (x :: acc) rest
  in
  aux [] options

let span_start (loc : Location.t) = loc.loc_start.Lexing.pos_cnum
let span_end (loc : Location.t) = loc.loc_end.Lexing.pos_cnum
let same_file a b = String.equal (Filename.basename a) (Filename.basename b)

type site =
  | Unit_signature
  | Ascription of Location.t

type subject = {
  file : string;
  site : site
}

type target =
  | Unit_interface
  | Module_type of {
      name : string;
      decl_span_start : int
    }

type work = {
  target : target;
  subjects : subject list
}

type discovery =
  | Unusable of string
  | Works of work list

module Target_key = struct
  type t =
    | Own_interface
    | Modtype of {
        name : string;
        span_start : int;
        span_end : int
      }

  let of_row (row : Impls.implementation) =
    match row.target with
    | Own_interface -> Some Own_interface
    | Modtype name ->
      Option.map row.target_loc ~f:(fun loc ->
          Modtype
            { name; span_start = span_start loc; span_end = span_end loc })
end

let subject_of_row config (row : Impls.implementation) =
  let loc = row.site.impl_loc in
  let recorded = loc.loc_start.Lexing.pos_fname in
  let file =
    if String.equal recorded "" then None
    else if Filename.check_suffix recorded ".mli" then
      Helpers.impl_source_of_interface config recorded
    else if Filename.check_suffix recorded ".ml" then
      Some (Misc.canonicalize_filename recorded)
    else None
  in
  Option.bind file ~f:(fun file ->
      match row.site.impl_kind with
      | Whole_unit -> Some { file; site = Unit_signature }
      | Ascription_sites ->
        if same_file recorded file && span_start loc < span_end loc then
          Some { file; site = Ascription loc }
        else None)

let works_of_response config (response : Impls.response) =
  match response.status with
  | Partial ->
    Unusable
      (Printf.sprintf "discovery is partial (%d reasons)"
         (List.length response.reasons))
  | Unavailable ->
    Works []
  | Complete ->
    let rows =
      List.map response.implementations ~f:(fun row ->
          match Target_key.of_row row, subject_of_row config row with
          | Some key, Some subject -> Some (key, subject)
          | (None | Some _), (None | Some _) -> None)
    in
    (match all_or_none rows with
    | None ->
      Unusable "an implementation could not be resolved to a source subject"
    | Some rows ->
      let keys = List.sort_uniq ~cmp:compare (List.map rows ~f:fst) in
      Works
        (List.map keys ~f:(fun key ->
             let subjects =
               List.sort_uniq ~cmp:compare
                 (List.filter_map rows ~f:(fun (key', subject) ->
                      if key' = key then Some subject else None))
             in
             let target =
               match key with
               | Target_key.Own_interface -> Unit_interface
               | Target_key.Modtype { name; span_start; span_end = _ } ->
                 Module_type { name; decl_span_start = span_start }
             in
             { target; subjects })))

let with_own_unit config (typedtree : Mtyper.typedtree) works =
  match typedtree with
  | `Interface _ -> works
  | `Implementation _ ->
    let own = { file = Helpers.own_file config; site = Unit_signature } in
    let updated =
      List.map works ~f:(fun work ->
          match work.target with
          | Module_type _ -> work
          | Unit_interface ->
            { work with
              subjects = List.sort_uniq ~cmp:compare (own :: work.subjects)
            })
    in
    let own_is_covered =
      List.exists updated ~f:(fun work ->
          match work.target with
          | Unit_interface -> true
          | Module_type _ -> false)
    in
    if own_is_covered then updated
    else { target = Unit_interface; subjects = [ own ] } :: updated

let module_type_body (parsetree : Mreader.parsetree) ~decl_span_start =
  let found = ref None in
  let declared_here (loc : Location.t) = span_start loc = decl_span_start in
  let iterator =
    { Ast_iterator.default_iterator with
      module_type_declaration =
        (fun iterator (mtd : Parsetree.module_type_declaration) ->
          (match mtd.pmtd_type with
          | Some { pmty_desc = Pmty_signature signature; _ }
            when declared_here mtd.pmtd_loc ->
            found := Some signature
          | Some _ | None -> ());
          Ast_iterator.default_iterator.module_type_declaration iterator mtd)
    }
  in
  (match parsetree with
  | `Implementation structure -> iterator.structure iterator structure
  | `Interface signature -> iterator.signature iterator signature);
  !found

let longident_of_unit_path ~unit_name name =
  let parts = String.split_on_char name ~sep:'.' in
  List.fold_left parts ~init:(Longident.Lident unit_name)
    ~f:(fun prefix part ->
      Longident.Ldot (Location.mknoloc prefix, Location.mknoloc part))

let module_type_signature ~env ~unit_name name =
  match
    Env.lookup_modtype ~use:false ~loc:Location.none
      (longident_of_unit_path ~unit_name name)
      env
  with
  | exception _ ->
    log ~title:"module_type_signature" "cannot resolve %s.%s" unit_name name;
    None
  | _path, mtd -> (
    match Option.map mtd.mtd_type ~f:(Mtype.scrape env) with
    | Some (Types.Mty_signature signature) -> Some signature
    | Some _ | None -> None)

let read_file file =
  match open_in_bin file with
  | exception Sys_error message ->
    log ~title:"read_file" "cannot read %s: %s" file message;
    None
  | ic ->
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> Some (Misc.string_of_file ic))

let interface_source (config : Mconfig.t) impl_file =
  let candidate = Filename.remove_extension impl_file ^ ".mli" in
  let file =
    if Sys.file_exists candidate then
      Some (Misc.canonicalize_filename candidate)
    else
      match
        Misc.find_in_path_normalized
          (Mconfig.source_path config)
          (Filename.basename candidate)
      with
      | file -> Some (Misc.canonicalize_filename file)
      | exception Not_found -> None
  in
  Option.bind file ~f:(fun file ->
      Option.bind (read_file file) ~f:(fun text ->
          let reader_config =
            { config with
              query =
                { config.query with
                  filename = file;
                  directory = Filename.dirname file
                }
            }
          in
          match
            (Mreader.parse reader_config (Msource.make text, None))
              .Mreader.parsetree
          with
          | `Interface intf -> Some (file, intf)
          | `Implementation _ ->
            log ~title:"interface_source" "%s is not an interface" file;
            None))

let unit_interface config impl_file =
  match Load_path.find_normalized (Env.get_current_unit_name () ^ ".cmi") with
  | exception Not_found -> None
  | path -> (
    match Cmi_cache.read path with
    | exception _ -> None
    | cmi ->
      Option.map (interface_source config impl_file)
        ~f:(fun (intf_file, intf) ->
          intf_file, intf, Subst.Lazy.force_signature (fst cmi.cmi_sign)))

let config_for_file (config : Mconfig.t) file =
  Mconfig.get_external_config file
    { config with
      query =
        { config.query with
          filename = Filename.basename file;
          directory = Filename.dirname file
        }
    }

let ascribed_module_type ~loc (structure : Typedtree.structure) =
  let covering = ref [] in
  let covers (binding : Location.t) =
    same_file binding.loc_start.Lexing.pos_fname
      loc.Location.loc_start.Lexing.pos_fname
    && span_start binding <= span_start loc
    && span_end loc <= span_end binding
  in
  let iterator =
    { Tast_iterator.default_iterator with
      module_binding =
        (fun iterator (mb : Typedtree.module_binding) ->
          if covers mb.mb_loc then
            covering := (mb.mb_loc, mb.mb_expr.mod_type) :: !covering;
          Tast_iterator.default_iterator.module_binding iterator mb)
    }
  in
  iterator.structure iterator structure;
  let innermost =
    List.sort !covering ~cmp:(fun (a, _) (b, _) ->
        compare (span_end a - span_start a) (span_end b - span_start b))
  in
  match innermost with
  | (_, module_type) :: _ -> Some module_type
  | [] -> None

let subject_signature ~env ~site (structure : Typedtree.structure) =
  match site with
  | Unit_signature -> Some structure.str_type
  | Ascription loc -> (
    match Option.map (ascribed_module_type ~loc structure) ~f:(Mtype.scrape env)
    with
    | Some (Types.Mty_signature signature) -> Some signature
    | Some _ | None ->
      log ~title:"subject_signature" "no module signature ascribed at %d-%d"
        (span_start loc) (span_end loc);
      None)

let with_typed_subject ~pipeline ~config subject ~f =
  let analyze pipeline =
    let typer = Mpipeline.typer_result pipeline in
    match Mtyper.get_typedtree typer with
    | `Interface _ -> None
    | `Implementation structure ->
      let env = Mtyper.get_env typer in
      Option.bind (subject_signature ~env ~site:subject.site structure)
        ~f:(fun impl_sig ->
          f ~config:(Mpipeline.final_config pipeline) ~env ~impl_sig)
  in
  if String.equal subject.file (Helpers.own_file config) then analyze pipeline
  else
    Option.bind (read_file subject.file) ~f:(fun text ->
        let source = Msource.make text in
        let config = config_for_file config subject.file in
        let parsetree =
          (Mreader.parse config (source, None)).Mreader.parsetree
        in
        analyze
          (Mpipeline.make_with_parsetree
             ~state:(Mpipeline.typer_state pipeline)
             config source parsetree))

module Merge = struct
  module Decl_key = struct
    type t = {
      file : string;
      span_start : int;
      span_end : int
    }

    let of_loc (loc : Location.t) =
      { file = Filename.basename loc.Location.loc_start.Lexing.pos_fname;
        span_start = span_start loc;
        span_end = span_end loc
      }
  end

  let agreed_on_axis (type a) (axis : a Mode.Alloc.Axis.t) ~claimed ~others =
    match Mode.Alloc.Const.Option.proj axis claimed with
    | None -> None
    | Some value ->
      let shared =
        List.for_all others ~f:(fun claims ->
            Mode.Alloc.Const.Option.proj axis claims = Some value)
      in
      if shared then Some value else None

  let alloc_claims ~intf impls =
    match List.map impls ~f:(fun impl -> Mode.Alloc.Const.diff impl intf) with
    | [] -> intf
    | claimed :: others ->
      let agreed =
        List.fold_left Mode.Alloc.Axis.all ~init:Mode.Alloc.Const.Option.none
          ~f:(fun agreed (Mode.Alloc.Axis.P axis) ->
            Mode.Alloc.Const.Option.set axis
              (agreed_on_axis axis ~claimed ~others)
              agreed)
      in
      Mode.Alloc.Const.Option.value agreed ~default:intf

  let modality_claims ~intf impls =
    match impls with
    | [] -> intf
    | claimed :: others ->
      List.fold_left (Mode.Modality.Const.diff intf claimed) ~init:intf
        ~f:(fun agreed (Mode.Modality.Atom (axis, value)) ->
          let shared =
            List.for_all others ~f:(fun impl ->
                let value' = Mode.Modality.Const.proj axis impl in
                Mode.Modality.Per_axis.le axis value value'
                && Mode.Modality.Per_axis.le axis value' value)
          in
          if shared then Mode.Modality.Const.set axis value agreed else agreed)

  let merge_modality_diff diffs =
    Option.bind (all_or_none diffs) ~f:(fun diffs ->
        match diffs with
        | [] -> None
        | (~impl:_, ~intf) :: _ ->
          let impls = List.map diffs ~f:(fun (~impl, ~intf:_) -> impl) in
          Some (~impl:(modality_claims ~intf impls), ~intf))

  let merge_arrow_diffs per_subject =
    match per_subject with
    | [] -> []
    | claimed :: others ->
      List.filter_map claimed ~f:(fun (diff : Intf_strengthen.arrow_diff) ->
          let at_same_position (other : Intf_strengthen.arrow_diff) =
            other.path.dir = diff.path.dir
          in
          let matching =
            List.map others ~f:(fun diffs ->
                List.find_some diffs ~f:at_same_position)
          in
          Option.map (all_or_none matching) ~f:(fun matching ->
              let impls =
                diff.impl
                :: List.map matching ~f:(fun (d : Intf_strengthen.arrow_diff) ->
                       d.impl)
              in
              { diff with impl = alloc_claims ~intf:diff.intf impls }))

  let merge_diffs (diffs : Abstract.diff list) =
    match diffs with
    | [] -> None
    | Kind_annotation annotation :: others ->
      let shared =
        List.for_all others ~f:(fun (diff : Abstract.diff) ->
            match diff with
            | Kind_annotation annotation' ->
              String.equal annotation annotation'
            | Mode_diffs _ -> false)
      in
      if shared then Some (Abstract.Kind_annotation annotation) else None
    | Mode_diffs { modality_diff; arrow_diffs } :: others ->
      let others =
        all_or_none
          (List.map others ~f:(fun (diff : Abstract.diff) ->
               match diff with
               | Mode_diffs { modality_diff; arrow_diffs } ->
                 Some (modality_diff, arrow_diffs)
               | Kind_annotation _ -> None))
      in
      Option.map others ~f:(fun others ->
          Abstract.Mode_diffs
            { modality_diff =
                merge_modality_diff
                  (modality_diff :: List.map others ~f:fst);
              arrow_diffs =
                merge_arrow_diffs (arrow_diffs :: List.map others ~f:snd)
            })

  let agreed (per_subject : Abstract.t list list) =
    match per_subject with
    | [] -> []
    | claimed :: others ->
      List.filter_map claimed ~f:(fun (strengthening : Abstract.t) ->
          let key = Decl_key.of_loc strengthening.decl_loc in
          let matching =
            List.map others ~f:(fun strengthenings ->
                match
                  List.filter strengthenings ~f:(fun (other : Abstract.t) ->
                      Decl_key.of_loc other.decl_loc = key)
                with
                | [ (other : Abstract.t) ] -> Some other.diff
                | [] | _ :: _ :: _ -> None)
          in
          Option.bind (all_or_none matching) ~f:(fun matching ->
              Option.map (merge_diffs (strengthening.diff :: matching))
                ~f:(fun diff -> { strengthening with diff })))
end

module Actions = struct
  let edit_key (edit : Intf_weakness.text_edit) =
    ( Filename.basename edit.edit_loc.Location.loc_start.Lexing.pos_fname,
      span_start edit.edit_loc,
      span_end edit.edit_loc,
      edit.edit_text )

  let dedup edits =
    List.rev
      (List.fold_left edits ~init:[] ~f:(fun kept edit ->
           if List.exists kept ~f:(fun kept -> edit_key kept = edit_key edit)
           then kept
           else edit :: kept))

  let by_interface (actions : Intf_weakness.code_action list) =
    let files =
      List.sort_uniq ~cmp:compare
        (List.map actions ~f:(fun (action : Intf_weakness.code_action) ->
             action.intf_file))
    in
    List.map files ~f:(fun intf_file ->
        let edits =
          List.concat_map actions
            ~f:(fun (action : Intf_weakness.code_action) ->
              if String.equal action.intf_file intf_file then action.edits
              else [])
        in
        { Intf_weakness.intf_file; edits = dedup edits })
end

type interface = {
  intf_file : string;
  intf : Parsetree.signature
}

let analyze_subject ~pipeline ~config ~parsetree ~unit_name work subject =
  with_typed_subject ~pipeline ~config subject
    ~f:(fun ~config:subject_config ~env ~impl_sig ->
      match work.target with
      | Unit_interface ->
        Option.map (unit_interface subject_config subject.file)
          ~f:(fun (intf_file, intf, intf_sig) ->
            ( { intf_file; intf },
              Intf_strengthen.analyze ~env ~impl_sig ~intf_sig () ))
      | Module_type { name; decl_span_start } ->
        Option.bind (module_type_body parsetree ~decl_span_start)
          ~f:(fun intf ->
            Option.map (module_type_signature ~env ~unit_name name)
              ~f:(fun intf_sig ->
                ( { intf_file = Helpers.own_file config; intf },
                  Intf_strengthen.analyze ~env ~impl_sig ~intf_sig () ))))

let actions_for_work ~pipeline ~config ~parsetree ~unit_name work =
  let analyzed =
    List.map work.subjects
      ~f:(analyze_subject ~pipeline ~config ~parsetree ~unit_name work)
  in
  match all_or_none analyzed with
  | None ->
    log ~title:"actions_for_work" "%d subjects, one unanalyzable"
      (List.length work.subjects);
    []
  | Some [] -> []
  | Some ((interface, _) :: _ as analyzed) ->
    let merged = Merge.agreed (List.map analyzed ~f:snd) in
    Intf_strengthen.render ~intf_file:interface.intf_file ~intf:interface.intf
      merged

let code_actions ~pipeline typedtree =
  let config = Mpipeline.final_config pipeline in
  let response = Module_type_impls.query ~pipeline typedtree in
  match works_of_response config response with
  | Unusable reason ->
    log ~title:"code_actions" "nothing can be strengthened: %s" reason;
    []
  | Works works ->
    let works = with_own_unit config typedtree works in
    let parsetree = Mpipeline.reader_parsetree pipeline in
    let unit_name = Mconfig.unitname config in
    let actions =
      Fun.protect
        ~finally:(fun () -> Mocaml.setup_typer_config config)
        (fun () ->
          List.concat_map works
            ~f:(actions_for_work ~pipeline ~config ~parsetree ~unit_name))
    in
    Actions.by_interface actions
