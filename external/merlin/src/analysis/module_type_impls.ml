open Std

let { Logger.log } = Logger.for_section "module-type-impls"

module Facts = Module_implementation_facts
module Helpers = Module_implementation_query_helpers

type target =
  { target_uid : Shape.Uid.t; target_name : string; target_loc : Location.t }

let module_type_decls (typedtree : Mtyper.typedtree) : target list =
  let targets = ref [] in
  let path = ref [] in
  let within_module module_name ~f =
    let previous_path = !path in
    path := module_name :: previous_path;
    Fun.protect ~finally:(fun () -> path := previous_path) f
  in
  let iterator =
    { Tast_iterator.default_iterator with
      module_binding =
        (fun iterator (mb : Typedtree.module_binding) ->
          match mb.mb_name.txt with
          | None -> Tast_iterator.default_iterator.module_binding iterator mb
          | Some module_name ->
            within_module module_name ~f:(fun () ->
                Tast_iterator.default_iterator.module_binding iterator mb));
      module_declaration =
        (fun iterator (md : Typedtree.module_declaration) ->
          match md.md_name.txt with
          | None ->
            Tast_iterator.default_iterator.module_declaration iterator md
          | Some module_name ->
            within_module module_name ~f:(fun () ->
                Tast_iterator.default_iterator.module_declaration iterator md));
      module_type_declaration =
        (fun iterator (mtd : Typedtree.module_type_declaration) ->
          targets :=
            { target_uid = mtd.mtd_uid;
              target_name =
                String.concat ~sep:"." (List.rev (mtd.mtd_name.txt :: !path));
              target_loc = mtd.mtd_loc
            }
            :: !targets;
          Tast_iterator.default_iterator.module_type_declaration iterator mtd)
    }
  in
  (match typedtree with
  | `Implementation str -> iterator.structure iterator str
  | `Interface sg -> iterator.signature iterator sg);
  List.rev !targets

type uid_site = { spans_file : string; row_file : string; loc : Location.t }

let uid_site (mconfig : Mconfig.t) (evidence_uid : Shape.Uid.t) =
  let source_and_loc =
    Option.bind (Locate.lookup_uid_loc_of_decl ~config:mconfig evidence_uid)
      ~f:(fun { Location.loc; _ } ->
        match evidence_uid with
        | Item _ ->
          let description = Format.asprintf "%a" Shape.Uid.print evidence_uid in
          Helpers.find_source_of_loc mconfig ~description loc
        | Compilation_unit _ | Internal | Predef _ | Unboxed_version _ -> None)
  in
  Option.bind source_and_loc ~f:(fun (spans_file, loc) ->
      match evidence_uid with
      | Item { from = Unit_info.Impl; _ } ->
        Some { spans_file; row_file = spans_file; loc }
      | Item { from = Unit_info.Intf; _ } ->
        Option.map (Helpers.impl_source_of_interface mconfig spans_file)
          ~f:(fun row_file -> { spans_file; row_file; loc })
      | Compilation_unit _ | Internal | Predef _ | Unboxed_version _ -> None)

type implementer =
  { loc : Location.t; kind : Query_protocol.Module_type_impls.impl_kind }

let own_interface_site mconfig (typedtree : Mtyper.typedtree) =
  let open Query_protocol.Module_type_impls in
  match typedtree with
  | `Implementation _ -> None
  | `Interface _ ->
    Option.map
      (Helpers.impl_source_of_interface mconfig (Helpers.own_file mconfig))
      ~f:(fun impl_file ->
        { impl_loc = Location.in_file impl_file; impl_kind = Whole_unit })

let source_of_site mconfig compilation_unit (loc : Location.t) =
  let recorded = loc.loc_start.Lexing.pos_fname in
  if String.equal recorded "" then None
  else
    let candidate =
      if Filename.is_relative recorded then
        if
          String.equal
            (Compilation_unit.full_path_as_string compilation_unit)
            (Mconfig.unitname mconfig)
        then Some (Filename.concat mconfig.query.directory recorded)
        else None
      else Some recorded
    in
    match candidate with
    | Some candidate when Sys.file_exists candidate ->
      let file = Misc.canonicalize_filename candidate in
      Some (file, Helpers.location_in_file file loc)
    | Some _ | None ->
      Helpers.find_source_of_loc mconfig
        ~description:(Compilation_unit.full_path_as_string compilation_unit)
        loc

let resolve_implementation mconfig (node : Facts.Node.t) =
  match node with
  | Location (compilation_unit, loc) ->
    let kind : Query_protocol.Module_type_impls.impl_kind =
      if loc.loc_ghost || Location.is_none loc then Whole_unit
      else if
        loc.loc_start.Lexing.pos_cnum = -1 || loc.loc_end.Lexing.pos_cnum = -1
      then Whole_unit
      else Annotation_sites
    in
    Option.map (source_of_site mconfig compilation_unit loc)
      ~f:(fun (file, loc) ->
        match kind with
        | Whole_unit -> { loc = Location.in_file file; kind }
        | Annotation_sites -> { loc = Helpers.location_in_file file loc; kind })
  | Uid uid ->
    Option.bind (uid_site mconfig uid) ~f:(fun { spans_file; row_file; loc } ->
        if loc.Location.loc_ghost then None
        else if String.equal spans_file row_file then
          Some
            ({ loc = Helpers.location_in_file row_file loc;
               kind = Annotation_sites
             }
              : implementer)
        else
          Some
            { loc = Location.in_file row_file;
              kind = Query_protocol.Module_type_impls.Whole_unit
            })

let resolve_check_site mconfig (check : Facts.Check.t) =
  if Location.is_none check.site then None
  else
    match check.implementation with
    | Location (compilation_unit, _) -> (
      match source_of_site mconfig compilation_unit check.site with
      | Some (file, loc) -> Some (Helpers.location_in_file file loc)
      | None -> None)
    | Uid uid ->
      let recorded = check.site.loc_start.Lexing.pos_fname in
      if String.equal recorded "" then None
      else if (not (Filename.is_relative recorded)) && Sys.file_exists recorded
      then begin
        let file = Misc.canonicalize_filename recorded in
        Some (Helpers.location_in_file file check.site)
      end
      else
        Option.map
          (Helpers.find_source_of_loc mconfig
             ~description:(Format.asprintf "%a" Shape.Uid.print uid)
             check.site)
          ~f:(fun (file, loc) -> Helpers.location_in_file file loc)

let protocol_check_kind :
    Facts.Check.Kind.t -> Query_protocol.Module_type_impls.check_kind = function
  | Annotation -> Annotation
  | Argument -> Argument
  | Package -> Package
  | Interface -> Interface

let string_of_omission_reason : Facts.Omission.Reason.t -> string = function
  | Unresolved_module_type -> "unresolved-module-type"
  | Unresolved_module -> "unresolved-module"
  | Unsupported_path -> "unsupported-path"
  | Missing_parameter_expectation -> "missing-parameter-expectation"

let reason_of_omission (omission : Facts.Omission.t) :
    Query_protocol.Module_type_impls.reason =
  Omission
    { family =
        Option.map omission.source ~f:(fun uid ->
            Format.asprintf "%a" Shape.Uid.print uid);
      reason = string_of_omission_reason omission.reason
    }

let render_witness (witness : Facts.Key.t) =
  Format.asprintf "%a" Facts.Key.print witness

let render_site (site : Location.t) =
  let start = site.loc_start in
  let finish = site.loc_end in
  Printf.sprintf "%s:%d:%d-%d:%d" start.Lexing.pos_fname start.pos_lnum
    (start.pos_cnum - start.pos_bol)
    finish.Lexing.pos_lnum
    (finish.pos_cnum - finish.pos_bol)

type row =
  { row_target : Query_protocol.Module_type_impls.target;
    row_target_loc : Location.t option;
    row_instance : string option;
    row_implementer : implementer;
    row_check : Query_protocol.Module_type_impls.check_kind option;
    row_check_site : Location.t option
  }

type site_resolution =
  | No_recorded_site
  | Resolved_site of Location.t
  | Unresolved_site

let query ~pipeline (typedtree : Mtyper.typedtree) =
  let open Query_protocol.Module_type_impls in
  let mconfig = Mpipeline.final_config pipeline in
  let targets = module_type_decls typedtree in
  let index_files = mconfig.merlin.index_files in
  let facts, reader_status = Helpers.module_facts mconfig in
  let engine = Option.map facts ~f:Module_change_impact.create in
  let results =
    List.map targets ~f:(fun target ->
        let result =
          match engine with
          | None ->
            ({ impacts = []; omissions = [] } : Module_change_impact.result)
          | Some engine ->
            Module_change_impact.query_family engine target.target_uid
        in
        (target, result))
  in
  let omissions =
    match (engine, targets) with
    | None, _ -> []
    | Some engine, [] -> Module_change_impact.global_omissions engine
    | Some _, _ :: _ ->
      List.concat_map results
        ~f:(fun ((_, result) : _ * Module_change_impact.result) ->
          result.omissions)
  in
  let rows, unresolved, unresolved_sites =
    List.fold_left results ~init:([], [], [])
      ~f:(fun acc ((target, result) : _ * Module_change_impact.result) ->
        List.fold_left result.impacts ~init:acc
          ~f:(fun
              (rows, unresolved, unresolved_sites)
              ({ witness; check } : Module_change_impact.impact)
            ->
            let site_resolution =
              if Location.is_none check.site then No_recorded_site
              else
                begin match resolve_check_site mconfig check with
                | Some loc -> Resolved_site loc
                | None -> Unresolved_site
                end
            in
            let unresolved_sites =
              match site_resolution with
              | No_recorded_site | Resolved_site _ -> unresolved_sites
              | Unresolved_site ->
                Unresolved_check_site
                  { target = target.target_name;
                    witness = render_witness witness;
                    site = render_site check.site
                  }
                :: unresolved_sites
            in
            match resolve_implementation mconfig check.implementation with
            | Some implementer ->
              ( { row_target = Modtype target.target_name;
                  row_target_loc =
                    Some
                      (Helpers.location_in_file (Helpers.own_file mconfig)
                         target.target_loc);
                  row_instance = Some (render_witness witness);
                  row_implementer = implementer;
                  row_check = Some (protocol_check_kind check.kind);
                  row_check_site =
                    (match site_resolution with
                    | Resolved_site loc -> Some loc
                    | No_recorded_site | Unresolved_site -> None)
                }
                :: rows,
                unresolved,
                unresolved_sites )
            | None ->
              ( rows,
                Unresolved_implementation
                  { target = target.target_name;
                    witness = render_witness witness;
                    implementation =
                      Format.asprintf "%a"
                        (fun fmt (node : Facts.Node.t) ->
                          match node with
                          | Uid uid -> Shape.Uid.print fmt uid
                          | Location (compilation_unit, _) ->
                            Format.fprintf fmt "%s"
                              (Compilation_unit.full_path_as_string
                                 compilation_unit))
                        check.implementation;
                    site =
                      (match site_resolution with
                      | Resolved_site loc -> Some loc.loc_start.Lexing.pos_fname
                      | No_recorded_site | Unresolved_site -> None)
                  }
                :: unresolved,
                unresolved_sites )))
  in
  let rows = List.sort_uniq ~cmp:compare rows in
  let unresolved = List.sort_uniq ~cmp:compare unresolved in
  let unresolved_sites = List.sort_uniq ~cmp:compare unresolved_sites in
  let own_interface_rows =
    match own_interface_site mconfig typedtree with
    | None -> []
    | Some site ->
      [ { row_target = Own_interface;
          row_target_loc = None;
          row_instance = None;
          row_implementer = { loc = site.impl_loc; kind = site.impl_kind };
          row_check = None;
          row_check_site = None
        }
      ]
  in
  let implementations =
    List.map (rows @ own_interface_rows) ~f:(fun row ->
        { target = row.row_target;
          target_loc = row.row_target_loc;
          instance = row.row_instance;
          site =
            { impl_loc = row.row_implementer.loc;
              impl_kind = row.row_implementer.kind
            };
          check = row.row_check;
          check_site = row.row_check_site
        })
  in
  let omissions = List.sort_uniq ~cmp:compare omissions in
  let reader_reasons =
    List.map reader_status.problems ~f:(fun problem ->
        Reader_problem
          (Format.asprintf "%a" Module_facts_reader.pp_problem problem))
  in
  let channel_reasons =
    if reader_status.facts_present then [] else [ Channel_absent ]
  in
  let status, reasons =
    if index_files = [] then (Unavailable, [ No_index_files ])
    else if
      (* No index contributed a facts channel, so there is nothing to answer
         from. This is decided from [channels_loaded] alone: an index whose
         facts decode to something while its channel is absent must not
         downgrade to [Partial], and a valid but empty channel still counts as
         loaded and so answers [Complete]. *)
      reader_status.channels_loaded = 0
    then (Unavailable, channel_reasons @ reader_reasons)
    else begin
      let reasons =
        channel_reasons @ reader_reasons
        @ List.map omissions ~f:reason_of_omission
        @ unresolved @ unresolved_sites
      in
      match reasons with
      | [] -> (Complete, [])
      | _ :: _ -> (Partial, reasons)
    end
  in
  log ~title:"query" "%d targets, %d rows, status %s" (List.length targets)
    (List.length implementations)
    (match status with
    | Complete -> "complete"
    | Partial -> "partial"
    | Unavailable -> "unavailable");
  { status; reasons; implementations }
