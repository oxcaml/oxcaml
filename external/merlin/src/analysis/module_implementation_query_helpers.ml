open Std

let { Logger.log } = Logger.for_section "module-implementation-query"

let find_in_path_opt path filename =
  try Some (Misc.find_in_path_normalized path filename) with Not_found -> None

let external_config_for_file (mconfig : Mconfig.t) path =
  Mconfig.get_external_config path
    { mconfig with
      query =
        { mconfig.query with
          filename = Filename.basename path;
          directory = Filename.dirname path
        }
    }

let impl_source_of_interface (mconfig : Mconfig.t) intf_file =
  let config = external_config_for_file mconfig intf_file in
  let unit = unitname (Filename.basename intf_file) in
  find_in_path_opt (Mconfig.source_path config) (unit ^ ".ml")
  |> Option.map ~f:Misc.canonicalize_filename

let module_facts (mconfig : Mconfig.t) =
  let index_files = mconfig.merlin.index_files in
  let facts, status =
    Module_facts_reader.fold ~index_files ~init:None
      ~f:(fun facts ~path:_ source ->
        Some
          (match facts with
          | None -> source
          | Some facts -> Module_implementation_facts.merge facts source))
  in
  List.iter status.problems ~f:(fun problem ->
      log ~title:"module_facts" "%a" Logger.fmt (fun fmt ->
          Module_facts_reader.pp_problem fmt problem));
  (facts, status)

let own_file (mconfig : Mconfig.t) =
  Misc.canonicalize_filename
    (Filename.concat mconfig.query.directory mconfig.query.filename)

let find_source_of_loc mconfig ~description loc =
  match Locate.find_source ~config:mconfig loc description with
  | `Found (file, loc) -> Some (Misc.canonicalize_filename file, loc)
  | `File_not_found reason ->
    log ~title:"find_source_of_loc" "cannot find source for %s: %s" description
      reason;
    None

let location_in_file file (loc : Location.t) =
  let with_file pos = { pos with Lexing.pos_fname = file } in
  { loc with
    loc_start = with_file loc.loc_start;
    loc_end = with_file loc.loc_end
  }
