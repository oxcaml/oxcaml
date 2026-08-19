module Kind = Shape.Sig_component_kind
open Index_format

let with_root ?root file =
  match root with
  | None -> file
  | Some root -> Filename.concat root file

let add_root ~root (lid : Longident.t Location.loc) =
  match root with
  | None -> lid
  | Some root ->
    let pos_fname = Filename.concat root lid.loc.loc_start.pos_fname in
    { lid with
      loc =
        { lid.loc with
          loc_start = { lid.loc.loc_start with pos_fname };
          loc_end = { lid.loc.loc_end with pos_fname }
        }
    }

let merge m m' =
  Uid_map.union (fun _uid locs locs' -> Some (Lid_set.union locs locs')) m m'

let add_one uid lid map =
  let lid = Lid.of_lid lid in
  Uid_map.update uid
    (function
      | None -> Some (Lid_set.singleton lid)
      | Some set -> Some (Lid_set.add lid set))
    map

(** Cmt files contains a table of declarations' Uids associated to a typedtree
    fragment. [add_locs_from_fragments] gather locations from these *)
let gather_locs_from_fragments ~root ~rewrite_root map fragments =
  let to_located_lid (name : string Location.loc) =
    { name with txt = Longident.Lident name.txt }
  in
  let add_loc uid fragment acc =
    match fragment with
    | None -> acc
    | Some lid ->
      let lid = to_located_lid lid in
      let lid = if rewrite_root then add_root ~root lid else lid in
      add_one uid lid acc
  in
  Shape.Uid.Tbl.fold add_loc fragments map

module Reduce_conf (Loaded_shapes : sig
  val shapes : (Compilation_unit.t, Shape.t) Hashtbl.t
end) =
struct
  let fuel () = Misc.Maybe_bounded.of_int 10

  let try_load ~unit_name () =
    match
      Hashtbl.find_opt Loaded_shapes.shapes
        (Compilation_unit.of_string unit_name)
    with
    | Some shape ->
      Log.debug "Used loaded shape for %s" unit_name;
      Some shape
    | None -> begin
      let artifact =
        let cms = Format.sprintf "%s.cms" unit_name in
        match Locate.Artifact.read (Load_path.find_normalized cms) with
        | artifact ->
          Log.debug "Loaded CMS %s" cms;
          Some artifact
        | exception Not_found -> (
          let cmt = Format.sprintf "%s.cmt" unit_name in
          match Locate.Artifact.read (Load_path.find_normalized cmt) with
          | artifact ->
            Log.debug "Loaded CMT %s" cmt;
            Some artifact
          | exception Not_found ->
            Log.warn "Failed to load file %S in load_path: @[%s@]\n%!" cmt
            @@ String.concat "; " (Load_path.get_path_list ());
            None)
      in
      match artifact with
      | None -> None
      | Some artifact -> Merlin_analysis.Locate.Artifact.impl_shape artifact
      end

  let read_unit_shape ~diagnostics:_ ~unit_name =
    Log.debug "Read unit shape: %s\n%!" unit_name;
    try_load ~unit_name ()

  let projection_rules_for_merlin_enabled = true
  let fuel_for_compilation_units () : Misc.Maybe_bounded.t = Unbounded
  let max_shape_reduce_steps_per_variable () : Misc.Maybe_bounded.t = Unbounded
  let max_compilation_unit_depth () : Misc.Maybe_bounded.t = Unbounded
end

let init_load_path_once ~do_not_use_cmt_loadpath =
  let loaded = ref false in
  fun ~(dirs : Load_path.paths) cmt_loadpath ->
    if not !loaded then (
      let cmt_visible, cmt_hidden =
        if do_not_use_cmt_loadpath then ([], [])
        else (cmt_loadpath.Load_path.visible, cmt_loadpath.Load_path.hidden)
      in
      let visible = List.concat [ cmt_visible; dirs.visible ] in
      let hidden = List.concat [ cmt_hidden; dirs.hidden ] in
      Load_path.(init ~auto_include:no_auto_include ~visible ~hidden);
      loaded := true)

let add_root_loc ~root (loc : Location.t) =
  if Location.is_none loc then loc
  else
    match root with
    | None -> loc
    | Some root ->
      let reroot (pos : Lexing.position) =
        { pos with pos_fname = Filename.concat root pos.pos_fname }
      in
      { loc with
        loc_start = reroot loc.loc_start;
        loc_end = reroot loc.loc_end
      }

let rewrite_module_facts ~root ~rewrite_root
    (facts : Module_implementation_facts.t) =
  if not rewrite_root then facts
  else
    let open Module_implementation_facts in
    match root with
    | None -> ensure_normalized facts
    | Some _ ->
      let rewrite_node : Node.t -> Node.t = function
        | Node.Uid _ as node -> node
        | Node.Location (compilation_unit, loc) ->
          Node.Location (compilation_unit, add_root_loc ~root loc)
      in
      ensure_normalized
        { facts with
          checks =
            List.map
              (fun (check : Check.t) ->
                { check with
                  implementation = rewrite_node check.implementation;
                  site = add_root_loc ~root check.site
                })
              facts.checks
        }

let index_of_artifact ~into ~root ~rewrite_root ~build_path
    ~do_not_use_cmt_loadpath ~shapes ~store_shapes ~cmt_loadpath ~cmt_impl_shape
    ~cmt_modname ~uid_to_loc ~cmt_ident_occurrences ~cmt_initial_env
    ~cmt_sourcefile ~cmt_source_digest ~cmt_declaration_dependencies
    ~module_implementation_facts ~module_implementation_facts_present =
  init_load_path_once ~do_not_use_cmt_loadpath ~dirs:build_path cmt_loadpath;
  let module Reduce = Shape_reduce.Make (Reduce_conf (struct
    let shapes = shapes
  end))
  in
  let defs =
    gather_locs_from_fragments ~root ~rewrite_root into.defs uid_to_loc
  in
  (* The list [cmt_ident_occurrences] associate each ident usage location in the
     module with its (partially) reduced shape. We finish the reduction and
     group together all the locations that share the same definition uid. *)
  let defs, approximated =
    Array.fold_left
      (fun ((acc_defs, acc_apx) as acc) (lid, (item : Shape_reduce.result)) ->
        let lid = if rewrite_root then add_root ~root lid else lid in
        let resolved =
          match item with
          | Unresolved shape -> Reduce.reduce_for_uid cmt_initial_env shape
          | result -> result
        in
        match Locate.uid_of_result ~traverse_aliases:false resolved with
        | Some uid, false -> (add_one uid lid acc_defs, acc_apx)
        | Some uid, true -> (acc_defs, add_one uid lid acc_apx)
        | None, _ -> acc)
      (defs, into.approximated) cmt_ident_occurrences
  in
  let facts_run =
    rewrite_module_facts ~root ~rewrite_root module_implementation_facts
  in
  let cu_shape = into.cu_shape in
  if store_shapes then
    Option.iter (Hashtbl.add cu_shape cmt_modname) cmt_impl_shape;
  let stats =
    match cmt_sourcefile with
    | None -> into.stats
    | Some src -> (
      let src, preprocessed =
        match Locate.sourcefile_for_ppx_sourcefile src with
        | Some raw_src -> (raw_src, true)
        | None -> (src, false)
      in
      let rooted_src = with_root ?root src in
      try
        let stats = Unix.stat rooted_src in
        let source_digest =
          if preprocessed then Some (Digest.file rooted_src)
          else cmt_source_digest
        in
        let src = if rewrite_root then rooted_src else src in
        Stats.add src
          { mtime = stats.st_mtime; size = stats.st_size; source_digest }
          into.stats
      with Unix.Unix_error _ -> into.stats)
  in
  let related_uids =
    List.fold_left
      (fun acc (_, uid1, uid2) ->
        let union = Union_find.make (Uid_set.of_list [ uid1; uid2 ]) in
        let map_update uid =
          Uid_map.update uid (function
            | None -> Some union
            | Some union' -> Some (Union_find.union union' union))
        in
        acc |> map_update uid1 |> map_update uid2)
      into.related_uids cmt_declaration_dependencies
  in
  ( { defs;
      approximated;
      cu_shape;
      stats;
      related_uids;
      module_facts =
        if module_implementation_facts_present then
          Some (link_module_facts Module_facts_compact.empty)
        else into.module_facts;
      root_directory = into.root_directory
    },
    facts_run )

let shape_of_artifact ~impl_shape ~modname =
  let cu_shape = Hashtbl.create 1 in
  Option.iter (Hashtbl.add cu_shape modname) impl_shape;
  { defs = Uid_map.empty ();
    approximated = Uid_map.empty ();
    cu_shape;
    stats = Stats.empty;
    root_directory = None;
    related_uids = Uid_map.empty ();
    module_facts = None
  }

let shape_of_cmt { Cmt_format.cmt_impl_shape; cmt_modname; _ } =
  shape_of_artifact ~impl_shape:cmt_impl_shape ~modname:cmt_modname

let shape_of_cms { Cms_format.cms_impl_shape; cms_modname; _ } =
  shape_of_artifact ~impl_shape:cms_impl_shape ~modname:cms_modname

let index_of_cmt ~into ~root ~rewrite_root ~build_path ~do_not_use_cmt_loadpath
    ~shapes ~store_shapes cmt_infos =
  let { Cmt_format.cmt_loadpath;
        cmt_impl_shape;
        cmt_modname;
        cmt_uid_to_decl;
        cmt_ident_occurrences;
        cmt_initial_env;
        cmt_sourcefile;
        cmt_source_digest;
        cmt_declaration_dependencies;
        cmt_module_implementation_facts;
        cmt_module_implementation_facts_present;
        _
      } =
    cmt_infos
  in
  let uid_to_loc =
    Shape.Uid.Tbl.to_list cmt_uid_to_decl
    |> List.map (fun (uid, fragment) ->
        (uid, Typedtree_utils.location_of_declaration ~uid fragment))
    |> Shape.Uid.Tbl.of_list
  in
  index_of_artifact ~into ~root ~rewrite_root ~build_path
    ~do_not_use_cmt_loadpath ~shapes ~store_shapes ~cmt_loadpath ~cmt_impl_shape
    ~cmt_modname ~uid_to_loc ~cmt_ident_occurrences ~cmt_initial_env
    ~cmt_sourcefile ~cmt_source_digest ~cmt_declaration_dependencies
    ~module_implementation_facts:cmt_module_implementation_facts
    ~module_implementation_facts_present:cmt_module_implementation_facts_present

let index_of_cms ~into ~root ~rewrite_root ~build_path ~do_not_use_cmt_loadpath
    ~shapes ~store_shapes cms_infos =
  let { Cms_format.cms_impl_shape;
        cms_modname;
        cms_uid_to_loc;
        cms_ident_occurrences;
        cms_sourcefile;
        cms_source_digest;
        cms_initial_env;
        cms_declaration_dependencies;
        cms_module_implementation_facts;
        cms_module_implementation_facts_present;
        _
      } =
    cms_infos
  in
  let uid_to_loc =
    Shape.Uid.Tbl.to_list cms_uid_to_loc
    |> List.map (fun (uid, l) -> (uid, Some l))
    |> Shape.Uid.Tbl.of_list
  in
  index_of_artifact ~into ~root ~rewrite_root ~build_path
    ~do_not_use_cmt_loadpath ~shapes ~store_shapes
    ~cmt_loadpath:{ visible = []; hidden = [] }
    ~cmt_impl_shape:cms_impl_shape ~cmt_modname:cms_modname ~uid_to_loc
    ~cmt_ident_occurrences:cms_ident_occurrences
    ~cmt_initial_env:(Option.value cms_initial_env ~default:Env.empty)
    ~cmt_sourcefile:cms_sourcefile ~cmt_source_digest:cms_source_digest
    ~cmt_declaration_dependencies:cms_declaration_dependencies
    ~module_implementation_facts:cms_module_implementation_facts
    ~module_implementation_facts_present:cms_module_implementation_facts_present

let facts_of_index_input ~file (index : index) =
  match index.module_facts with
  | None -> (Module_implementation_facts.empty, false)
  | Some module_facts -> (
    match Module_facts_compact.to_facts (module_facts_block module_facts) with
    | Ok facts -> (facts, true)
    | Error message ->
      Log.error "Cannot read the module facts of %s: %s" file message;
      (Module_implementation_facts.empty, false)
    | exception exn ->
      Log.error "Cannot read the module facts of %s: %s" file
        (Printexc.to_string exn);
      (Module_implementation_facts.empty, false))

let read_index_input_uncached ~file =
  match read ~file with
  | Index index -> Some index
  | Cmt _ | Cms _ | Unknown -> None
  | exception exn ->
    Log.error "Cannot read %s: %s" file (Printexc.to_string exn);
    None

let merge_index ~store_shapes ~into index =
  let defs = merge index.defs into.defs in
  let approximated = merge index.approximated into.approximated in
  let stats = Stats.union (fun _ f1 _f2 -> Some f1) into.stats index.stats in
  let related_uids =
    Uid_map.union
      (fun _ a b -> Some (Union_find.union a b))
      index.related_uids into.related_uids
  in
  if store_shapes then
    Hashtbl.add_seq into.cu_shape (Hashtbl.to_seq index.cu_shape);
  { into with
    defs;
    approximated;
    stats;
    related_uids;
    module_facts =
      (match into.module_facts with
      | Some _ -> into.module_facts
      | None -> index.module_facts)
  }

let from_files ~store_shapes ~output_file ~root ~rewrite_root ~build_path
    ~do_not_use_cmt_loadpath files =
  Log.debug "Debug log is enabled";
  let initial_index =
    { defs = Uid_map.empty ();
      approximated = Uid_map.empty ();
      cu_shape = Hashtbl.create 64;
      stats = Stats.empty;
      root_directory = root;
      related_uids = Uid_map.empty ();
      module_facts = None
    }
  in
  let final_index, facts_runs =
    Ocaml_utils.Local_store.with_store (Ocaml_utils.Local_store.fresh ())
    @@ fun () ->
    List.fold_left
      (fun (into, facts_runs) file ->
        let store_shapes =
          (* Merlin-jst: We add the shapes into `into` because we need to collect them so
             we can use them for shape reduction, regardless of whether store_shapes is
             true. So we shadow the [store_shapes] that's passed into [from_files].

             Q: Why don't we just explicitly pass [true] in the usages below rather than
                doing this shadowing?
             A: So that when we merge changes from upstream, we're more likely to do the
                right thing. *)
          true
        in
        Log.debug "Indexing from file: %s" file;
        match Cms_cache.read file with
        | cms_item ->
          let index, facts_run =
            index_of_cms ~into ~root ~rewrite_root ~build_path ~store_shapes
              ~do_not_use_cmt_loadpath ~shapes:into.cu_shape cms_item.cms_infos
          in
          (index, facts_run :: facts_runs)
        | exception _ -> (
          match Cmt_cache.read file with
          | cmt_item ->
            let index, facts_run =
              index_of_cmt ~into ~root ~rewrite_root ~build_path ~store_shapes
                ~do_not_use_cmt_loadpath ~shapes:into.cu_shape
                cmt_item.cmt_infos
            in
            (index, facts_run :: facts_runs)
          | exception _ -> (
            match read_index_input_uncached ~file with
            | Some index ->
              let facts_run, decoded = facts_of_index_input ~file index in
              ( merge_index ~store_shapes
                  { index with
                    module_facts = if decoded then index.module_facts else None
                  }
                  ~into,
                facts_run :: facts_runs )
            | None ->
              Log.error "Unknown file type: %s" file;
              exit 1)))
      (initial_index, []) files
  in
  (* The facts of every input are copied into the output, so that it stands
     alone even if its inputs are removed. *)
  let final_index =
    { final_index with
      module_facts =
        Option.map
          (fun _ ->
            inline_module_facts
              (Module_implementation_facts.merge_many (List.rev facts_runs)))
          final_index.module_facts
    }
  in
  let final_index =
    (* Don't save the collected shapes if store_shapes is false *)
    if store_shapes then final_index
    else { final_index with cu_shape = Hashtbl.create 0 }
  in
  write ~file:output_file final_index

let gather_shapes ~output_file files =
  let initial_index =
    { defs = Uid_map.empty ();
      approximated = Uid_map.empty ();
      cu_shape = Hashtbl.create 64;
      stats = Stats.empty;
      root_directory = None;
      related_uids = Uid_map.empty ();
      module_facts = None
    }
  in
  let final_index, facts_runs =
    List.fold_left
      (fun ((into, facts_runs) as acc) file ->
        match Cache.read file with
        | Cmt cmt_infos ->
          ( merge_index ~store_shapes:true (shape_of_cmt cmt_infos) ~into,
            facts_runs )
        | Cms cms_infos ->
          ( merge_index ~store_shapes:true (shape_of_cms cms_infos) ~into,
            facts_runs )
        | Index _ -> (
          (* Read the index again without the cache: the facts are a lazy link
             into the file, so they must be decoded from a live channel. *)
          match read_index_input_uncached ~file with
          | Some index ->
            let facts_run, decoded = facts_of_index_input ~file index in
            ( merge_index ~store_shapes:true
                { index with
                  module_facts = if decoded then index.module_facts else None
                }
                ~into,
              facts_run :: facts_runs )
          | None ->
            Log.error "Not a valid file %S" file;
            acc)
        | Unknown | (exception _) ->
          Log.error "Not a valid file %S" file;
          acc)
      (initial_index, []) files
  in
  let final_index =
    { final_index with
      module_facts =
        Option.map
          (fun _ ->
            inline_module_facts
              (Module_implementation_facts.merge_many (List.rev facts_runs)))
          final_index.module_facts
    }
  in
  write ~file:output_file final_index
