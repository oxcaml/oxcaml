open Std
module Lid_set = Index_format.Lid_set
let { Logger.log } = Logger.for_section "index-occurrences"

let set_fname ~file (loc : Location.t) =
  let pos_fname = file in
  { loc with
    loc_start = { loc.loc_start with pos_fname };
    loc_end = { loc.loc_end with pos_fname }
  }

let decl_of_path_or_lid env namespace path lid =
  match (namespace : Shape.Sig_component_kind.t) with
  | Constructor -> begin
    match Env.find_constructor_by_name lid env with
    | exception Not_found -> None
    | { cstr_uid; cstr_loc; _ } ->
      Some { Env_lookup.uid = cstr_uid; loc = cstr_loc; namespace }
  end
  | Label -> begin
    match Env.find_label_by_name Legacy lid env with
    | exception Not_found -> None
    | { lbl_uid; lbl_loc; _ } ->
      Some { Env_lookup.uid = lbl_uid; loc = lbl_loc; namespace }
  end
  | Unboxed_label -> begin
    match Env.find_label_by_name Unboxed_product lid env with
    | exception Not_found -> None
    | { lbl_uid; lbl_loc; _ } ->
      Some { Env_lookup.uid = lbl_uid; loc = lbl_loc; namespace }
  end
  | _ -> Env_lookup.by_path path namespace env

let should_ignore_lid (lid : Longident.t Location.loc) =
  (* Ignore occurrence if the location of the identifier is "none" because there is not a
     useful location to report to the user. This can occur when the occurrence is in ppx
     generated code and the ppx does not give location information.

     An alternative implementation could instead ignore the occurrence if the location is
     marked as "ghost". However, this seems too aggressive for two reasons:
      - The expression being bound in a punned let expression is marked as ghost
      - Ppx-generated code is often "ghost", but occurrences within ppx-generated code may
        be useful
  *)
  Location.is_none lid.loc

let iterator ~current_buffer_path ~index ~reduce_for_uid =
  let add uid loc = index := Shape.Uid.Map.add_to_list uid loc !index in
  let f ~namespace env path (lid : Longident.t Location.loc) =
    log ~title:"index_buffer" "Path: %a" Logger.fmt (Fun.flip Path.print path);
    let lid = { lid with loc = set_fname ~file:current_buffer_path lid.loc } in
    let index_decl () =
      begin
        match decl_of_path_or_lid env namespace path lid.txt with
        | (exception _) | None ->
          log ~title:"index_buffer" "Declaration not found"
        | Some decl ->
          log ~title:"index_buffer" "Found declaration: %a" Logger.fmt
            (Fun.flip Location.print_loc decl.loc);
          add decl.uid lid
      end
    in
    if not (should_ignore_lid lid) then
      match Env.shape_of_path ~namespace env path with
      | exception Not_found -> ()
      | path_shape ->
        log ~title:"index_buffer" "Shape of path: %a" Logger.fmt
          (Fun.flip Shape.print path_shape);
        let result = reduce_for_uid env path_shape in
        begin
          match Locate.uid_of_result ~traverse_aliases:false result with
          | Some uid, false ->
            log ~title:"index_buffer" "Found %a (%a) wiht uid %a" Logger.fmt
              (Fun.flip Pprintast.longident lid.txt)
              Logger.fmt
              (Fun.flip Location.print_loc lid.loc)
              Logger.fmt
              (Fun.flip Shape.Uid.print uid);
            add uid lid
          | Some uid, true ->
            log ~title:"index_buffer" "Shape is approximative, found uid: %a"
              Logger.fmt
              (Fun.flip Shape.Uid.print uid);
            index_decl ()
          | None, _ ->
            log ~title:"index_buffer" "Reduction failed: missing uid";
            index_decl ()
        end
  in
  Ast_iterators.iterator_on_usages ~include_hidden:true ~f

let items index (config : Mconfig.t) items =
  let module Shape_reduce = Shape_reduce.Make (struct
    let fuel () = Misc_stdlib.Maybe_bounded.of_int 10

    let read_unit_shape ~diagnostics:_ ~unit_name =
      log ~title:"read_unit_shape" "inspecting %s" unit_name;
      let read unit_name =
        let cms = Format.sprintf "%s.cms" unit_name in
        match Locate.Artifact.read (Load_path.find_normalized cms) with
        | artifact -> Some artifact
        | exception _ -> (
          let cmt = Format.sprintf "%s.cmt" unit_name in
          match Locate.Artifact.read (Load_path.find_normalized cmt) with
          | artifact -> Some artifact
          | exception _ -> None)
      in
      match read unit_name with
      | Some artifact ->
        log ~title:"read_unit_shape" "shapes loaded for %s" unit_name;
        Locate.Artifact.impl_shape artifact
      | None ->
        log ~title:"read_unit_shape" "failed to find %s" unit_name;
        None

    let projection_rules_for_merlin_enabled = true
    let fuel_for_compilation_units () : Misc_stdlib.Maybe_bounded.t = Unbounded
    let max_shape_reduce_steps_per_variable () : Misc_stdlib.Maybe_bounded.t =
      Unbounded
    let max_compilation_unit_depth () : Misc_stdlib.Maybe_bounded.t = Unbounded
  end) in
  let current_buffer_path =
    Filename.concat config.query.directory config.query.filename
  in
  let reduce_for_uid = Shape_reduce.reduce_for_uid in
  let index = ref index in
  let iterator = iterator ~current_buffer_path ~index ~reduce_for_uid in
  let () =
    match items with
    | `Impl items -> List.iter ~f:(iterator.structure_item iterator) items
    | `Intf items -> List.iter ~f:(iterator.signature_item iterator) items
  in
  !index
