open Std

module Facts = Module_implementation_facts

module Dependency_analysis : sig
  type t

  type impact = { witness : Facts.Key.t; check : Facts.Check.t }

  type result = { impacts : impact list; omissions : Facts.Omission.t list }

  val create : Facts.t -> t

  val query_family : t -> Shape.Uid.t -> result

  val global_omissions : t -> Facts.Omission.t list
end = struct
  module Context = Facts.Context
  module Key = Facts.Key
  module Uid = Shape.Uid
  module Ctx_map = Map.Make (Context)
  module Int_set = Set.Make (Int)

  type context_node =
    | Atomic
    | Application of int * int
    | Projection of int * Uid.t

  module Congruence_key = struct
    type t = Apply of int * int | Project of int * Uid.t

    let compare a b =
      match (a, b) with
      | Apply (f1, a1), Apply (f2, a2) ->
        let c = Int.compare f1 f2 in
        if c <> 0 then c else Int.compare a1 a2
      | Apply _, Project _ -> -1
      | Project _, Apply _ -> 1
      | Project (c1, u1), Project (c2, u2) ->
        let c = Int.compare c1 c2 in
        if c <> 0 then c else Uid.compare u1 u2
  end

  module Congruence_map = Map.Make (Congruence_key)

  module Key_repr = struct
    type t = Named of int * Uid.t | Anon of Uid.t

    let compare a b =
      match (a, b) with
      | Named (c1, u1), Named (c2, u2) ->
        let c = Int.compare c1 c2 in
        if c <> 0 then c else Uid.compare u1 u2
      | Named _, Anon _ -> -1
      | Anon _, Named _ -> 1
      | Anon u1, Anon u2 -> Uid.compare u1 u2
  end

  module Key_map = Map.Make (Key_repr)

  type t =
    { context_nodes : context_node Dynarray.t;
      parent : int Dynarray.t;
      rank : int Dynarray.t;
      class_label : int Dynarray.t;
      uses : int list Dynarray.t;
      use_size : int Dynarray.t;
      mutable atoms : int Ctx_map.t;
      mutable congruences : int Congruence_map.t;
      mutable key_ids : int Key_map.t;
      key_witness : Key.t Dynarray.t;
      key_family : Uid.t option Dynarray.t;
      key_checks : Facts.Check.t list Dynarray.t;
      key_out : int list Dynarray.t;
      mutable families : Int_set.t Uid.Map.t;
      mutable global_omissions : Facts.Omission.t list;
      mutable family_omissions : Facts.Omission.t list Uid.Map.t;
      mutable comp_of : int array;
      mutable comp_keys : int list array;
      mutable comp_out : int list array;
      mutable comp_count : int
    }

  let find t i =
    let i = ref i in
    while Dynarray.get t.parent !i <> !i do
      let p = Dynarray.get t.parent !i in
      Dynarray.set t.parent !i (Dynarray.get t.parent p);
      i := p
    done;
    !i

  let new_context_node t context_node =
    let id = Dynarray.length t.context_nodes in
    Dynarray.add_last t.context_nodes context_node;
    Dynarray.add_last t.parent id;
    Dynarray.add_last t.rank 0;
    Dynarray.add_last t.class_label id;
    Dynarray.add_last t.uses [];
    Dynarray.add_last t.use_size 0;
    id

  let add_use t root parent_node =
    Dynarray.set t.uses root (parent_node :: Dynarray.get t.uses root);
    Dynarray.set t.use_size root (Dynarray.get t.use_size root + 1)

  let class_label t i = Dynarray.get t.class_label (find t i)

  let congruence_key_of_node t i =
    match Dynarray.get t.context_nodes i with
    | Atomic -> None
    | Application (f, a) ->
      Some (Congruence_key.Apply (class_label t f, class_label t a))
    | Projection (c, u) -> Some (Congruence_key.Project (class_label t c, u))

  let merge t a b =
    let pending = Queue.create () in
    Queue.add (a, b) pending;
    while not (Queue.is_empty pending) do
      let a, b = Queue.take pending in
      let ra = find t a and rb = find t b in
      if ra <> rb then begin
        let rep, absorbed =
          if Dynarray.get t.rank ra >= Dynarray.get t.rank rb then (ra, rb)
          else (rb, ra)
        in
        if Dynarray.get t.rank rep = Dynarray.get t.rank absorbed then
          Dynarray.set t.rank rep (Dynarray.get t.rank rep + 1);
        let big, small =
          if Dynarray.get t.use_size ra >= Dynarray.get t.use_size rb then
            (ra, rb)
          else (rb, ra)
        in
        let surviving_label = Dynarray.get t.class_label big in
        let moved = Dynarray.get t.uses small in
        let combined = List.rev_append moved (Dynarray.get t.uses big) in
        let total = Dynarray.get t.use_size ra + Dynarray.get t.use_size rb in
        Dynarray.set t.parent absorbed rep;
        Dynarray.set t.uses ra [];
        Dynarray.set t.uses rb [];
        Dynarray.set t.use_size ra 0;
        Dynarray.set t.use_size rb 0;
        Dynarray.set t.uses rep combined;
        Dynarray.set t.use_size rep total;
        Dynarray.set t.class_label rep surviving_label;
        List.iter moved ~f:(fun p ->
            match congruence_key_of_node t p with
            | None -> ()
            | Some key -> (
              match Congruence_map.find_opt key t.congruences with
              | Some q -> if find t q <> find t p then Queue.add (p, q) pending
              | None -> t.congruences <- Congruence_map.add key p t.congruences))
      end
    done

  let rec intern t (context : Context.t) =
    match context with
    | Def _ | Body _ | Site _ -> (
      match Ctx_map.find_opt context t.atoms with
      | Some id -> id
      | None ->
        let id = new_context_node t Atomic in
        t.atoms <- Ctx_map.add context id t.atoms;
        id)
    | App (functor_, argument) -> (
      let f = intern t functor_ in
      let a = intern t argument in
      let key = Congruence_key.Apply (class_label t f, class_label t a) in
      match Congruence_map.find_opt key t.congruences with
      | Some id -> id
      | None ->
        let id = new_context_node t (Application (f, a)) in
        add_use t (find t f) id;
        add_use t (find t a) id;
        t.congruences <- Congruence_map.add key id t.congruences;
        id)
    | Proj (inner, uid) -> (
      let c = intern t inner in
      let key = Congruence_key.Project (class_label t c, uid) in
      match Congruence_map.find_opt key t.congruences with
      | Some id -> id
      | None ->
        let id = new_context_node t (Projection (c, uid)) in
        add_use t (find t c) id;
        t.congruences <- Congruence_map.add key id t.congruences;
        id)

  let key_repr t (key : Key.t) : Key_repr.t =
    match key with
    | Named { context; family_uid } ->
      Named (find t (intern t context), family_uid)
    | Anon { key_uid } -> Anon key_uid

  let key_id t (key : Key.t) =
    let repr = key_repr t key in
    match Key_map.find_opt repr t.key_ids with
    | Some id ->
      if Key.compare key (Dynarray.get t.key_witness id) < 0 then
        Dynarray.set t.key_witness id key;
      id
    | None ->
      let id = Dynarray.length t.key_witness in
      t.key_ids <- Key_map.add repr id t.key_ids;
      Dynarray.add_last t.key_witness key;
      Dynarray.add_last t.key_family (Key.family key);
      Dynarray.add_last t.key_checks [];
      Dynarray.add_last t.key_out [];
      id

  let observe_family t id =
    match Dynarray.get t.key_family id with
    | None -> ()
    | Some family ->
      t.families <-
        Uid.Map.update family
          (fun ids ->
            let ids = Option.value ids ~default:Int_set.empty in
            Some (Int_set.add id ids))
          t.families

  let build_condensation t =
    let n = Dynarray.length t.key_out in
    let visit_index = Array.make n (-1) in
    let lowlink = Array.make n 0 in
    let on_stack = Array.make n false in
    let comp_of = Array.make n (-1) in
    let scc_stack = ref [] in
    let next_index = ref 0 in
    let comp_count = ref 0 in
    let start v =
      visit_index.(v) <- !next_index;
      lowlink.(v) <- !next_index;
      incr next_index;
      scc_stack := v :: !scc_stack;
      on_stack.(v) <- true
    in
    for root = 0 to n - 1 do
      if visit_index.(root) = -1 then begin
        start root;
        let frames = ref [ (root, Dynarray.get t.key_out root) ] in
        while !frames <> [] do
          match !frames with
          | [] -> ()
          | (v, edges) :: rest -> (
            match edges with
            | w :: edges ->
              frames := (v, edges) :: rest;
              if visit_index.(w) = -1 then begin
                start w;
                frames := (w, Dynarray.get t.key_out w) :: !frames
              end
              else if on_stack.(w) then
                lowlink.(v) <- min lowlink.(v) visit_index.(w)
            | [] ->
              frames := rest;
              (match rest with
              | (parent_v, _) :: _ ->
                lowlink.(parent_v) <- min lowlink.(parent_v) lowlink.(v)
              | [] -> ());
              if lowlink.(v) = visit_index.(v) then begin
                let comp = !comp_count in
                incr comp_count;
                let continue = ref true in
                while !continue do
                  match !scc_stack with
                  | [] -> continue := false
                  | w :: remaining ->
                    scc_stack := remaining;
                    on_stack.(w) <- false;
                    comp_of.(w) <- comp;
                    if w = v then continue := false
                done
              end)
        done
      end
    done;
    let comp_keys = Array.make !comp_count [] in
    for id = n - 1 downto 0 do
      let c = comp_of.(id) in
      comp_keys.(c) <- id :: comp_keys.(c)
    done;
    let comp_out = Array.make !comp_count [] in
    for id = 0 to n - 1 do
      let c = comp_of.(id) in
      List.iter (Dynarray.get t.key_out id) ~f:(fun derived ->
          let d = comp_of.(derived) in
          if c <> d then comp_out.(c) <- d :: comp_out.(c))
    done;
    for c = 0 to !comp_count - 1 do
      let out = List.sort_uniq ~cmp:Int.compare comp_out.(c) in
      comp_out.(c) <- out
    done;
    t.comp_of <- comp_of;
    t.comp_keys <- comp_keys;
    t.comp_out <- comp_out;
    t.comp_count <- !comp_count

  let empty () =
    { context_nodes = Dynarray.create ();
      parent = Dynarray.create ();
      rank = Dynarray.create ();
      class_label = Dynarray.create ();
      uses = Dynarray.create ();
      use_size = Dynarray.create ();
      atoms = Ctx_map.empty;
      congruences = Congruence_map.empty;
      key_ids = Key_map.empty;
      key_witness = Dynarray.create ();
      key_family = Dynarray.create ();
      key_checks = Dynarray.create ();
      key_out = Dynarray.create ();
      families = Uid.Map.empty;
      global_omissions = [];
      family_omissions = Uid.Map.empty;
      comp_of = [||];
      comp_keys = [||];
      comp_out = [||];
      comp_count = 0
    }

  let merge_equalities t equalities =
    Facts.Context_equality.Set.iter
      (fun ({ left; right } : Facts.Context_equality.t) ->
        merge t (intern t left) (intern t right))
      equalities

  let index_checks t checks =
    Facts.Check.Set.iter
      (fun (check : Facts.Check.t) ->
        let id = key_id t check.expectation in
        Dynarray.set t.key_checks id (check :: Dynarray.get t.key_checks id);
        observe_family t id)
      checks

  let index_dependencies t dependencies =
    Facts.Dependency.Set.iter
      (fun ({ derived; source; reason } : Facts.Dependency.t) ->
        let derived_id = key_id t derived in
        let source_id = key_id t source in
        Dynarray.set t.key_out source_id
          (derived_id :: Dynarray.get t.key_out source_id);
        observe_family t derived_id;
        match reason with
        | Definition ->
          Dynarray.set t.key_out derived_id
            (source_id :: Dynarray.get t.key_out derived_id);
          observe_family t source_id
        | Instance -> ()
        | Alias
        | Include
        | With_constraint
        | Destructive_substitution
        | Module_type_of
        | Strengthening
        | Functor_type
        | Argument_member
        | Interface -> observe_family t source_id)
      dependencies

  let index_omissions t omissions =
    Facts.Omission.Set.iter
      (fun (omission : Facts.Omission.t) ->
        (match omission.affected with
        | None -> ()
        | Some affected -> observe_family t (key_id t affected));
        match omission.source with
        | None -> t.global_omissions <- omission :: t.global_omissions
        | Some family ->
          t.family_omissions <-
            Uid.Map.update family
              (fun omissions ->
                let omissions = Option.value omissions ~default:[] in
                Some (omission :: omissions))
              t.family_omissions)
      omissions

  let normalize_edges t =
    for id = 0 to Dynarray.length t.key_out - 1 do
      Dynarray.set t.key_out id
        (List.sort_uniq ~cmp:Int.compare (Dynarray.get t.key_out id))
    done

  let create (facts : Facts.t) =
    let t = empty () in
    merge_equalities t facts.equalities;
    index_checks t facts.checks;
    index_dependencies t facts.dependencies;
    index_omissions t facts.omissions;
    normalize_edges t;
    build_condensation t;
    t

  type impact = { witness : Key.t; check : Facts.Check.t }

  type result = { impacts : impact list; omissions : Facts.Omission.t list }

  let scoped_omissions t families =
    let omissions =
      Uid.Set.fold
        (fun family omissions ->
          match Uid.Map.find_opt family t.family_omissions with
          | None -> omissions
          | Some scoped -> List.rev_append scoped omissions)
        families t.global_omissions
    in
    List.sort_uniq ~cmp:Facts.Omission.compare omissions

  let impact_compare a b =
    let c = Key.compare a.witness b.witness in
    if c <> 0 then c else Facts.Check.compare a.check b.check

  let query_seeds t ~queried_families seeds =
    let sets = Array.make t.comp_count Int_set.empty in
    let witnesses = Array.of_list (List.map seeds ~f:fst) in
    List.iteri seeds ~f:(fun w (_, id) ->
        let c = t.comp_of.(id) in
        sets.(c) <- Int_set.add w sets.(c));
    for c = t.comp_count - 1 downto 0 do
      if not (Int_set.is_empty sets.(c)) then
        List.iter t.comp_out.(c) ~f:(fun d ->
            sets.(d) <- Int_set.union sets.(d) sets.(c))
    done;
    let impacts = ref [] in
    let families = ref queried_families in
    for c = 0 to t.comp_count - 1 do
      let reaching = sets.(c) in
      if not (Int_set.is_empty reaching) then
        List.iter t.comp_keys.(c) ~f:(fun id ->
            (match Dynarray.get t.key_family id with
            | None -> ()
            | Some family -> families := Uid.Set.add family !families);
            List.iter (Dynarray.get t.key_checks id) ~f:(fun check ->
                Int_set.iter
                  (fun w ->
                    impacts := { witness = witnesses.(w); check } :: !impacts)
                  reaching))
    done;
    { impacts = List.sort_uniq ~cmp:impact_compare !impacts;
      omissions = scoped_omissions t !families
    }

  let query_family t family =
    let queried_families = Uid.Set.singleton family in
    match Uid.Map.find_opt family t.families with
    | None -> { impacts = []; omissions = scoped_omissions t queried_families }
    | Some ids ->
      let seeds =
        List.map (Int_set.elements ids) ~f:(fun id ->
            (Dynarray.get t.key_witness id, id))
      in
      query_seeds t ~queried_families seeds

  let global_omissions t =
    List.sort_uniq ~cmp:Facts.Omission.compare t.global_omissions
end

let { Logger.log } = Logger.for_section "module-type-impls"

module Helpers = struct
  let find_in_path_opt path filename =
    try Some (Misc.find_in_path_normalized path filename)
    with Not_found -> None

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
            | Some facts -> Facts.merge facts source))
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
      log ~title:"find_source_of_loc" "cannot find source for %s: %s"
        description reason;
      None

  let location_in_file file (loc : Location.t) =
    let with_file pos = { pos with Lexing.pos_fname = file } in
    { loc with
      loc_start = with_file loc.loc_start;
      loc_end = with_file loc.loc_end
    }
end

type target =
  { target_uid : Shape.Uid.t; target_name : string; target_loc : Location.t }

let module_type_decls (typedtree : Mtyper.typedtree) : target list =
  let targets = ref [] in
  let path = ref [] in
  let within_path_component name ~f =
    let previous_path = !path in
    path := name :: previous_path;
    Fun.protect ~finally:(fun () -> path := previous_path) f
  in
  let iterator =
    { Tast_iterator.default_iterator with
      module_binding =
        (fun iterator (mb : Typedtree.module_binding) ->
          match mb.mb_name.txt with
          | None -> Tast_iterator.default_iterator.module_binding iterator mb
          | Some module_name ->
            within_path_component module_name ~f:(fun () ->
                Tast_iterator.default_iterator.module_binding iterator mb));
      module_declaration =
        (fun iterator (md : Typedtree.module_declaration) ->
          match md.md_name.txt with
          | None ->
            Tast_iterator.default_iterator.module_declaration iterator md
          | Some module_name ->
            within_path_component module_name ~f:(fun () ->
                Tast_iterator.default_iterator.module_declaration iterator md));
      module_type_declaration =
        (fun iterator (mtd : Typedtree.module_type_declaration) ->
          let name = mtd.mtd_name.txt in
          targets :=
            { target_uid = mtd.mtd_uid;
              target_name = String.concat ~sep:"." (List.rev (name :: !path));
              target_loc = mtd.mtd_loc
            }
            :: !targets;
          within_path_component name ~f:(fun () ->
              Tast_iterator.default_iterator.module_type_declaration iterator
                mtd))
    }
  in
  (match typedtree with
  | `Implementation str -> iterator.structure iterator str
  | `Interface sg -> iterator.signature iterator sg);
  List.rev !targets

type uid_site =
  { uid : Shape.Uid.t;
    name : string;
    spans_file : string;
    row_file : string;
    loc : Location.t
  }

let source_of_recorded_location mconfig ~unit_name ~description
    (loc : Location.t) =
  let recorded = loc.loc_start.Lexing.pos_fname in
  if String.equal recorded "" then None
  else
    let candidate =
      if Filename.is_relative recorded then
        match unit_name with
        | Some unit_name when String.equal unit_name (Mconfig.unitname mconfig)
          -> Some (Filename.concat mconfig.query.directory recorded)
        | None | Some _ -> None
      else Some recorded
    in
    match candidate with
    | Some candidate when Sys.file_exists candidate ->
      let file = Misc.canonicalize_filename candidate in
      Some (file, Helpers.location_in_file file loc)
    | Some _ | None ->
      Option.map (Helpers.find_source_of_loc mconfig ~description loc)
        ~f:(fun (file, loc) -> (file, Helpers.location_in_file file loc))

let source_of_site mconfig compilation_unit loc =
  let unit_name = Compilation_unit.full_path_as_string compilation_unit in
  source_of_recorded_location mconfig ~unit_name:(Some unit_name)
    ~description:unit_name loc

let uid_site (mconfig : Mconfig.t) (evidence_uid : Shape.Uid.t) =
  match evidence_uid with
  | Item { from; _ } ->
    Option.bind (Locate.lookup_uid_loc_of_decl ~config:mconfig evidence_uid)
      ~f:(fun { Location.txt = name; loc } ->
        let description = Format.asprintf "%a" Shape.Uid.print evidence_uid in
        Option.bind (Helpers.find_source_of_loc mconfig ~description loc)
          ~f:(fun (spans_file, loc) ->
            let loc = Helpers.location_in_file spans_file loc in
            match from with
            | Unit_info.Impl ->
              Some
                { uid = evidence_uid;
                  name;
                  spans_file;
                  row_file = spans_file;
                  loc
                }
            | Unit_info.Intf ->
              Option.map (Helpers.impl_source_of_interface mconfig spans_file)
                ~f:(fun row_file ->
                  { uid = evidence_uid; name; spans_file; row_file; loc })))
  | Compilation_unit _ | Internal | Predef _ | Unboxed_version _ -> None

let own_interface_implementation mconfig ~own_file
    (typedtree : Mtyper.typedtree) =
  let open Query_protocol.Module_type_impls in
  match typedtree with
  | `Implementation _ -> None
  | `Interface _ ->
    Option.map (Helpers.impl_source_of_interface mconfig own_file)
      ~f:(fun impl_file ->
        { target = Own_interface;
          target_loc = None;
          instance = None;
          implementation_uid = None;
          implementation_name = None;
          site =
            { impl_loc = Location.in_file impl_file; impl_kind = Whole_unit };
          check = None;
          check_site = None
        })

let has_precise_location (loc : Location.t) =
  (not loc.loc_ghost)
  && (not (Location.is_none loc))
  && loc.loc_start.Lexing.pos_cnum <> -1
  && loc.loc_end.Lexing.pos_cnum <> -1

type resolved_implementation =
  { implementation_uid : string option;
    implementation_name : string option;
    site : Query_protocol.Module_type_impls.impl_site
  }

let resolve_implementation mconfig (node : Facts.Node.t) =
  let open Query_protocol.Module_type_impls in
  match node with
  | Location (compilation_unit, loc) ->
    let impl_kind =
      if has_precise_location loc then Annotation_sites else Whole_unit
    in
    Option.map (source_of_site mconfig compilation_unit loc)
      ~f:(fun (file, loc) ->
        let impl_loc =
          match impl_kind with
          | Whole_unit -> Location.in_file file
          | Annotation_sites -> loc
        in
        { implementation_uid = None;
          implementation_name = None;
          site = { impl_loc; impl_kind }
        })
  | Uid uid ->
    Option.bind (uid_site mconfig uid)
      ~f:(fun { uid; name; spans_file; row_file; loc } ->
        if loc.Location.loc_ghost then None
        else
          let site =
            if String.equal spans_file row_file then
              { impl_loc = loc; impl_kind = Annotation_sites }
            else
              { impl_loc = Location.in_file row_file; impl_kind = Whole_unit }
          in
          Some
            { implementation_uid =
                Some (Format.asprintf "%a" Shape.Uid.print uid);
              implementation_name =
                if String.equal name "_" then None else Some name;
              site
            })

let resolve_check_site mconfig (check : Facts.Check.t) =
  if Location.is_none check.site then None
  else
    match check.implementation with
    | Location (compilation_unit, _) ->
      Option.map (source_of_site mconfig compilation_unit check.site)
        ~f:(fun (_, loc) -> loc)
    | Uid uid ->
      let unit_name =
        match uid with
        | Item { comp_unit; _ } -> Some comp_unit
        | Compilation_unit _ | Internal | Predef _ | Unboxed_version _ -> None
      in
      let description = Format.asprintf "%a" Shape.Uid.print uid in
      Option.map
        (source_of_recorded_location mconfig ~unit_name ~description check.site)
        ~f:(fun (_, loc) -> loc)

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
  Format.asprintf "%s:%d:%d-%d:%d" start.Lexing.pos_fname start.pos_lnum
    (start.pos_cnum - start.pos_bol)
    finish.Lexing.pos_lnum
    (finish.pos_cnum - finish.pos_bol)

type site_resolution =
  | No_recorded_site
  | Resolved_site of Location.t
  | Unresolved_site

type impact_resolution =
  | Resolved_impact of
      Query_protocol.Module_type_impls.implementation
      * Query_protocol.Module_type_impls.reason list
  | Unresolved_impact of Query_protocol.Module_type_impls.reason list

let compare_target left right =
  let open Query_protocol.Module_type_impls in
  match (left, right) with
  | Own_interface, Own_interface -> 0
  | Own_interface, Modtype _ -> -1
  | Modtype _, Own_interface -> 1
  | Modtype left, Modtype right -> String.compare left right

let compare_impl_kind left right =
  let open Query_protocol.Module_type_impls in
  match (left, right) with
  | Whole_unit, Whole_unit | Annotation_sites, Annotation_sites -> 0
  | Whole_unit, Annotation_sites -> -1
  | Annotation_sites, Whole_unit -> 1

let compare_check_kind left right =
  let rank : Query_protocol.Module_type_impls.check_kind -> int = function
    | Annotation -> 0
    | Argument -> 1
    | Package -> 2
    | Interface -> 3
  in
  Int.compare (rank left) (rank right)

let compare_implementation
    (left : Query_protocol.Module_type_impls.implementation)
    (right : Query_protocol.Module_type_impls.implementation) =
  let c = compare_target left.target right.target in
  if c <> 0 then c
  else
    let c =
      Stdlib.Option.compare Location.compare left.target_loc right.target_loc
    in
    if c <> 0 then c
    else
      let c =
        Stdlib.Option.compare String.compare left.instance right.instance
      in
      if c <> 0 then c
      else
        let c =
          Stdlib.Option.compare String.compare left.implementation_uid
            right.implementation_uid
        in
        if c <> 0 then c
        else
          let c =
            Stdlib.Option.compare String.compare left.implementation_name
              right.implementation_name
          in
          if c <> 0 then c
          else
            let c = Location.compare left.site.impl_loc right.site.impl_loc in
            if c <> 0 then c
            else
              let c =
                compare_impl_kind left.site.impl_kind right.site.impl_kind
              in
              if c <> 0 then c
              else
                let c =
                  Stdlib.Option.compare compare_check_kind left.check
                    right.check
                in
                if c <> 0 then c
                else
                  Stdlib.Option.compare Location.compare left.check_site
                    right.check_site

let reason_rank : Query_protocol.Module_type_impls.reason -> int = function
  | No_index_files -> 0
  | Channel_absent -> 1
  | Reader_problem _ -> 2
  | Omission _ -> 3
  | Unresolved_implementation _ -> 4
  | Unresolved_check_site _ -> 5

let compare_reason left right =
  let open Query_protocol.Module_type_impls in
  match (left, right) with
  | No_index_files, No_index_files | Channel_absent, Channel_absent -> 0
  | Reader_problem left, Reader_problem right -> String.compare left right
  | ( Omission { family = left_family; reason = left_reason },
      Omission { family = right_family; reason = right_reason } ) ->
    let c = Stdlib.Option.compare String.compare left_family right_family in
    if c <> 0 then c else String.compare left_reason right_reason
  | Unresolved_implementation left, Unresolved_implementation right ->
    let c = String.compare left.target right.target in
    if c <> 0 then c
    else
      let c = String.compare left.witness right.witness in
      if c <> 0 then c
      else
        let c = String.compare left.implementation right.implementation in
        if c <> 0 then c
        else Stdlib.Option.compare String.compare left.site right.site
  | Unresolved_check_site left, Unresolved_check_site right ->
    let c = String.compare left.target right.target in
    if c <> 0 then c
    else
      let c = String.compare left.witness right.witness in
      if c <> 0 then c else String.compare left.site right.site
  | _, _ -> Int.compare (reason_rank left) (reason_rank right)

let pp_node fmt (node : Facts.Node.t) =
  match node with
  | Uid uid -> Shape.Uid.print fmt uid
  | Location (compilation_unit, _) ->
    Format.fprintf fmt "%s"
      (Compilation_unit.full_path_as_string compilation_unit)

let check_site_resolution mconfig (check : Facts.Check.t) =
  if Location.is_none check.site then No_recorded_site
  else
    match resolve_check_site mconfig check with
    | Some loc -> Resolved_site loc
    | None -> Unresolved_site

let resolve_impact ~mconfig ~own_file target
    ({ witness; check } : Dependency_analysis.impact) =
  let open Query_protocol.Module_type_impls in
  let witness = render_witness witness in
  let site_resolution = check_site_resolution mconfig check in
  let site_reasons =
    match site_resolution with
    | No_recorded_site | Resolved_site _ -> []
    | Unresolved_site ->
      [ Unresolved_check_site
          { target = target.target_name;
            witness;
            site = render_site check.site
          }
      ]
  in
  match resolve_implementation mconfig check.implementation with
  | Some { implementation_uid; implementation_name; site } ->
    Resolved_impact
      ( { target = Modtype target.target_name;
          target_loc =
            Some (Helpers.location_in_file own_file target.target_loc);
          instance = Some witness;
          implementation_uid;
          implementation_name;
          site;
          check = Some (protocol_check_kind check.kind);
          check_site =
            (match site_resolution with
            | Resolved_site loc -> Some loc
            | No_recorded_site | Unresolved_site -> None)
        },
        site_reasons )
  | None ->
    let site =
      match site_resolution with
      | Resolved_site loc -> Some loc.loc_start.Lexing.pos_fname
      | No_recorded_site | Unresolved_site -> None
    in
    Unresolved_impact
      (Unresolved_implementation
         { target = target.target_name;
           witness;
           implementation = Format.asprintf "%a" pp_node check.implementation;
           site
         }
      :: site_reasons)

let resolve_impacts ~mconfig ~own_file results =
  let resolutions =
    List.concat_map results
      ~f:(fun ((target, result) : target * Dependency_analysis.result) ->
        List.map result.impacts ~f:(resolve_impact ~mconfig ~own_file target))
  in
  let implementations =
    List.filter_map resolutions ~f:(function
      | Resolved_impact (implementation, _) -> Some implementation
      | Unresolved_impact _ -> None)
    |> List.sort_uniq ~cmp:compare_implementation
  in
  let reasons =
    List.concat_map resolutions ~f:(function
        | Resolved_impact (_, reasons) | Unresolved_impact reasons -> reasons)
    |> List.sort_uniq ~cmp:compare_reason
  in
  (implementations, reasons)

let status_and_reasons ~index_files
    ~(reader_status : Module_facts_reader.status) ~omissions ~resolution_reasons
    =
  let open Query_protocol.Module_type_impls in
  let reader_reasons =
    List.map reader_status.problems ~f:(fun problem ->
        Reader_problem
          (Format.asprintf "%a" Module_facts_reader.pp_problem problem))
  in
  let channel_reasons =
    if reader_status.facts_present then [] else [ Channel_absent ]
  in
  match index_files with
  | [] -> (Unavailable, [ No_index_files ])
  | _ :: _ when reader_status.channels_loaded = 0 ->
    (Unavailable, channel_reasons @ reader_reasons)
  | _ :: _ -> (
    let reasons =
      channel_reasons @ reader_reasons
      @ List.map omissions ~f:reason_of_omission
      @ resolution_reasons
    in
    match reasons with
    | [] -> (Complete, [])
    | _ :: _ -> (Partial, reasons))

let query_results engine targets =
  List.map targets ~f:(fun target ->
      let result =
        match engine with
        | None ->
          ({ impacts = []; omissions = [] } : Dependency_analysis.result)
        | Some engine ->
          Dependency_analysis.query_family engine target.target_uid
      in
      (target, result))

let query_omissions engine results =
  match (engine, results) with
  | None, _ -> []
  | Some engine, [] -> Dependency_analysis.global_omissions engine
  | Some _, _ :: _ ->
    List.concat_map results
      ~f:(fun ((_, result) : target * Dependency_analysis.result) ->
        result.omissions)
    |> List.sort_uniq ~cmp:Facts.Omission.compare

let query ~pipeline (typedtree : Mtyper.typedtree) =
  let open Query_protocol.Module_type_impls in
  let mconfig = Mpipeline.final_config pipeline in
  let own_file = Helpers.own_file mconfig in
  let targets = module_type_decls typedtree in
  let index_files = mconfig.merlin.index_files in
  let facts, reader_status = Helpers.module_facts mconfig in
  let engine = Option.map facts ~f:Dependency_analysis.create in
  let results = query_results engine targets in
  let omissions = query_omissions engine results in
  let implementations, resolution_reasons =
    resolve_impacts ~mconfig ~own_file results
  in
  let implementations =
    match own_interface_implementation mconfig ~own_file typedtree with
    | None -> implementations
    | Some own_interface ->
      List.sort_uniq ~cmp:compare_implementation
        (own_interface :: implementations)
  in
  let status, reasons =
    status_and_reasons ~index_files ~reader_status ~omissions
      ~resolution_reasons
  in
  log ~title:"query" "%d targets, %d rows, status %s" (List.length targets)
    (List.length implementations)
    (match status with
    | Complete -> "complete"
    | Partial -> "partial"
    | Unavailable -> "unavailable");
  { status; reasons; implementations }
