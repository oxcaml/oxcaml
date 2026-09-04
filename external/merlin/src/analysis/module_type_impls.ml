open Std

module Facts = Module_implementation_facts

module Implementation_search : sig
  type t

  type matching_check = { target_instance : Facts.Key.t; check : Facts.Check.t }

  type result =
    { matches : matching_check list; omissions : Facts.Omission.t list }

  val create : Facts.t -> t

  val find_for_family : t -> Shape.Uid.t -> result

  val find_for_anonymous_type : t -> Shape.Uid.t -> result
end = struct
  module Context = Facts.Context
  module Key = Facts.Key
  module Uid = Shape.Uid
  module Context_map = Map.Make (Context)
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
      context_parent : int Dynarray.t;
      context_rank : int Dynarray.t;
      context_class_label : int Dynarray.t;
      congruence_uses : int list Dynarray.t;
      congruence_use_count : int Dynarray.t;
      mutable atoms : int Context_map.t;
      mutable congruences : int Congruence_map.t;
      mutable key_ids : int Key_map.t;
      canonical_keys : Key.t Dynarray.t;
      family_by_key : Uid.t option Dynarray.t;
      checks_by_key : Facts.Check.t list Dynarray.t;
      requiring_keys : int list Dynarray.t;
      mutable keys_by_family : Int_set.t Uid.Map.t;
      mutable paired_families : Uid.Set.t Uid.Map.t;
      mutable global_omissions : Facts.Omission.t list;
      mutable family_omissions : Facts.Omission.t list Uid.Map.t;
      mutable component_by_key : int array;
      mutable component_keys : int list array;
      mutable requiring_components : int list array;
      mutable component_count : int
    }

  let find_context_root t i =
    let i = ref i in
    while Dynarray.get t.context_parent !i <> !i do
      let p = Dynarray.get t.context_parent !i in
      Dynarray.set t.context_parent !i (Dynarray.get t.context_parent p);
      i := p
    done;
    !i

  let new_context_node t context_node =
    let id = Dynarray.length t.context_nodes in
    Dynarray.add_last t.context_nodes context_node;
    Dynarray.add_last t.context_parent id;
    Dynarray.add_last t.context_rank 0;
    Dynarray.add_last t.context_class_label id;
    Dynarray.add_last t.congruence_uses [];
    Dynarray.add_last t.congruence_use_count 0;
    id

  let add_use t root parent_node =
    Dynarray.set t.congruence_uses root
      (parent_node :: Dynarray.get t.congruence_uses root);
    Dynarray.set t.congruence_use_count root
      (Dynarray.get t.congruence_use_count root + 1)

  let context_class_label t i =
    Dynarray.get t.context_class_label (find_context_root t i)

  let congruence_key_of_node t i =
    match Dynarray.get t.context_nodes i with
    | Atomic -> None
    | Application (f, a) ->
      Some
        (Congruence_key.Apply (context_class_label t f, context_class_label t a))
    | Projection (c, u) ->
      Some (Congruence_key.Project (context_class_label t c, u))

  let merge_contexts t a b =
    let pending = Queue.create () in
    Queue.add (a, b) pending;
    while not (Queue.is_empty pending) do
      let a, b = Queue.take pending in
      let ra = find_context_root t a and rb = find_context_root t b in
      if ra <> rb then begin
        let rep, absorbed =
          if Dynarray.get t.context_rank ra >= Dynarray.get t.context_rank rb
          then (ra, rb)
          else (rb, ra)
        in
        if
          Dynarray.get t.context_rank rep = Dynarray.get t.context_rank absorbed
        then
          Dynarray.set t.context_rank rep (Dynarray.get t.context_rank rep + 1);
        let big, small =
          if
            Dynarray.get t.congruence_use_count ra
            >= Dynarray.get t.congruence_use_count rb
          then (ra, rb)
          else (rb, ra)
        in
        let surviving_label = Dynarray.get t.context_class_label big in
        let moved = Dynarray.get t.congruence_uses small in
        let combined =
          List.rev_append moved (Dynarray.get t.congruence_uses big)
        in
        let total =
          Dynarray.get t.congruence_use_count ra
          + Dynarray.get t.congruence_use_count rb
        in
        Dynarray.set t.context_parent absorbed rep;
        Dynarray.set t.congruence_uses ra [];
        Dynarray.set t.congruence_uses rb [];
        Dynarray.set t.congruence_use_count ra 0;
        Dynarray.set t.congruence_use_count rb 0;
        Dynarray.set t.congruence_uses rep combined;
        Dynarray.set t.congruence_use_count rep total;
        Dynarray.set t.context_class_label rep surviving_label;
        List.iter moved ~f:(fun p ->
            match congruence_key_of_node t p with
            | None -> ()
            | Some key -> (
              match Congruence_map.find_opt key t.congruences with
              | Some q ->
                if find_context_root t q <> find_context_root t p then
                  Queue.add (p, q) pending
              | None -> t.congruences <- Congruence_map.add key p t.congruences))
      end
    done

  let rec intern_context t (context : Context.t) =
    match context with
    | Def _ | Body _ | Site _ -> (
      match Context_map.find_opt context t.atoms with
      | Some id -> id
      | None ->
        let id = new_context_node t Atomic in
        t.atoms <- Context_map.add context id t.atoms;
        id)
    | App (functor_, argument) -> (
      let f = intern_context t functor_ in
      let a = intern_context t argument in
      let key =
        Congruence_key.Apply (context_class_label t f, context_class_label t a)
      in
      match Congruence_map.find_opt key t.congruences with
      | Some id -> id
      | None ->
        let id = new_context_node t (Application (f, a)) in
        add_use t (find_context_root t f) id;
        add_use t (find_context_root t a) id;
        t.congruences <- Congruence_map.add key id t.congruences;
        id)
    | Proj (inner, uid) -> (
      let c = intern_context t inner in
      let key = Congruence_key.Project (context_class_label t c, uid) in
      match Congruence_map.find_opt key t.congruences with
      | Some id -> id
      | None ->
        let id = new_context_node t (Projection (c, uid)) in
        add_use t (find_context_root t c) id;
        t.congruences <- Congruence_map.add key id t.congruences;
        id)

  let key_repr t (key : Key.t) : Key_repr.t =
    match key with
    | Named { context; family_uid } ->
      Named (find_context_root t (intern_context t context), family_uid)
    | Anon uid -> Anon uid

  let key_id t (key : Key.t) =
    let repr = key_repr t key in
    match Key_map.find_opt repr t.key_ids with
    | Some id ->
      if Key.compare key (Dynarray.get t.canonical_keys id) < 0 then
        Dynarray.set t.canonical_keys id key;
      id
    | None ->
      let id = Dynarray.length t.canonical_keys in
      t.key_ids <- Key_map.add repr id t.key_ids;
      Dynarray.add_last t.canonical_keys key;
      Dynarray.add_last t.family_by_key (Key.family key);
      Dynarray.add_last t.checks_by_key [];
      Dynarray.add_last t.requiring_keys [];
      id

  let observe_family t id =
    match Dynarray.get t.family_by_key id with
    | None -> ()
    | Some family ->
      t.keys_by_family <-
        Uid.Map.update family
          (fun ids ->
            let ids = Option.value ids ~default:Int_set.empty in
            Some (Int_set.add id ids))
          t.keys_by_family

  let build_requirement_components t =
    let n = Dynarray.length t.requiring_keys in
    let visit_index = Array.make n (-1) in
    let lowlink = Array.make n 0 in
    let on_stack = Array.make n false in
    let component_by_key = Array.make n (-1) in
    let scc_stack = ref [] in
    let next_index = ref 0 in
    let component_count = ref 0 in
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
        let frames = ref [ (root, Dynarray.get t.requiring_keys root) ] in
        while !frames <> [] do
          match !frames with
          | [] -> ()
          | (v, edges) :: rest -> (
            match edges with
            | w :: edges ->
              frames := (v, edges) :: rest;
              if visit_index.(w) = -1 then begin
                start w;
                frames := (w, Dynarray.get t.requiring_keys w) :: !frames
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
                let comp = !component_count in
                incr component_count;
                let continue = ref true in
                while !continue do
                  match !scc_stack with
                  | [] -> continue := false
                  | w :: remaining ->
                    scc_stack := remaining;
                    on_stack.(w) <- false;
                    component_by_key.(w) <- comp;
                    if w = v then continue := false
                done
              end)
        done
      end
    done;
    let component_keys = Array.make !component_count [] in
    for id = n - 1 downto 0 do
      let c = component_by_key.(id) in
      component_keys.(c) <- id :: component_keys.(c)
    done;
    let requiring_components = Array.make !component_count [] in
    for id = 0 to n - 1 do
      let c = component_by_key.(id) in
      List.iter (Dynarray.get t.requiring_keys id) ~f:(fun derived ->
          let d = component_by_key.(derived) in
          if c <> d then
            requiring_components.(c) <- d :: requiring_components.(c))
    done;
    for c = 0 to !component_count - 1 do
      let out = List.sort_uniq ~cmp:Int.compare requiring_components.(c) in
      requiring_components.(c) <- out
    done;
    t.component_by_key <- component_by_key;
    t.component_keys <- component_keys;
    t.requiring_components <- requiring_components;
    t.component_count <- !component_count

  let empty () =
    { context_nodes = Dynarray.create ();
      context_parent = Dynarray.create ();
      context_rank = Dynarray.create ();
      context_class_label = Dynarray.create ();
      congruence_uses = Dynarray.create ();
      congruence_use_count = Dynarray.create ();
      atoms = Context_map.empty;
      congruences = Congruence_map.empty;
      key_ids = Key_map.empty;
      canonical_keys = Dynarray.create ();
      family_by_key = Dynarray.create ();
      checks_by_key = Dynarray.create ();
      requiring_keys = Dynarray.create ();
      keys_by_family = Uid.Map.empty;
      paired_families = Uid.Map.empty;
      global_omissions = [];
      family_omissions = Uid.Map.empty;
      component_by_key = [||];
      component_keys = [||];
      requiring_components = [||];
      component_count = 0
    }

  let merge_equalities t equalities =
    Facts.Context_equality_set.iter
      (fun equality ->
        merge_contexts t
          (intern_context t (Facts.Context_equality.left equality))
          (intern_context t (Facts.Context_equality.right equality)))
      equalities

  let index_checks t checks =
    Facts.Check_set.iter
      (fun (check : Facts.Check.t) ->
        let id = key_id t check.expectation in
        Dynarray.set t.checks_by_key id
          (check :: Dynarray.get t.checks_by_key id);
        observe_family t id)
      checks

  let index_requirement_edges t relations =
    Facts.Dependency_set.iter
      (fun ({ derived; source; reason } : Facts.Dependency.t) ->
        let derived_id = key_id t derived in
        let source_id = key_id t source in
        let add_requirement_edge () =
          Dynarray.set t.requiring_keys source_id
            (derived_id :: Dynarray.get t.requiring_keys source_id)
        in
        observe_family t derived_id;
        match reason with
        | Definition ->
          add_requirement_edge ();
          Dynarray.set t.requiring_keys derived_id
            (source_id :: Dynarray.get t.requiring_keys derived_id);
          observe_family t source_id
        | Instance -> add_requirement_edge ()
        | Alias
        | Include
        | With_constraint
        | Module_type_of
        | Strengthening
        | Interface ->
          add_requirement_edge ();
          observe_family t source_id
        | Destructive_substitution
        | Functor_type
        | Argument_member
        | Interface_member -> observe_family t source_id
        | Interface_pair -> (
          add_requirement_edge ();
          observe_family t source_id;
          match (Facts.Key.family derived, Facts.Key.family source) with
          | Some left, Some right when not (Uid.equal left right) ->
            let pair a b =
              let existing =
                match Uid.Map.find_opt a t.paired_families with
                | None -> Uid.Set.empty
                | Some set -> set
              in
              t.paired_families <-
                Uid.Map.add a (Uid.Set.add b existing) t.paired_families
            in
            pair left right;
            pair right left
          | (Some _ | None), _ -> ()))
      relations

  let index_omissions t omissions =
    Facts.Omission_set.iter
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
    for id = 0 to Dynarray.length t.requiring_keys - 1 do
      Dynarray.set t.requiring_keys id
        (List.sort_uniq ~cmp:Int.compare (Dynarray.get t.requiring_keys id))
    done

  let create (facts : Facts.t) =
    let t = empty () in
    merge_equalities t facts.equalities;
    index_checks t facts.checks;
    index_requirement_edges t facts.dependencies;
    index_omissions t facts.omissions;
    normalize_edges t;
    build_requirement_components t;
    t

  type matching_check = { target_instance : Key.t; check : Facts.Check.t }

  type result =
    { matches : matching_check list; omissions : Facts.Omission.t list }

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

  let compare_matching_check a b =
    let c = Key.compare a.target_instance b.target_instance in
    if c <> 0 then c else Facts.Check.compare a.check b.check

  let find_matching_checks t ~queried_families target_keys =
    let targets_by_component = Array.make t.component_count Int_set.empty in
    let target_instances = Array.of_list (List.map target_keys ~f:fst) in
    List.iteri target_keys ~f:(fun target_index (_, key_id) ->
        let component = t.component_by_key.(key_id) in
        targets_by_component.(component) <-
          Int_set.add target_index targets_by_component.(component));
    for component = t.component_count - 1 downto 0 do
      if not (Int_set.is_empty targets_by_component.(component)) then
        List.iter t.requiring_components.(component)
          ~f:(fun requiring_component ->
            targets_by_component.(requiring_component) <-
              Int_set.union
                targets_by_component.(requiring_component)
                targets_by_component.(component))
    done;
    let matches = ref [] in
    let reached_families = ref queried_families in
    for component = 0 to t.component_count - 1 do
      let matching_targets = targets_by_component.(component) in
      if not (Int_set.is_empty matching_targets) then
        List.iter t.component_keys.(component) ~f:(fun key_id ->
            (match Dynarray.get t.family_by_key key_id with
            | None -> ()
            | Some family ->
              reached_families := Uid.Set.add family !reached_families);
            List.iter (Dynarray.get t.checks_by_key key_id) ~f:(fun check ->
                Int_set.iter
                  (fun target_index ->
                    matches :=
                      { target_instance = target_instances.(target_index);
                        check
                      }
                      :: !matches)
                  matching_targets))
    done;
    { matches = List.sort_uniq ~cmp:compare_matching_check !matches;
      omissions = scoped_omissions t !reached_families
    }

  let find_for_family t family =
    let rec collect_paired_families pending families =
      match pending with
      | [] -> families
      | family :: pending ->
        if Uid.Set.mem family families then
          collect_paired_families pending families
        else
          let paired =
            match Uid.Map.find_opt family t.paired_families with
            | None -> []
            | Some set -> Uid.Set.elements set
          in
          collect_paired_families
            (List.rev_append paired pending)
            (Uid.Set.add family families)
    in
    let queried_families = collect_paired_families [ family ] Uid.Set.empty in
    let target_keys =
      Uid.Set.fold
        (fun family target_keys ->
          match Uid.Map.find_opt family t.keys_by_family with
          | None -> target_keys
          | Some ids ->
            List.rev_append
              (List.map (Int_set.elements ids) ~f:(fun id ->
                   (Dynarray.get t.canonical_keys id, id)))
              target_keys)
        queried_families []
    in
    match target_keys with
    | [] -> { matches = []; omissions = scoped_omissions t queried_families }
    | _ :: _ -> find_matching_checks t ~queried_families target_keys

  let find_for_anonymous_type t uid =
    let queried_families = Uid.Set.singleton uid in
    match Key_map.find_opt (Key_repr.Anon uid) t.key_ids with
    | None -> { matches = []; omissions = scoped_omissions t queried_families }
    | Some id ->
      find_matching_checks t ~queried_families
        [ (Dynarray.get t.canonical_keys id, id) ]
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
    List.fold_left index_files ~init:(Some None) ~f:(fun accumulator path ->
        match (accumulator, (Index_cache.read path).module_facts) with
        | None, _ | _, None -> None
        | Some facts, Some source ->
          let source = Index_format.fetch_module_facts source in
          Some
            (Some
               (match facts with
               | None -> source
               | Some facts -> Facts.union facts source)))

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

let impl_source_of_interface = Helpers.impl_source_of_interface
let own_file = Helpers.own_file

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

let uid_site (mconfig : Mconfig.t) ~local_defs (evidence_uid : Shape.Uid.t) =
  match evidence_uid with
  | Item { from; _ } ->
    let declaration =
      match
        Locate.lookup_loc_of_uid ~config:mconfig ~local_defs evidence_uid
      with
      | Some (`Declaration declaration) -> Some declaration
      | Some (`Compilation_unit _) | None -> None
      | exception Not_found -> None
    in
    Option.bind declaration ~f:(fun { Location.txt = name; loc } ->
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
          target_instance = None;
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

let resolve_implementation mconfig ~local_defs (node : Facts.Node.t) =
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
  | Uid (Compilation_unit unit_name as unit_uid) ->
    let unit_loc =
      match Locate.lookup_loc_of_uid ~config:mconfig ~local_defs unit_uid with
      | Some (`Compilation_unit loc) -> Some loc
      | Some (`Declaration _) | None -> None
      | exception Not_found -> None
    in
    Option.bind unit_loc ~f:(fun loc ->
        Option.map
          (Helpers.find_source_of_loc mconfig ~description:unit_name loc)
          ~f:(fun (file, _) ->
            { implementation_uid =
                Some (Format.asprintf "%a" Shape.Uid.print unit_uid);
              implementation_name = Some unit_name;
              site =
                { impl_loc = Location.in_file file; impl_kind = Whole_unit }
            }))
  | Uid uid ->
    Option.bind (uid_site mconfig ~local_defs uid)
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
                (if String.equal name "_" then None else Some name);
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
        | Compilation_unit unit_name -> Some unit_name
        | Internal | Predef _ | Unboxed_version _ -> None
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

let render_target_instance (target_instance : Facts.Key.t) =
  Format.asprintf "%a" Facts.Key.print target_instance

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

type matching_check_resolution =
  | Resolved_match of
      Query_protocol.Module_type_impls.implementation
      * Query_protocol.Module_type_impls.reason list
  | Unresolved_match of Query_protocol.Module_type_impls.reason list

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

let compare_implementation_identity
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
          else compare_impl_kind left.site.impl_kind right.site.impl_kind

let unique_implementations implementations =
  let compare left right =
    let c = compare_implementation_identity left right in
    if c <> 0 then c
    else Stdlib.Option.compare compare_check_kind left.check right.check
  in
  List.sort implementations ~cmp:compare
  |> List.fold_left ~init:[] ~f:(fun unique implementation ->
      match unique with
      | previous :: _
        when compare_implementation_identity previous implementation = 0 ->
        unique
      | _ -> implementation :: unique)
  |> List.rev

let reason_rank : Query_protocol.Module_type_impls.reason -> int = function
  | No_index_files -> 0
  | Channel_absent -> 1
  | Omission _ -> 2
  | Unresolved_implementation _ -> 3
  | Unresolved_check_site _ -> 4

let compare_reason left right =
  let open Query_protocol.Module_type_impls in
  match (left, right) with
  | No_index_files, No_index_files | Channel_absent, Channel_absent -> 0
  | ( Omission { family = left_family; reason = left_reason },
      Omission { family = right_family; reason = right_reason } ) ->
    let c = Stdlib.Option.compare String.compare left_family right_family in
    if c <> 0 then c else String.compare left_reason right_reason
  | Unresolved_implementation left, Unresolved_implementation right ->
    let c = String.compare left.target right.target in
    if c <> 0 then c
    else
      let c = String.compare left.target_instance right.target_instance in
      if c <> 0 then c
      else
        let c = String.compare left.implementation right.implementation in
        if c <> 0 then c
        else Stdlib.Option.compare String.compare left.site right.site
  | Unresolved_check_site left, Unresolved_check_site right ->
    let c = String.compare left.target right.target in
    if c <> 0 then c
    else
      let c = String.compare left.target_instance right.target_instance in
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

let resolve_matching_check ~mconfig ~local_defs ~own_file target
    ({ target_instance; check } : Implementation_search.matching_check) =
  let open Query_protocol.Module_type_impls in
  let target_instance = render_target_instance target_instance in
  let site_resolution = check_site_resolution mconfig check in
  let site_reasons =
    match site_resolution with
    | No_recorded_site | Resolved_site _ -> []
    | Unresolved_site ->
      [ Unresolved_check_site
          { target = target.target_name;
            target_instance;
            site = render_site check.site
          }
      ]
  in
  match resolve_implementation mconfig ~local_defs check.implementation with
  | Some { implementation_uid; implementation_name; site } ->
    Resolved_match
      ( { target = Modtype target.target_name;
          target_loc =
            Some (Helpers.location_in_file own_file target.target_loc);
          target_instance = Some target_instance;
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
    Unresolved_match
      (Unresolved_implementation
         { target = target.target_name;
           target_instance;
           implementation = Format.asprintf "%a" pp_node check.implementation;
           site
         }
      :: site_reasons)

let resolve_matching_checks ~mconfig ~local_defs ~own_file results =
  let resolutions =
    List.concat_map results
      ~f:(fun ((target, result) : target * Implementation_search.result) ->
        List.map result.matches
          ~f:(resolve_matching_check ~mconfig ~local_defs ~own_file target))
  in
  let implementations =
    List.filter_map resolutions ~f:(function
      | Resolved_match (implementation, _) -> Some implementation
      | Unresolved_match _ -> None)
    |> unique_implementations
  in
  let reasons =
    List.concat_map resolutions ~f:(function
        | Resolved_match (_, reasons) | Unresolved_match reasons -> reasons)
    |> List.sort_uniq ~cmp:compare_reason
  in
  (implementations, reasons)

let status_and_reasons ~index_files ~facts_present ~omissions
    ~resolution_reasons =
  let open Query_protocol.Module_type_impls in
  match (index_files, facts_present) with
  | [], _ -> (Unavailable, [ No_index_files ])
  | _ :: _, false -> (Unavailable, [ Channel_absent ])
  | _ :: _, true -> (
    let reasons =
      List.map omissions ~f:reason_of_omission @ resolution_reasons
    in
    match reasons with
    | [] -> (Complete, [])
    | _ :: _ -> (Partial, reasons))

let find_target_checks search targets =
  List.map targets ~f:(fun target ->
      let result =
        match search with
        | None ->
          ({ matches = []; omissions = [] } : Implementation_search.result)
        | Some search ->
          Implementation_search.find_for_family search target.target_uid
      in
      (target, result))

let query ~pipeline ?position (typedtree : Mtyper.typedtree) =
  let mconfig = Mpipeline.final_config pipeline in
  let own_file = Helpers.own_file mconfig in
  let targets = module_type_decls typedtree in
  let targets, buffer_wide =
    match position with
    | None -> (targets, true)
    | Some position -> (
      let enclosing =
        List.filter targets ~f:(fun (target : target) ->
            Location_aux.compare_pos position target.target_loc = 0)
      in
      match List.rev enclosing with
      | [] -> failwith "No module-type declaration at this position"
      | target :: _ -> ([ target ], false))
  in
  let open Query_protocol.Module_type_impls in
  let index_files = mconfig.merlin.index_files in
  let facts = Helpers.module_facts mconfig in
  let search =
    match facts with
    | None | Some None -> None
    | Some (Some facts) -> Some (Implementation_search.create facts)
  in
  let results = find_target_checks search targets in
  let targets, implementations =
    List.split
      (List.map results ~f:(fun ((target, result) as target_result) ->
           let implementations, resolution_reasons =
             resolve_matching_checks ~mconfig ~local_defs:typedtree ~own_file
               [ target_result ]
           in
           let status, reasons =
             status_and_reasons ~index_files
               ~facts_present:(Option.is_some facts) ~omissions:result.omissions
               ~resolution_reasons
           in
           ( { target = target.target_name;
               target_loc = Helpers.location_in_file own_file target.target_loc;
               status;
               reasons
             },
             implementations )))
  in
  let implementations = List.concat implementations |> unique_implementations in
  let own_interface_rows =
    match own_interface_implementation mconfig ~own_file typedtree with
    | Some own_interface -> [ own_interface ]
    | None -> (
      match (typedtree, search) with
      | `Implementation _, _ | _, None -> []
      | `Interface _, Some search ->
        let unit_uid =
          Shape.Uid.of_compilation_unit_id
            (Compilation_unit.of_string (Mconfig.unitname mconfig))
        in
        let result =
          Implementation_search.find_for_anonymous_type search unit_uid
        in
        List.filter_map result.matches
          ~f:(fun
              ({ target_instance; check } :
                Implementation_search.matching_check)
            ->
            Option.map
              (resolve_implementation mconfig ~local_defs:typedtree
                 check.implementation)
              ~f:(fun
                  ({ implementation_uid; implementation_name; site } :
                    resolved_implementation)
                ->
                { target = Own_interface;
                  target_loc = None;
                  target_instance =
                    Some (render_target_instance target_instance);
                  implementation_uid;
                  implementation_name;
                  site;
                  check = Some (protocol_check_kind check.kind);
                  check_site =
                    (match check_site_resolution mconfig check with
                    | Resolved_site loc -> Some loc
                    | No_recorded_site | Unresolved_site -> None)
                })))
  in
  let own_interface_rows = if buffer_wide then own_interface_rows else [] in
  let implementations =
    unique_implementations (own_interface_rows @ implementations)
  in
  log ~title:"query" "%d targets, %d rows" (List.length targets)
    (List.length implementations);
  { targets; implementations }
