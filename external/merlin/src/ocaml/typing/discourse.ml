(* TODO SP make it (or check it is made) explicit that all rules are used *)

(*

We call U the set of all paths used directly in a file:

- 1. Any path occurring in the file is in U. For example, List.map occurring in
     the file will add both List.map and List to U.
- 2. All paths for definitions in the current file are in U. So if module M = …
     occurs in the file then M is in U.
- 3. All paths for things “defined” using include or open in the current file
     are in U. It is possible that all of these would end up in D anyway via
     other rules, but it's not entirely obvious so I've included this rule here
     just to make sure they do.
- Note that constructors or fields only used via type-based disambiguation are
  not in U.

We call D the domain of discourse:

- 1. The paths of all the predefined types that are intended for direct use by
     users, like int, are in D.
- 2. If a path is in U then it is also in D.
- 3. If a module path is in U then all the paths of its subcomponents are in D.
- 4. If a value path is in U and its value description was written by a user -
     as opposed to being inferred - then the paths used in that description are
     in D.
- 5. If a module path is in U and its module description was written by a user -
     as opposed to being inferred - then the paths used in that description are
     in D, excluding those paths that only appear inside of a sig…end.
- 6. If a type path is in U then any paths used in its equation or
     representation are in D.
- 7. If a constructor or record field is in U then any paths used in its type
     are in D.
- 8. If a module type path is in U then any paths used in its definition are in
     D, excluding those paths that only appear inside of a sig…end.
- 9. If a class path is in U and its class description was written by a user -
     as opposed to being inferred - then of any paths used in that description
     are in D.
- 10. If a class type path is in U then any paths used in its definition are in
      D.
- 11. If a path is in D and it includes another module path within it, then that
      module path is also in D.
- 12. If a module path m in D - note D not U - is a module alias with target n
      and another path p in D includes n within it, then the path obtained by
      substituting the m for n in p is also in D.
*)

(* This logging section logs a "recap" of the discourse creation: The value of
   [U] before [D.of_U] is called, and the value of [D] at the end.

   It will also display the "intermediate" U that are built during the creation
   of D (see "special rule") *)
let recap_log_section = "discourse-recap"
let { Logger.log = log_recap } = Logger.for_section recap_log_section

let pp_d fmt d =
  let open Discourse_types in
  let open Format in
  let pp_sep fmt () = fprintf fmt ";@ " in
  let pp_lid_set fmt set =
    fprintf fmt "@[<1>[%a]@]"
      (pp_print_list ~pp_sep Pprintast.longident)
      (Lid_set.elements set)
  in
  let pp_substs_binding fmt (lid, lids) =
    fprintf fmt "@[<2>%a ->@ %a@]" Pprintast.longident lid pp_lid_set lids
  in
  let pp_substs fmt map =
    if Lid_map.is_empty map then fprintf fmt "[]"
    else
      fprintf fmt "@[<v>[%a]@]"
        (pp_print_list ~pp_sep pp_substs_binding)
        (Lid_map.bindings map)
  in
  fprintf fmt
    "@[<v 2>Discourse {@;size = %i;@;paths =@ %a;@;substs =@ %a@;<-2>}@]"
    (Lid_trie.size d.paths) pp d.paths pp_substs d.substs

(* A more verbose section which logs every addition to U or D, along with the
   reason the element is added (U1, U2, U3, D2, …, D12) *)
let log_section = "discourse-verbose"
let { Logger.log } = Logger.for_section log_section

open Shape.Sig_component_kind
open Discourse_types

module U = struct
  module Disambiguate_id : sig
    type t
    val get_id : unit -> t
    val compare : t -> t -> int
  end = struct
    type t = int
    let get_id =
      let cpt = ref 0 in
      fun () ->
        incr cpt;
        !cpt

    let compare = Int.compare
  end
  type u_item =
    { item : Item.t;
      env : Env.t option;
          (* Items added by "defined" rules (U2 and U3) do not need an env, as
             they are in the compilation unit (and so U1 will add all paths
             mentioned in them). TODO: Is that true for U3?

             Items added by the "used" rule (U1) need an environment, to be able
             to find them later in the translation U -> D, when applying D rules
             such as D4, D6, D8, ... *)
      disambiguator : Disambiguate_id.t
          (* We cannot compare two env with a different env, in a way that it
             makes an order (id we have transitivity, antisymetry, ...).

             So when we would need to do that, we reach out to a disambigator
             id. *)
    }
  module ItemSet = Set.Make (struct
    type t = u_item

    let compare i1 i2 =
      match (i1.env, i2.env) with
      | None, None -> Item.compare i1.item i2.item
      | Some _, None -> -1
      | None, Some _ -> 1
      | Some env1, Some env2 when env1 == env2 -> Item.compare i1.item i2.item
      (* In order to keep order properties from compare, in case where we cannot
         compare existing env, we disambiguate with the ID. *)
      | Some _, Some _ ->
        Disambiguate_id.compare i1.disambiguator i2.disambiguator
  end)

  type u =
    { u_paths : ItemSet.t Lid_map.t;
      substs : Lid_set.t Lid_map.t;
      discourse : Lid_trie.t
    }

  let paths_union (ps1 : ItemSet.t Lid_map.t) (ps2 : ItemSet.t Lid_map.t) =
    Lid_map.union (fun _key set1 set2 -> Some (ItemSet.union set1 set2)) ps1 ps2

  let pp_u fmt u =
    let open Format in
    let pp_sep fmt () = fprintf fmt ";@ " in
    let pp_env fmt env =
      match env with
      | None -> pp_print_string fmt "without env"
      | Some _ -> pp_print_string fmt "with env"
    in
    let pp_u_item fmt { item = kind, path; env } =
      fprintf fmt "@[<1>{item = (%s,@ %a);@ env = %a}@]"
        (Shape.Sig_component_kind.to_string kind)
        (Format_doc.compat Path.print)
        path pp_env env
    in
    let pp_item_set fmt set =
      fprintf fmt "@[<1>[%a]@]"
        (pp_print_list ~pp_sep pp_u_item)
        (ItemSet.elements set)
    in
    let pp_u_paths_binding fmt (lid, items) =
      fprintf fmt "@[<2>%a ->@ %a@]" Pprintast.longident lid pp_item_set items
    in
    let pp_u_paths fmt map =
      fprintf fmt "@[<v>[%a]@]"
        (pp_print_list ~pp_sep pp_u_paths_binding)
        (Lid_map.bindings map)
    in
    let pp_lid_set fmt set =
      fprintf fmt "@[<1>[%a]@]"
        (pp_print_list ~pp_sep Pprintast.longident)
        (Lid_set.elements set)
    in
    let pp_substs_binding fmt (lid, lids) =
      fprintf fmt "@[<2>%a ->@ %a@]" Pprintast.longident lid pp_lid_set lids
    in
    let pp_substs fmt map =
      fprintf fmt "@[<v>[%a]@]"
        (pp_print_list ~pp_sep pp_substs_binding)
        (Lid_map.bindings map)
    in
    fprintf fmt "@[<v 2>{ u_paths =@ %a;@ substs =@ %a }@]" pp_u_paths u.u_paths
      pp_substs u.substs

  let add_item_set lid item item_set =
    Lid_map.update lid
      (function
        | None -> Some (ItemSet.singleton item)
        | Some set -> Some (ItemSet.add item set))
      item_set

  let add_item lid item u =
    let u_paths = add_item_set lid item u.u_paths in
    { u with u_paths }

  let empty_u : u =
    { u_paths = Lid_map.empty; substs = Lid_map.empty; discourse = empty }
  let g = Local_store.s_ref empty_u

  (** We call U the set of all paths used directly in a file:

    1. Any path occurring in the file is in U. For example, List.map occurring in
       the file will add both List.map and List to U.
    2. All paths for definitions in the current file are in U. So if module M = …
       occurs in the file then M is in U.
    3. All paths for things “defined” using include or open in the current file
       are in U. It is possible that all of these would end up in D anyway via
       other rules, but it's not entirely obvious so I've included this rule here
       just to make sure they do.
    - Note that constructors or fields only used via type-based disambiguation are
      not in U.
*)

  let get () = !g
  let set v = g := v
  let reset () = g := empty_u

  let record_usages = Config.merlin

  let add_initial_discourse () =
    let d = !g in
    g := { d with discourse = Lid_trie.union (Predef.discourse ()) d.discourse }

  let fold_on_common_lid_and_path_segments ~init ~kind ~f (lid, path) =
    let rec aux acc kind ((lid, path) : Longident.t * Path.t) =
      let acc = f acc kind (lid, path) in
      match (lid, path) with
      | Lident _, Pident _ -> acc
      | Ldot (l, _), Pdot (p, _) -> aux acc Module (l.txt, p)
      | Lapply (l1, l2), Papply (p1, p2) ->
        let acc = aux acc Module (l2.txt, p2) in
        aux acc Module (l1.txt, p1)
      | _ -> acc
    in
    aux init kind (lid, path)

  (* If a path is in D and it includes another module path within it, then that
     module path is also in D.*)
  let add_all_components acc paths =
    let seq = Lid_trie.to_seq paths in
    Seq.fold_left
      (fun acc (lid, paths) ->
        Paths.fold
          (fun (kind, path) init ->
            fold_on_common_lid_and_path_segments ~init ~kind
              ~f:(fun acc kind (lid, path) -> Lid_trie.add lid (kind, path) acc)
              (lid, path))
          paths acc)
      acc seq

  let add_subst substs path lid =
    log ~title:"subst" "subst: %a -> %a (open/alias defined in file)" Logger.fmt
      (Fun.flip (Format_doc.compat Path.print) path)
      Logger.fmt
      (Fun.flip Pprintast.longident lid);
    let substs =
      Lid_map.update
        (Untypeast.lident_of_path path)
        (function
          | None -> Some (Lid_set.singleton lid)
          | Some lids -> Some (Lid_set.add lid lids))
        substs
    in
    substs

  let add_subst_g path lid =
    g := { !g with substs = add_subst !g.substs path lid }

  (** {1 Rule U2: All paths for definitions in the current file are in U} *)

  let lid_and_path_of_ident ?root_lid ?root_path id =
    let lid =
      match root_lid with
      | Some lid ->
        Longident.Ldot (Location.mknoloc lid, Location.mknoloc (Ident.name id))
      | None -> Longident.Lident (Ident.name id)
    in
    let path =
      match root_path with
      | Some path -> Path.Pdot (path, Ident.name id)
      | None -> Path.Pident id
    in
    (lid, path)

  let if_record_usage f = if record_usages then f ()

  let define ~from kind ?root_path ?root_lid id =
    if_record_usage @@ fun () ->
    let lid, path = lid_and_path_of_ident ?root_path ?root_lid id in
    log ~title:"U2" "U2: %s %a [%a] %s"
      (Shape.Sig_component_kind.to_string kind)
      Logger.fmt
      (fun fmt -> Pprintast.longident fmt lid)
      Logger.fmt
      (fun fmt -> (Format_doc.compat Path.print) fmt path)
      (match from with
      | `File -> "defined in current file"
      | `Open -> "brough in scope by an open");
    let item = (kind, path) in
    g := { !g with discourse = Lid_trie.add lid item !g.discourse }

  let rec define_signature ?(from = `File) ?root_path ?root_lid sg =
    if record_usages then
      List.iter (define_component ~from ?root_path ?root_lid) sg

  and define_component ?(from = `File) ?root_path ?root_lid sig_item =
    if record_usages then
      match (sig_item : Types.signature_item) with
      | Sig_type (id, _, _, _) -> define_type ~from ?root_path ?root_lid id
      | Sig_value (id, _, _) -> define_value ~from ?root_path ?root_lid id
      | Sig_typext (_, _, _, _) | Sig_jkind _ -> ()
      | Sig_module (id, _, md, _, _) ->
        define_module ~from ?root_path ?root_lid md id
      | Sig_modtype (id, _, _) -> define_modtype ~from ?root_path ?root_lid id
      | Sig_class (_, _, _, _) | Sig_class_type (_, _, _, _) ->
        (* TODO: do *) ()

  and define_type ?(from = `File) ?root_path ?root_lid id =
    define ~from ?root_path ?root_lid Type id

  and define_value ?(from = `File) ?root_path ?root_lid id =
    define ~from ?root_path ?root_lid Value id

  and define_module ?(from = `File) ?root_path ?root_lid
      (decl : Types.module_declaration) id =
    define ~from Module ?root_path ?root_lid id;
    let root_lid, root_path = lid_and_path_of_ident ?root_path ?root_lid id in
    match decl.md_type with
    | Mty_ident path | Mty_alias path -> add_subst_g path root_lid
    | Mty_signature module_type ->
      define_signature ~from ~root_path ~root_lid module_type
    | _ -> ()

  and define_modtype ?(from = `File) ?root_path ?root_lid id =
    define ~from ?root_path ?root_lid Module_type id

  (** {1 Rule U3}

     All paths for things “defined” using include or open in the current file
     are in U. It is possible that all of these would end up in D anyway via
     other rules, but it's not entirely obvious so I've included this rule here
     just to make sure they do.
  *)

  let rec define_signature_for_open _env ~root_path ~root_lid
      (sg : Subst.Lazy.signature) =
    List.iter
      (fun sig_item ->
        match (sig_item : Subst.Lazy.signature_item) with
        | Sig_type (id, _, _, _) ->
          log ~title:"U3" "U3: type %a brought in scope by open" Logger.fmt
            (Fun.flip Ident.print id);
          define_type ~from:`Open ~root_path ?root_lid id
        | Sig_value (id, _, _) ->
          log ~title:"U3" "U3: value %a brought in scope by open" Logger.fmt
            (Fun.flip Ident.print id);
          define_value ~from:`Open ~root_path ?root_lid id
        | Sig_typext (_, _, _, _) | Sig_jkind _ -> ()
        | Sig_module (id, Mp_present, { md_type = Mty_signature s; _ }, _, _) ->
          (* We recursively  bring everything that is direcelty defined in the
             opened module, but without following aliases. *)
          log ~title:"U3" "U3: module (present) %a brought in scope by open"
            Logger.fmt (Fun.flip Ident.print id);
          let lid, path = lid_and_path_of_ident ~root_path ?root_lid id in
          add_subst_g path lid;
          define ~from:`Open Module ~root_path ?root_lid id;
          let root_lid = Some lid in
          define_signature_for_open _env ~root_path:path ~root_lid s
        | Sig_module (id, _, { md_type; _ }, _, _) ->
          log ~title:"U3" "U3: module %a brought in scope by open" Logger.fmt
            (Fun.flip Ident.print id);
          let lid, path = lid_and_path_of_ident ~root_path ?root_lid id in
          let () =
            match md_type with
            | Mty_alias alias_path -> add_subst_g alias_path lid
            | _ -> ()
          in
          add_subst_g path lid;
          define ~from:`Open Module ~root_path ?root_lid id
          (* TODO Adding to U here fixes a few issues but we would prefer not to
             do it. *)
          (* g := *)
          (*   add_item lid *)
          (*     { item = (Module, path); *)
          (*       env = Some env; *)
          (*       disambiguator = Disambiguate_id.get_id () *)
          (*     } *)
          (*     !g *)
        | Sig_modtype (id, _, _) ->
          log ~title:"U3" "U3: module type %a brought in scope by open"
            Logger.fmt (Fun.flip Ident.print id);
          define_modtype ~from:`Open ~root_path ?root_lid id
        | Sig_class (_, _, _, _) | Sig_class_type (_, _, _, _) ->
          (* TODO: do *) ())
      (Subst.Lazy.force_signature_once sg)

  let open_module env path =
    if record_usages then begin
      log ~title:"U3" "U3: open module %a" Logger.fmt (fun fmt ->
          (Format_doc.compat Path.print) fmt path);
      try
        (* TODO: should we do this lazily to? *)
        (* When opening we need to traverse the aliases to get the components *)
        let root_path = Env.normalize_module_path None env path in
        let md = Env.find_module_lazy root_path env in
        match md.md_type with
        | Mty_signature sg ->
          define_signature_for_open env ~root_path ~root_lid:None sg
        | _ -> ()
      with Not_found -> ()
    end

  (** {1 Rule U1}

      Any path occurring in the file is in U. For example, List.map occurring in
      the file will add both List.map and List to U.
  *)

  let add_used env kind lid path t =
    let loc = lid.Location.loc in
    let f acc kind (lid, path) =
      log ~title:"U1" "U1: path %a used in file: %a (%a)" Logger.fmt
        (fun fmt ->
          Format.pp_print_string fmt (Shape.Sig_component_kind.to_string kind))
        Logger.fmt
        (Fun.flip (Format_doc.compat Path.print) path)
        Logger.fmt
        (fun fmt -> Location.print_loc fmt loc);
      add_item lid
        { item = (kind, path);
          env = Some env;
          disambiguator = Disambiguate_id.get_id ()
        }
        acc
    in
    fold_on_common_lid_and_path_segments ~init:t ~kind ~f
      (lid.Location.txt, path)

  let use_module env lid path = add_used env Module lid path
  let use_modtype env lid path = add_used env Module_type lid path
  let use_type env lid path = add_used env Type lid path
  let use_value env lid path = add_used env Value lid path

  let use_constructor env lid (constr : Data_types.constructor_description) t =
    if record_usages then begin
      let t =
        (* When using a constructor, the modules appearing in its path should be
           added to U. TODO: we might want to do that even if the constructor
           has been disambiguated *)
        match lid.Location.txt with
        | Longident.Ldot (lid, _) ->
          (* This find should not load additional CUs, because
             [lookup_structure_components] was called anyway by the
             compiler. *)
          let path, _ = Env.find_module_by_name_lazy lid.txt env in
          use_module env lid path t
        | _ -> t
      in
      (* If a constructor is in U then any paths used in its type are in D. *)
      log ~title:"D7" "D7: constructor %a used, merging its discourse"
        Logger.fmt
        (Fun.flip Pprintast.longident lid.txt);
      { t with discourse = Lid_trie.union t.discourse constr.cstr_discourse }
    end
    else t

  let use_label env lid (label : _ Data_types.gen_label_description) t =
    if record_usages then begin
      let t =
        (* When using a constructor, the modules appearing in its path should be
           added to U. TODO we might want to do that even if the constructor has
           been disambiguated. *)
        match lid.Location.txt with
        | Longident.Ldot (lid, _) ->
          (* This find should not load additional CUs, because
             [lookup_all_labels] was called anyway by the compiler. *)
          let path, _ = Env.find_module_by_name_lazy lid.txt env in
          use_module env lid path t
        | _ -> t
      in
      (* If a label is in U then any paths used in its type are in D. *)
      log ~title:"D7" "D7: label %a used, merging its discourse" Logger.fmt
        (Fun.flip Pprintast.longident lid.txt);
      { t with discourse = Lid_trie.union t.discourse label.lbl_discourse }
    end
    else t
end

module D = struct
  (**

     {2 The domain of discourse}

     - 1. The paths of all the predefined types that are intended for direct use by
       users, like int, are in D.
     - 2. If a path is in U then it is also in D.
     - 3. If a module path is in U then all the paths of its subcomponents are in D.
     - 4. If a value path is in U and its value description was written by a user -
       as opposed to being inferred - then the paths used in that description are
       in D.
     - 5. If a module path is in U and its module description was written by a user -
       as opposed to being inferred - then the paths used in that description are
       in D, excluding those paths that only appear inside of a sig…end.
     - 6. If a type path is in U then any paths used in its equation or
       representation are in D.
     - 7. If a constructor or record field is in U then any paths used in its type
       are in D.
     - 8. If a module type path is in U then any paths used in its definition are in
       D, excluding those paths that only appear inside of a sig…end.
     - 9. If a class path is in U and its class description was written by a user -
       as opposed to being inferred - then of any paths used in that description
       are in D.
     - 10. If a class type path is in U then any paths used in its definition are in
       D.
     - 11. If a path is in D and it includes another module path within it, then that
       module path is also in D.
     - 12. If a module path m in D - note D not U - is a module alias with target n
       and another path p in D includes n within it, then the path obtained by
       substituting the m for n in p is also in D.
  *)

  (* TODO: this is not really a chain, just increasingly large jumps. This
     reduces the number of combinations when unrolling substitutions but we might
     need the missing internal aliases. *)
  let follow_aliases_adding_subst log_detail env substs path lid =
    let add_to_substs substs path lid =
      log ~title:"D12" "D12: subst %a -> %a (%s)" Logger.fmt
        (Fun.flip (Format_doc.compat Path.print) path)
        Logger.fmt
        (Fun.flip Pprintast.longident lid)
        log_detail;
      U.add_subst substs path lid
    in
    let rec loop substs path =
      match path with
      | Path.Pident id when Ident.is_global id -> add_to_substs substs path lid
      | _ -> (
        match Env.find_module_lazy path env with
        | { md_type = Mty_alias path1 } ->
          let substs = add_to_substs substs path1 lid in
          loop substs path1
        | _ -> add_to_substs substs path lid)
    in
    loop substs path

  let special_rule_for_aliases env { paths; substs } u_next path alias_lid
      alias_path =
    try
      let substs =
        log ~title:"D12" "D12: subst %a -> %a (alias at %a)" Logger.fmt
          (Fun.flip (Format_doc.compat Path.print) path)
          Logger.fmt
          (Fun.flip Pprintast.longident alias_lid.Location.txt)
          Logger.fmt
          (Fun.flip Location.print_loc alias_lid.loc);
        U.add_subst substs path alias_lid.Location.txt
      in
      (* TODO: This is an unwritten rule (yet): If a module in U is an alias
         then this alias is also in U *)
      let u_next = U.use_module env alias_lid alias_path u_next in
      ({ paths; substs }, u_next)
    with Not_found -> ({ paths; substs }, u_next)

  let d3_rule env lid path paths substs sig_ =
    let ldot id = Longident.Ldot (lid, Location.mknoloc (Ident.name id)) in
    let pdot id = Path.Pdot (path, Ident.name id) in
    List.fold_left
      (fun (paths, substs) item ->
        let add kind id =
          log ~title:"D3" "D3: signature component %s %a (%a) under module %a"
            (Shape.Sig_component_kind.to_string kind)
            Logger.fmt
            (fun fmt -> Ident.print fmt id)
            Logger.fmt
            (Fun.flip (Format_doc.compat Path.print) (pdot id))
            Logger.fmt
            (Fun.flip Pprintast.longident lid.txt);
          Lid_trie.add (ldot id) (kind, pdot id) paths
        in
        match (item : Subst.Lazy.signature_item) with
        | Subst.Lazy.Sig_value (id, _, _) -> (add Value id, substs)
        | Sig_type (id, _, _, _) -> (add Type id, substs)
        | Sig_typext (id, _, _, _) -> (add Extension_constructor id, substs)
        | Sig_module (id, _, _, _, _) ->
          let md = Env.find_module_lazy (pdot id) env in
          let paths = add Module id in
          let substs =
            match md.md_type with
            | Mty_alias path' ->
              let lid = ldot id in
              follow_aliases_adding_subst "sub-component is a module alias" env
                substs path' lid
            | _ -> substs
          in
          (paths, substs)
        | Sig_modtype (id, _, _) -> (add Module_type id, substs)
        | Sig_class (id, _, _, _) -> (add Class id, substs)
        | Sig_class_type (id, _, _, _) -> (add Class_type id, substs)
        | Sig_jkind _ -> (paths, substs))
      (paths, substs)
      (Subst.Lazy.force_signature_once sig_)

  (* TODO: see if we can do better with the accumulator ([d] and
     [u_next]): sometimes it is represented as a couple and sometimes as two
     distinct arguments, preventing the more readable folds *)

  let module_consequences { paths; substs } u_next env lid path :
      discourse * U.u =
    let (paths, substs), u_next =
      let md = Env.find_module_lazy path env in
      let { paths; substs }, u_next =
        match md.md_discourse_alias with
        | None -> ({ paths; substs }, u_next)
        | Some (alias_lid, (_Module, alias_path)) ->
          special_rule_for_aliases env { paths; substs } u_next path alias_lid
            alias_path
      in

      (* D5. If a module path is in U and its module description was written then
         the paths used in that description are in D *)
      (* TODO : If a path is in D and it includes another module path within it,
         then that module path is also in D. *)
      log ~title:"D5" "D5: merging discourse of module %a" Logger.fmt
        (Fun.flip Pprintast.longident lid);
      let paths = Lid_trie.union paths md.md_discourse in
      begin match md.md_type with
      | Mty_alias path' ->
        (* D12. If a module path m in D - note D not U - is a module alias
             with target n and another path p in D includes n within it, then
             the path obtained by substituting the m for n in p is also in D.

             We accumulate such substitution and will apply them when shortening
             a path. *)
        let substs =
          follow_aliases_adding_subst "Mty_alias target" env substs path' lid
        in
        (* We have to follow aliases to be able to add module components to
             the discourse.

             TODO now that we have md_discourse_aliases, this might be redundant
             ? *)
        let path' = Env.normalize_module_path None env path' in
        (* TODO: Check, this might not be the same as the code before the
             rebase. *)
        let u_next =
          U.add_item lid
            { item = (Module, path');
              env = Some env;
              disambiguator = U.Disambiguate_id.get_id ()
            }
            u_next
          (* add_path_to_discourse env { paths; substs } Module lid path'  *)
        in

        ((paths, substs), u_next)
      | Mty_signature sig_ ->
        (* D3. If a module path is in U then all the paths of its subcomponents
             are in D *)
        (d3_rule env (Location.mknoloc lid) path paths substs sig_, u_next)
      | _ -> ((paths, substs), u_next)
      end
    in
    ({ paths; substs }, u_next)

  let consequences d u_next (longident, { U.item = kind, path; env }) =
    match (kind, env) with
    | _, None -> (d, u_next)
    | Module_type, Some env ->
      let mtd = Env.find_modtype_lazy path env in
      (* D8. If a module type path is in U then any paths used in its definition
         are in *)
      log ~title:"D8" "D8: merging discourse of module type %a" Logger.fmt
        (Fun.flip Pprintast.longident longident);
      (* TODO : If a path is in D and it includes another module path within it,
         then that module path is also in D. *)
      ({ d with paths = Lid_trie.union d.paths mtd.mtd_discourse }, u_next)
    | Module, Some env -> module_consequences d u_next env longident path
    | Value, Some env ->
      (* D4. If a value path is in U and its value description was written by a user -
         as opposed to being inferred - then the paths used in that description are
         in D. *)
      (* TODO : If a path is in D and it includes another module path within it,
         then that module path is also in D. *)
      let vd = Env.find_value path env in
      log ~title:"D4" "D4: merging discourse of value %a" Logger.fmt
        (Fun.flip Pprintast.longident longident);
      ({ d with paths = Lid_trie.union d.paths vd.val_discourse }, u_next)
    | Type, Some env ->
      (* D6. If a type path is in U then any paths used in its equation or
         representation are in D. *)
      (* TODO : If a path is in D and it includes another module path within it,
         then that module path is also in D. *)
      let td = Env.find_type path env in
      (* What does it mean when such a path is just an ident that is local to
         another module ?*)
      log ~title:"D6" "D6: merging discourse of type %a" Logger.fmt
        (Fun.flip Pprintast.longident longident);
      ({ d with paths = Lid_trie.union d.paths td.type_discourse }, u_next)
    | _ -> (d, u_next)

  let add_from_u_to_d :
      discourse -> U.u -> Longident.t * U.u_item -> discourse * U.u =
   fun d u_next ((longident, item) as input) ->
    log ~title:"D2" "D2: %a in U so in D (kind: %s, path: %a)" Logger.fmt
      (Fun.flip Pprintast.longident longident)
      (Shape.Sig_component_kind.to_string (fst item.U.item))
      Logger.fmt
      (Fun.flip (Format_doc.compat Path.print) (snd item.U.item));
    (* TODO: If item is already in paths we should skip adding it (and more
       importantly, skip the consequences!) *)
    let d = { d with paths = Lid_trie.add longident item.item d.paths } in
    (* In some cases, [consequences] tries to load its own compilation unit,
       which (inconsistently to our understanding) fails.

       For instance, the two following queries on base's float raise a [Not
       found] exception:

       {[
       /path/to/this/ocamlmerlin single type-enclosing -position '892:7' -index 0  -filename /path/to/base/src/float.ml < /path/to/base/src/float.ml

       {"class":"exception","value":"Not_found
       Raised at Ocaml_typing__Ident.find_same in file \"src/ocaml/typing/ident.ml\", line 305, characters 6-21
       [...]
       Called from Ocaml_typing__Discourse.D.module_consequences in file \"src/ocaml/typing/discourse.ml\", line 574, characters 15-44
       [...]
       ","notifications":[],"timing":{"clock":326,"cpu":234,"query":20,"pp":0,"reader":7,"ppx":41,"typer":166,"error":0},"heap_mbytes":41,"cache":{"reader_phase":"miss","ppx_phase":"miss","typer":"miss","cmt":{"hit":0,"miss":0},"cms":{"hit":0,"miss":0},"cmi":{"hit":0,"miss":53},"document_overrides_phase":"miss","locate_overrides_phase":"miss"},"query_num":0}
       ]}

       and

       {[
       /path/to/this/ocamlmerlin server type-enclosing -position '201:18' -index 0  -filename /path/to/base/src/int32.ml < /path/to/base/src/int32.ml
       ]}

       TODO: find why there is such a self-module added/why it sometimes load
       sometimes raises. *)
    try consequences d u_next input
    with Not_found | Env.Error (Lookup_error _) -> (d, u_next)

  let of_U u =
    log_recap ~title:"U" "U at start of D.of_U:\n%a" Logger.fmt
      (Fun.flip U.pp_u u);
    let is_empty u = Lid_map.is_empty u.U.u_paths in
    let has_been_added lid item old_u =
      match Lid_map.find_opt lid old_u with
      | None -> false
      | Some set -> U.ItemSet.mem item set
    in
    let rec add_u_to_d d u old_u =
      let d, u_next =
        Lid_map.to_seq u.U.u_paths
        |> Seq.fold_left
             (fun (d, u_next) (lid, (x : U.ItemSet.t)) ->
               U.ItemSet.fold
                 (fun item (d, u_next) ->
                   if has_been_added lid item old_u then (d, u_next)
                   else add_from_u_to_d d u_next (lid, item))
                 x (d, u_next))
             (d, U.empty_u)
      in
      let d =
        { d with
          substs =
            Lid_map.union
              (fun _ a b -> Some (Lid_set.union a b))
              d.substs u.U.substs
        }
      in
      if is_empty u_next then d
      else (
        log_recap ~title:"next_U" "next_U (non-empty, looping):\n%a" Logger.fmt
          (Fun.flip U.pp_u u_next);
        let old_u = U.paths_union u.U.u_paths old_u in
        add_u_to_d d u_next old_u)
    in
    let d =
      add_u_to_d
        { paths = u.U.discourse; substs = Lid_map.empty }
        u Lid_map.empty
    in
    log_recap ~title:"D" "Final D:\n%a" Logger.fmt (Fun.flip pp_d d);
    d

  let pp = pp_d
end

include U

let use_module env lid path = g := use_module env lid path !g
let use_modtype env lid path = g := use_modtype env lid path !g
let use_type env lid path = g := use_type env lid path !g
let use_value env lid path = g := use_value env lid path !g
let use_constructor env lid path = g := use_constructor env lid path !g
let use_label env lid path = g := use_label env lid path !g

let get () = D.of_U !g

let debug_print fmt = D.pp fmt (D.of_U !g)
