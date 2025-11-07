open Std
open Local_store

let { Logger.log } = Logger.for_section "Mtyper"

type index = Longident.t Location.loc list Shape.Uid.Map.t

(* Forward ref to be filled by analysis.Occurrences *)
let index_items :
    (index ->
    Mconfig.t ->
    [ `Impl of Typedtree.structure_item list
    | `Intf of Typedtree.signature_item list ] ->
    index)
    ref =
  ref (fun acc _config _item -> acc)
let set_index_items f = index_items := f

type ('p, 't) item =
  { parsetree_item : 'p;
    typedtree_items : 't list * Types.signature_item list;
    part_snapshot : Types.snapshot;
    part_stamp : int;
    part_uid : int;
    part_env : Env.t;
    part_rev_sg : Types.signature_item list;
    part_errors : exn list;
    part_checks : Typecore.delayed_check list;
    part_warnings : Warnings.state;
    part_index : index lazy_t
  }

type typedtree =
  [ `Interface of Typedtree.signature | `Implementation of Typedtree.structure ]

type typedtree_items =
  | Interface_items of
      { items : (Parsetree.signature_item, Typedtree.signature_item) item list;
        psig_modalities : Parsetree.modalities;
        sig_modalities : Mode.Modality.Const.t;
        sig_sloc : Location.t
      }
  | Implementation_items of
      (Parsetree.structure_item, Typedtree.structure_item) item list

type typer_cache_stats = Miss | Hit of { reused : int; typed : int }

type 'a cache_result =
  { env : Env.t;
    snapshot : Types.snapshot;
    ident_stamp : int;
    uid_stamp : int;
    value : 'a
  }

let cache : typedtree_items option cache_result option ref = s_ref None

let fresh_env config =
  let env0 = Typer_raw.fresh_env () in
  let env0 = Extension.register Mconfig.(config.merlin.extensions) env0 in
  let snap0 = Btype.snapshot () in
  let stamp0 = Ident.get_currentstamp () in
  let uid0 = Shape.Uid.get_current_stamp () in
  (env0, snap0, stamp0, uid0)

let get_cache config =
  match !cache with
  | Some ({ snapshot; _ } as c) when Types.is_valid snapshot -> c
  | Some _ | None ->
    let env, snapshot, ident_stamp, uid_stamp = fresh_env config in
    { env; snapshot; ident_stamp; uid_stamp; value = None }

let return_and_cache status =
  cache := Some { status with value = Some status.value };
  status

type result =
  { config : Mconfig.t;
    initial_env : Env.t;
    initial_snapshot : Types.snapshot;
    initial_stamp : int;
    stamp : int;
    initial_uid_stamp : int;
    typedtree : typedtree_items;
    cache_stat : typer_cache_stats
  }

let initial_env res = res.initial_env

let get_cache_stat res = res.cache_stat

let compatible_prefix_rev result_items tree_items =
  let rec aux acc = function
    | ritem :: ritems, pitem :: pitems
      when Types.is_valid ritem.part_snapshot
           && compare ritem.parsetree_item pitem = 0 ->
      aux (ritem :: acc) (ritems, pitems)
    | _, pitems ->
      let reused = List.length acc in
      let typed = List.length pitems in
      let cache_stat = Hit { reused; typed } in
      log ~title:"compatible_prefix" "reusing %d items, %d new items to type"
        reused typed;
      (acc, pitems, cache_stat)
  in
  aux [] (result_items, tree_items)

let[@tail_mod_cons] rec type_structure config caught env index sg = function
  | parsetree_item :: rest ->
    let items, sg', part_env =
      Typemod.merlin_type_structure env sg [ parsetree_item ]
    in
    let part_index =
      lazy (!index_items (Lazy.force index) config (`Impl items.str_items))
    in
    let typedtree_items =
      (items.Typedtree.str_items, items.Typedtree.str_type)
    in
    let part_rev_sg = List.rev_append sg' sg in
    let item =
      { parsetree_item;
        typedtree_items;
        part_env;
        part_rev_sg;
        part_snapshot = Btype.snapshot ();
        part_stamp = Ident.get_currentstamp ();
        part_uid = Shape.Uid.get_current_stamp ();
        part_errors = !caught;
        part_checks = !Typecore.delayed_checks;
        part_warnings = Warnings.backup ();
        part_index
      }
    in
    item :: type_structure config caught part_env part_index part_rev_sg rest
  | [] -> []

let[@tail_mod_cons] rec type_signature config caught env index sg psg_modalities
    psg_loc = function
  | parsetree_item :: rest ->
    let { Typedtree.sig_final_env = part_env;
          sig_items;
          sig_type;
          sig_modalities = _;
          sig_sloc = _
        } =
      Typemod.merlin_transl_signature env sg
        (Ast_helper.Sg.mk ~loc:psg_loc ~modalities:psg_modalities
           [ parsetree_item ])
    in
    let part_rev_sg = List.rev_append sig_type sg in
    let part_index =
      lazy (!index_items (Lazy.force index) config (`Intf sig_items))
    in
    let item =
      { parsetree_item;
        typedtree_items = (sig_items, sig_type);
        part_env;
        part_rev_sg;
        part_snapshot = Btype.snapshot ();
        part_stamp = Ident.get_currentstamp ();
        part_uid = Shape.Uid.get_current_stamp ();
        part_errors = !caught;
        part_checks = !Typecore.delayed_checks;
        part_warnings = Warnings.backup ();
        part_index
      }
    in
    item
    :: type_signature config caught part_env part_index part_rev_sg
         psg_modalities psg_loc rest
  | [] -> []

let type_implementation config caught parsetree =
  let { env; snapshot; ident_stamp; uid_stamp; value = cached_value; _ } =
    get_cache config
  in
  let rev_prefix, parsetree_suffix, cache_stats =
    match cached_value with
    | Some (Implementation_items items) -> compatible_prefix_rev items parsetree
    | Some (Interface_items _) | None -> ([], parsetree, Miss)
  in
  let env', sg', snap', stamp', uid_stamp', warn', index' =
    match rev_prefix with
    | [] ->
      ( env,
        [],
        snapshot,
        ident_stamp,
        uid_stamp,
        Warnings.backup (),
        lazy Shape.Uid.Map.empty )
    | x :: _ ->
      caught := x.part_errors;
      Typecore.delayed_checks := x.part_checks;
      ( x.part_env,
        x.part_rev_sg,
        x.part_snapshot,
        x.part_stamp,
        x.part_uid,
        x.part_warnings,
        x.part_index )
  in
  Btype.backtrack snap';
  Warnings.restore warn';
  Env.cleanup_functor_caches ~stamp:stamp';
  Env.cleanup_usage_tables ~stamp:uid_stamp';
  Shape.Uid.restore_stamp uid_stamp';
  let suffix = type_structure config caught env' index' sg' parsetree_suffix in
  let value = Implementation_items (List.rev_append rev_prefix suffix) in
  ( return_and_cache { env; snapshot; ident_stamp; uid_stamp; value },
    cache_stats )

let type_interface config caught (parsetree : Parsetree.signature) =
  let { env; snapshot; ident_stamp; uid_stamp; value = cached_value; _ } =
    get_cache config
  in
  let rev_prefix, parsetree_suffix, cache_stats =
    match cached_value with
    | Some
        (Interface_items
          { items; psig_modalities; sig_modalities = _; sig_sloc = _ })
    (* only use the cached items if they were typed using the same modalities on the sig *)
      when List.equal
             ~eq:(fun
                 { Location.txt = Parsetree.Modality a; loc = _ }
                 { Location.txt = Parsetree.Modality b; loc = _ }
               -> String.equal a b)
             psig_modalities parsetree.psg_modalities ->
      compatible_prefix_rev items parsetree.psg_items
    | Some (Interface_items _) | Some (Implementation_items _) | None ->
      ([], parsetree.psg_items, Miss)
  in
  let env', sg', snap', stamp', uid_stamp', warn', index' =
    match rev_prefix with
    | [] ->
      ( env,
        [],
        snapshot,
        ident_stamp,
        uid_stamp,
        Warnings.backup (),
        lazy Shape.Uid.Map.empty )
    | x :: _ ->
      caught := x.part_errors;
      Typecore.delayed_checks := x.part_checks;
      ( x.part_env,
        x.part_rev_sg,
        x.part_snapshot,
        x.part_stamp,
        x.part_uid,
        x.part_warnings,
        x.part_index )
  in
  Btype.backtrack snap';
  Warnings.restore warn';
  Env.cleanup_functor_caches ~stamp:stamp';
  Env.cleanup_usage_tables ~stamp:uid_stamp';
  Shape.Uid.restore_stamp uid_stamp';
  let suffix =
    type_signature config caught env' index' sg' parsetree.psg_modalities
      parsetree.psg_loc parsetree_suffix
  in
  (* transl an empty signature to get the sig_modalities and sig_sloc *)
  let ({ sig_final_env = _;
         sig_items = _;
         sig_type = _;
         sig_modalities;
         sig_sloc
       }
        : Typedtree.signature) =
    Typemod.merlin_transl_signature Env.empty []
      (Ast_helper.Sg.mk ~modalities:parsetree.psg_modalities
         ~loc:parsetree.psg_loc [])
  in
  let value =
    Interface_items
      { items = List.rev_append rev_prefix suffix;
        sig_modalities;
        psig_modalities = parsetree.psg_modalities;
        sig_sloc
      }
  in
  ( return_and_cache { env; snapshot; ident_stamp; uid_stamp; value },
    cache_stats )

let run config parsetree =
  if not (Env.check_state_consistency ()) then (
    (* Resetting the local store will clear the load_path cache.
       Save it now, reset the store and then restore the path. *)
    let { Load_path.visible; hidden } = Load_path.get_paths () in
    (* Same story with the registered parameters. *)
    let parameters = Env.parameters () in
    Mocaml.flush_caches ();
    Local_store.reset ();
    Load_path.reset ();
    Load_path.(init ~auto_include:no_auto_include ~visible ~hidden);
    List.iter ~f:Env.register_parameter parameters);
  let caught = ref [] in
  Msupport.catch_errors Mconfig.(config.ocaml.warnings) caught @@ fun () ->
  Typecore.reset_delayed_checks ();
  let cached_result, cache_stat =
    match parsetree with
    | `Implementation parsetree -> type_implementation config caught parsetree
    | `Interface parsetree -> type_interface config caught parsetree
  in
  let stamp = Ident.get_currentstamp () in
  Typecore.reset_delayed_checks ();
  { config;
    initial_env = cached_result.env;
    initial_snapshot = cached_result.snapshot;
    initial_stamp = cached_result.ident_stamp;
    stamp;
    initial_uid_stamp = cached_result.uid_stamp;
    typedtree = cached_result.value;
    cache_stat
  }

let get_env ?pos:_ t =
  Option.value ~default:t.initial_env
    (match t.typedtree with
    | Implementation_items l ->
      Option.map ~f:(fun x -> x.part_env) (List.last l)
    | Interface_items
        { items = l; sig_modalities = _; psig_modalities = _; sig_sloc = _ } ->
      Option.map ~f:(fun x -> x.part_env) (List.last l))

let get_errors t =
  let errors, checks =
    Option.value ~default:([], [])
      (let f x = (x.part_errors, x.part_checks) in
       match t.typedtree with
       | Implementation_items l -> Option.map ~f (List.last l)
       | Interface_items l -> Option.map ~f (List.last l.items))
  in
  let caught = ref errors in
  Typecore.delayed_checks := checks;
  Msupport.catch_errors
    Mconfig.(t.config.ocaml.warnings)
    caught Typecore.force_delayed_checks;
  Typecore.reset_delayed_checks ();
  !caught

let get_typedtree t =
  let split_items l =
    let typd, typs = List.split (List.map ~f:(fun x -> x.typedtree_items) l) in
    (List.concat typd, List.concat typs)
  in
  match t.typedtree with
  | Implementation_items l ->
    let str_items, str_type = split_items l in
    `Implementation { Typedtree.str_items; str_type; str_final_env = get_env t }
  | Interface_items { items = l; psig_modalities = _; sig_modalities; sig_sloc }
    ->
    let sig_items, sig_type = split_items l in
    `Interface
      { Typedtree.sig_items;
        sig_type;
        sig_final_env = get_env t;
        sig_modalities;
        sig_sloc
      }

let get_index t =
  let of_items items =
    List.last items
    |> Option.value_map ~default:Shape.Uid.Map.empty
         ~f:(fun { part_index; _ } -> Lazy.force part_index)
  in
  match t.typedtree with
  | Implementation_items items -> of_items items
  | Interface_items { items; _ } -> of_items items

let get_stamp t = t.stamp

let node_at ?(skip_recovered = false) ?let_pun_behavior t pos_cursor =
  let node = Mbrowse.of_typedtree (get_typedtree t) in
  log ~title:"node_at" "Node: %s" (Mbrowse.print () node);
  let rec select = function
    (* If recovery happens, the incorrect node is kept and a recovery node
       is introduced, so the node to check for recovery is the second one. *)
    | (_, _) :: ((_, node') :: _ as ancestors) when Mbrowse.is_recovered node'
      -> select ancestors
    | l -> l
  in
  match Mbrowse.deepest_before ?let_pun_behavior pos_cursor [ node ] with
  | [] -> [ (get_env t, Browse_raw.Dummy) ]
  | path when skip_recovered -> select path
  | path ->
    log ~title:"node_at" "Deepest before %s" (Mbrowse.print () path);
    path
