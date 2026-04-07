(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaëlle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Disable [not-principal] warning in this file. We often write code that looks
   like [let@ [x; y] = a in b] where the list constructor is an [hlist], and [a]
   is used to determine the type of that [hlist]. Unfortunately, due to how
   [let@] is expansed, this is not principal. *)
[@@@ocaml.warning "-not-principal"]

open Global_flow_graph.Relations
open! Datalog_helpers.Syntax
open Datalog_helpers
open! Points_to_analysis.Relations
include Points_to_analysis
open! Unboxing_analysis.Relations
include Unboxing_analysis
include Types_rewriter

let fixpoint (graph : Global_flow_graph.graph) =
  let datalog = Global_flow_graph.to_datalog graph in
  let stats =
    Datalog.Schedule.create_stats
      ~with_provenance:(Flambda_features.debug_reaper "prov")
      datalog
  in
  let db =
    Profile.record_call ~accumulate:true "analysis" (fun () ->
        Datalog.Schedule.run ~stats datalog_schedule datalog)
  in
  let db =
    Profile.record_call ~accumulate:true "compute_field_usages" (fun () ->
        List.fold_left
          (fun db rule -> Datalog.Schedule.run ~stats rule db)
          db field_of_constructor_is_used_rules)
  in
  let db =
    Profile.record_call ~accumulate:true "compute_unboxing_decisions" (fun () ->
        (* We need to do this after [field_of_constructor_is_used] is computed,
           so that we prevent unboxing based on the number of fields actually
           used. *)
        let db =
          let max_unbox_size = Flambda_features.reaper_max_unbox_size () in
          Datalog.set_table cannot_unbox0_tbl
            (Code_id_or_name.Map.filter_map
               (fun _block fields ->
                 let num_used_fields =
                   Field.Map.fold
                     (fun field () acc ->
                       if
                         Field.is_real_field field
                         && not (Field.is_function_slot field)
                       then acc + 1
                       else acc)
                     fields 0
                 in
                 if num_used_fields > max_unbox_size then Some () else None)
               (Datalog.get_table field_of_constructor_is_used_tbl db))
            db
        in
        List.fold_left
          (fun db rule -> Datalog.Schedule.run ~stats rule db)
          db datalog_rules)
  in
  if
    Flambda_features.debug_reaper "stats"
    || Flambda_features.debug_reaper "prov"
  then Format.eprintf "%a@." Datalog.Schedule.print_stats stats;
  if Flambda_features.debug_reaper "db"
  then Format.eprintf "%a@." Datalog.print db;
  let name_of_node =
    if Flambda_features.debug_reaper "nostamps"
    then
      fun node ->
        Code_id_or_name.pattern_match node ~code_id:Code_id.name
          ~symbol:Symbol.linkage_name_as_string ~var:Variable.name
    else
      fun node ->
        Flambda_colours.without_colours ~f:(fun () ->
            Format.asprintf "%a" Code_id_or_name.print node)
  in
  let has_to_be_unboxed code_or_name = has_to_be_unboxed [code_or_name] db in
  let unboxed, changed_representation =
    Profile.record_call ~accumulate:true "compute_unboxing_variables" (fun () ->
        let unboxed =
          Datalog.Cursor.fold query_to_unbox db ~init:Code_id_or_name.Map.empty
            ~f:(fun [code_or_name; to_patch] unboxed ->
              (* CR-someday ncourant: produce ghost makeblocks/set of closures
                 for debugging *)
              let new_name =
                Format.asprintf "%s_into_%s"
                  (name_of_node code_or_name)
                  (name_of_node to_patch)
              in
              let fields =
                mk_unboxed_fields ~has_to_be_unboxed
                  ~mk:(fun kind name -> Variable.create name kind)
                  db code_or_name
                  (get_fields db
                     (get_all_usages ~follow_known_arity_calls:true db
                        (Code_id_or_name.Map.singleton to_patch ())))
                  new_name
              in
              Code_id_or_name.Map.add to_patch fields unboxed)
        in
        if Flambda_features.debug_reaper "unbox"
        then
          Format.printf "TO UNBOX: %a@."
            (Code_id_or_name.Map.print
               (Field.Map.print (pp_unboxed_elt Variable.print)))
            unboxed;
        let changed_representation = ref Code_id_or_name.Map.empty in
        Datalog.Cursor.iter query_to_change_representation db
          ~f:(fun [code_id_or_name] ->
            (* This can happen because we change the representation of each
               function slot of a set of closures at the same time. *)
            if Code_id_or_name.Map.mem code_id_or_name !changed_representation
            then ()
            else
              let add_to_s repr alloc_point =
                Datalog.Cursor.iter_with_parameters query_dominated_by
                  [alloc_point] db ~f:(fun [c] ->
                    changed_representation
                      := Code_id_or_name.Map.add c (repr, alloc_point)
                           !changed_representation)
              in
              match get_set_of_closures_def db code_id_or_name with
              | Not_a_set_of_closures ->
                let r = ref ~-1 in
                let mk _kind _name =
                  (* XXX fixme, disabled for now *)
                  (* TODO depending on the kind, use two counters; then produce a
               mixed block; map_unboxed_fields should help with that *)
                  incr r;
                  ( !r,
                    Flambda_primitive.(
                      Block_access_kind.Values
                        { tag = Unknown;
                          size = Unknown;
                          field_kind = Block_access_field_kind.Any_value
                        }) )
                in
                let uses =
                  get_all_usages ~follow_known_arity_calls:false db
                    (Code_id_or_name.Map.singleton code_id_or_name ())
                in
                let repr =
                  mk_unboxed_fields ~has_to_be_unboxed ~mk db code_id_or_name
                    (get_fields db uses) ""
                in
                add_to_s (Block_representation (repr, !r + 1)) code_id_or_name
              | Set_of_closures l ->
                let mk kind name =
                  Value_slot.create
                    (Compilation_unit.get_current_exn ())
                    ~name ~is_always_immediate:false kind
                in
                let fields =
                  get_fields_usage_of_constructors db
                    (List.fold_left
                       (fun acc (_, x) -> Code_id_or_name.Map.add x () acc)
                       Code_id_or_name.Map.empty l)
                in
                let repr =
                  mk_unboxed_fields ~has_to_be_unboxed ~mk db code_id_or_name
                    fields "unboxed"
                in
                let fss =
                  List.fold_left
                    (fun acc (fs, _) ->
                      Function_slot.Map.add fs
                        (Function_slot.create
                           (Compilation_unit.get_current_exn ())
                           ~name:(Function_slot.name fs)
                           ~is_always_immediate:false Flambda_kind.value)
                        acc)
                    Function_slot.Map.empty l
                in
                List.iter
                  (fun (fs, f) ->
                    add_to_s (Closure_representation (repr, fss, fs)) f)
                  l);
        if Flambda_features.debug_reaper "unbox"
        then
          Format.eprintf "@.TO_CHG: %a@."
            (Code_id_or_name.Map.print (fun ff (repr, alloc_point) ->
                 Format.fprintf ff "[from %a]%a" Code_id_or_name.print
                   alloc_point pp_changed_representation repr))
            !changed_representation;
        unboxed, !changed_representation)
  in
  if
    Flambda_features.reaper_unbox ()
    && Flambda_features.reaper_change_calling_conventions ()
  then { db; unboxed_fields = unboxed; changed_representation }
  else
    { db;
      unboxed_fields = Code_id_or_name.Map.empty;
      changed_representation = Code_id_or_name.Map.empty
    }

let print_color { db; unboxed_fields; changed_representation } v =
  let red =
    if Code_id_or_name.Map.mem v unboxed_fields
    then "22"
    else if Code_id_or_name.Map.mem v changed_representation
    then "88"
    else "ff"
  in
  let green =
    if any_usage_query [v] db then "22" else if has_use db v then "88" else "ff"
  in
  let blue =
    if any_source_query [v] db
    then "22"
    else if has_source_query db v
    then "88"
    else "ff"
  in
  "#" ^ red ^ green ^ blue

let get_unboxed_fields uses cn =
  Code_id_or_name.Map.find_opt cn uses.unboxed_fields

let get_changed_representation uses cn =
  Option.map fst (Code_id_or_name.Map.find_opt cn uses.changed_representation)

let has_use uses v = has_use uses.db v

let field_used uses v f = field_used uses.db v f

let not_local_field_has_source uses v f = not_local_field_has_source uses.db v f

let cannot_change_calling_convention_query =
  let^? [x], [] = ["x"], [] in
  [cannot_change_calling_convention x]

let cannot_change_calling_convention uses v =
  (not (Flambda_features.reaper_change_calling_conventions ()))
  || (not (Compilation_unit.is_current (Code_id.get_compilation_unit v)))
  || cannot_change_calling_convention_query [Code_id_or_name.code_id v] uses.db

let unknown_code_id_actually_directly_called_query =
  let^? [closure], [known_arity_call_witness] =
    ["closure"], ["known_arity_call_witness"]
  in
  [ rev_accessor ~base:closure
      !!Field.known_arity_call_witness
      ~to_:known_arity_call_witness;
    any_source known_arity_call_witness ]

let code_id_actually_directly_called_query =
  query
    (let^$ [closure], [apply_widget; call_witness; codeid] =
       ["closure"], ["apply_widget"; "call_witness"; "codeid"]
     in
     [ rev_accessor ~base:closure
         !!Field.known_arity_call_witness
         ~to_:apply_widget;
       sources apply_widget call_witness;
       has_source call_witness;
       constructor ~base:call_witness
         !!Field.code_id_of_call_witness
         ~from:codeid ]
     =>? [codeid])

let code_id_actually_directly_called uses closure =
  let closure = Code_id_or_name.name closure in
  if unknown_code_id_actually_directly_called_query [closure] uses.db
  then Or_unknown.Unknown
  else
    Or_unknown.Known
      (Datalog.Cursor.fold_with_parameters
         code_id_actually_directly_called_query [closure] uses.db
         ~init:Code_id.Set.empty ~f:(fun [codeid] acc ->
           let codeid =
             Code_id_or_name.pattern_match' codeid
               ~code_id:(fun code_id -> code_id)
               ~name:(fun name ->
                 Misc.fatal_errorf
                   "code_id_actually_directly_called found a name: %a"
                   Name.print name)
           in
           Code_id.Set.add codeid acc))

type sources =
  | Any_source
  | Sources of unit Code_id_or_name.Map.t

let get_direct_sources :
    Datalog.database -> unit Code_id_or_name.Map.t -> sources =
  let open! Fixit in
  run
    (let@ in_ = param "in_" Cols.[n] in
     let+ [any; out] =
       let@ [any; out] = fix' [empty One.cols; empty Cols.[n]] in
       [ (let$ [x] = ["x"] in
          [in_ % [x]; any_source x] ==> One.flag any);
         (let$ [x; y] = ["x"; "y"] in
          [~~(One.flag any); in_ % [x]; sources x y; has_source y] ==> out % [y])
       ]
     in
     if One.to_bool any then Any_source else Sources out)

let get_field_sources :
    Datalog.database -> unit Code_id_or_name.Map.t -> Field.t -> sources =
  let open! Fixit in
  run
    (let@ in_ = param "in_" Cols.[n] in
     let@ in_field = param1s "in_field" Cols.f in
     let+ [any; out] =
       let@ [any; out] = fix' [empty One.cols; empty Cols.[n]] in
       [ (let$ [x; field; y] = ["x"; "field"; "y"] in
          [ ~~(One.flag any);
            in_ % [x];
            in_field % [field];
            constructor ~base:x field ~from:y;
            any_source y ]
          ==> One.flag any);
         (let$ [x; field; y; z] = ["x"; "field"; "y"; "z"] in
          [ ~~(One.flag any);
            in_ % [x];
            in_field % [field];
            constructor ~base:x field ~from:y;
            sources y z;
            has_source z ]
          ==> out % [z]) ]
     in
     if One.to_bool any then Any_source else Sources out)

let cofield_has_use :
    Datalog.database -> unit Code_id_or_name.Map.t -> Cofield.t -> bool =
  let open! Fixit in
  run
    (let@ in_ = param "in_" Cols.[n] in
     let@ in_field = param1s "in_field" Cols.cf in
     let+ out =
       let@ out = fix1' (empty One.cols) in
       [ (let$ [x; field; y] = ["x"; "field"; "y"] in
          [ in_ % [x];
            in_field % [field];
            parameter ~base:x field ~to_:y;
            has_usage y ]
          ==> One.flag out) ]
     in
     One.to_bool out)

let rec arguments_used_by_call db ep callee_sources grouped_args =
  match grouped_args with
  | [] -> []
  | first_arg_group :: grouped_args_rest -> (
    match callee_sources with
    | Any_source -> List.map (List.map (fun x -> x, Keep)) grouped_args
    | Sources callee_sources -> (
      let witness_sources =
        get_field_sources db callee_sources (Field.call_witness ep)
      in
      match witness_sources with
      | Any_source -> List.map (List.map (fun x -> x, Keep)) grouped_args
      | Sources witness_sources ->
        let first_arg_group =
          List.mapi
            (fun i x ->
              ( x,
                if cofield_has_use db witness_sources (Cofield.param i)
                then Keep
                else Delete ))
            first_arg_group
        in
        let grouped_args_rest =
          match grouped_args_rest with
          | [] -> [] (* Avoid computing sources of result if no more args *)
          | _ :: _ ->
            arguments_used_by_call db ep
              (get_field_sources db witness_sources
                 (Field.normal_return_of_call 0))
              grouped_args_rest
        in
        first_arg_group :: grouped_args_rest))

let arguments_used_by_known_arity_call result callee args =
  List.flatten
    (arguments_used_by_call result.db Field.Known_arity_code_pointer
       (get_direct_sources result.db (Code_id_or_name.Map.singleton callee ()))
       [args])

let arguments_used_by_unknown_arity_call result callee args =
  arguments_used_by_call result.db Field.Unknown_arity_code_pointer
    (get_direct_sources result.db (Code_id_or_name.Map.singleton callee ()))
    args

let has_source uses v = has_source_query uses.db v
