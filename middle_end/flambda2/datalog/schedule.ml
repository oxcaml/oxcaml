(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Basile Clément, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2024--2025 OCamlPro SAS                                    *)
(*   Copyright 2024--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Heterogenous_list

(* CR bclement: It's a shame that we can't easily have provenance information as
   a runtime flag due to the way we evaluate rules before parsing arguments.
   There might be a better way, but we need to be cautious not to degrade
   performance when not using provenance. *)
let use_provenance = ref false

let enable_provenance_for_debug () = use_provenance := true

(** An ['a incremental] represents a value (database or table), paired with
    another copy representing the latest changes to the value. *)
type 'a incremental =
  { current : 'a;
    difference : 'a
  }

let incremental ~difference ~current = { current; difference }

let incremental_get f t = { current = f t.current; difference = f t.difference }

let incremental_set f v t =
  { current = f v.current t.current; difference = f v.difference t.difference }

(** A [binder] is a reference to the state of a single table during evaluation.
    It contains the state of the table at the start of evaluation, the current
    state of the table, and the difference between those. *)
type binder =
  | Binder :
      { table_id : ('t, 'k, 'v) Table.Id.t;
        previous : 't ref;
        current : 't incremental ref
      }
      -> binder

let print_binder ppf (Binder { table_id; _ }) = Table.Id.print ppf table_id

type rule_id = Rule_id of int [@@unboxed]

let fresh_rule_id =
  let cnt = ref 0 in
  fun () ->
    incr cnt;
    Rule_id !cnt

(* This is used to generate a unique name for rules, it just needs to be
   somewhat random, not necessarily secure in any way*)
let fasthash h =
  let h = h lxor (h lsr 23) in
  let h = h * 2388976653695081527 in
  let h = h lxor (h lsr 47) in
  h

let print_rule_id ppf (Rule_id id) =
  let n = ref (fasthash id) in
  for _ = 0 to 12 do
    let c = (36 + (!n mod 36)) mod 36 in
    let c =
      if c < 10
      then char_of_int (int_of_char '0' + c)
      else char_of_int (int_of_char 'a' + c - 10)
    in
    Format.pp_print_char ppf c;
    n := !n / 36
  done;
  assert (!n = 0)

let rule_id_to_string rule_id = Format.asprintf "%a" print_rule_id rule_id

type fact_id =
  | Fact_id : ('t, 'k, 'v) Table.Id.t * 'k Constant.hlist -> fact_id

let compare_fact_id (Fact_id (tid1, args1)) (Fact_id (tid2, args2)) =
  let c = Table.Id.compare tid1 tid2 in
  if c <> 0
  then c
  else
    let Equal = Table.Id.provably_equal_keys_exn tid1 tid2 in
    let columns1 = Table.Id.columns tid1 in
    Column.compare_keys columns1 args1 args2

module FactMap = Map.Make (struct
  type t = fact_id

  let compare = compare_fact_id
end)

type provenance =
  | Input
  | Rule of rule_id * Datalog.bindings

let add_if_not_exists sources tid args provenance =
  let fact = Fact_id (tid, args) in
  match FactMap.find_opt fact sources with
  | None -> FactMap.add fact provenance sources
  | Some _ -> sources

(** A rule consists of:

      - A [Cursor.t] to iterates on the entries produced by the right-hand
        side of the rule (hypotheses);

      - A list of [binder]s that are bound to the tables appearing in the
        left-hand side of the rule (conclusion) and updated throughout rule
        evaluation;

      - An unique identifier for logging/tracing purposes.

    The cursor embeds callbacks to update the [binder]s. *)
type rule =
  | Rule :
      { cursor : 'a Cursor.t;
        binders : binder list;
        provenance_table_ref : provenance FactMap.t ref;
        rule_id : rule_id
      }
      -> rule

type deduction =
  [ `Atom of Datalog.atom
  | `And of deduction list ]

let find_or_create_ref (type t k v) binders (table_id : (t, k, v) Table.Id.t) :
    t incremental ref =
  let uid = Table.Id.uid table_id in
  match Hashtbl.find_opt binders uid with
  | None ->
    let empty = Trie.empty (Table.Id.is_trie table_id) in
    let current = ref (incremental ~difference:empty ~current:empty) in
    let previous = ref empty in
    Hashtbl.replace binders uid (Binder { table_id; previous; current });
    current
  | Some (Binder { table_id = other_table_id; previous = _; current }) ->
    let Equal = Table.Id.provably_equal_exn other_table_id table_id in
    current

let deduce (atoms : deduction) =
  let rec fold f atoms acc =
    match atoms with
    | `Atom atom -> f atom acc
    | `And atoms -> List.fold_left (fun acc atoms -> fold f atoms acc) acc atoms
  in
  let rule_id = fresh_rule_id () in
  let binders : (int, binder) Hashtbl.t = Hashtbl.create 17 in
  let provenance_table_ref = ref FactMap.empty in
  let callbacks =
    fold
      (fun (Datalog.Atom (tid, args)) callbacks ->
        let is_trie = Table.Id.is_trie tid in
        let table_ref = find_or_create_ref binders tid in
        let[@inline] callback_fn ~accept keys =
          let incremental_table = !table_ref in
          match Trie.find_opt is_trie keys incremental_table.current with
          | Some _ -> ()
          | None ->
            (accept [@inlined hint]) ();
            table_ref
              := incremental
                   ~current:
                     (Trie.add_or_replace is_trie keys ()
                        incremental_table.current)
                   ~difference:
                     (Trie.add_or_replace is_trie keys ()
                        incremental_table.difference)
        in
        let name = Table.Id.name tid ^ ".insert" in
        let callback =
          if !use_provenance
          then
            Datalog.create_callback_with_bindings ~name
              (fun bindings keys ->
                callback_fn
                  ~accept:(fun [@inline] () ->
                    provenance_table_ref
                      := add_if_not_exists !provenance_table_ref tid keys
                           (Rule (rule_id, bindings) : provenance))
                  keys)
              args
          else
            Datalog.create_callback ~name
              (fun keys -> callback_fn ~accept:(fun [@inline] () -> ()) keys)
              args
        in
        callback :: callbacks)
      atoms []
  in
  let binders =
    Hashtbl.fold (fun _ binder binders -> binder :: binders) binders []
  in
  Datalog.map_program (Datalog.execute callbacks) (fun cursor ->
      let cursor = Cursor.With_parameters.without_parameters cursor in
      Rule { cursor; binders; provenance_table_ref; rule_id })

type stats =
  { timings : (rule_id, rule * float) Hashtbl.t;
    mutable provenance : provenance FactMap.t
  }

let rec vars : type a b c. (a, b, c) Column.hlist -> b Datalog.String.hlist =
  function
  | [] -> []
  | _column :: columns -> "_" :: vars columns

let provenance_from_db db =
  Table.Map.fold db ~init:FactMap.empty ~f:(fun (Binding (tid, _)) provenance ->
      let cursor =
        Datalog.compile
          (vars (Table.Id.columns tid))
          (fun args -> Datalog.where_atom tid args (Datalog.yield args))
      in
      let cursor = Cursor.With_parameters.without_parameters cursor in
      Cursor.naive_fold cursor db
        (fun args provenance -> add_if_not_exists provenance tid args Input)
        provenance)

let create_stats db =
  let provenance =
    if !use_provenance then provenance_from_db db else FactMap.empty
  in
  { timings = Hashtbl.create 17; provenance }

let add_timing ~stats (Rule { rule_id; _ } as rule) time =
  Hashtbl.replace stats.timings rule_id
    ( rule,
      time
      +. try snd (Hashtbl.find stats.timings rule_id) with Not_found -> -0. )

module CharMap = Map.Make (Char)

type char_trie =
  { count : int;
    trie : char_trie CharMap.t
  }

let rec add_to_trie char_trie word ~pos =
  if pos >= String.length word
  then { char_trie with count = char_trie.count + 1 }
  else
    let trie =
      CharMap.update word.[pos]
        (fun sub_trie ->
          let sub_trie =
            Option.value sub_trie ~default:{ count = 0; trie = CharMap.empty }
          in
          Some (add_to_trie sub_trie word ~pos:(pos + 1)))
        char_trie.trie
    in
    { count = char_trie.count + 1; trie }

let rec unique_prefix_len char_trie word ~pos =
  if pos >= String.length word
  then String.length word
  else if char_trie.count = 1
  then pos
  else
    match CharMap.find_opt word.[pos] char_trie.trie with
    | None -> String.length word
    | Some sub_trie -> unique_prefix_len sub_trie word ~pos:(pos + 1)

let print_string_with_unique_prefix len ppf s =
  Format.fprintf ppf "%t%s%t%s" Flambda_colours.expr_keyword
    (String.sub s 0 len) Flambda_colours.pop
    (String.sub s len (String.length s - len))

let print_rule char_trie ppf (Rule { cursor; binders; rule_id; _ }) =
  let rule_id = rule_id_to_string rule_id in
  let len = unique_prefix_len char_trie rule_id ~pos:0 in
  Format.fprintf ppf "%a:@ @[@[%a@]@ :- %a@]"
    (print_string_with_unique_prefix len)
    rule_id
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
       print_binder)
    binders Cursor.print cursor

let print_fact ppf (tid, args) =
  Format.fprintf ppf "@[%a(@;<1 2>@[<hv>%a@]@,)@]" Table.Id.print tid
    (Column.print_keys (Table.Id.columns tid))
    args

let print_provenance_group char_trie rule_id ppf provenance =
  let first = ref false in
  FactMap.iter
    (fun (Fact_id (tid, args)) bindings ->
      if not !first then Format.fprintf ppf "@ ";
      first := false;
      let rule_id = rule_id_to_string rule_id in
      let len = unique_prefix_len char_trie rule_id ~pos:0 in
      Format.fprintf ppf "@[<hv 2>%a :-@ @[<2>%a@ @[%a@]@]@]" print_fact
        (tid, args)
        (print_string_with_unique_prefix len)
        rule_id Datalog.print_bindings bindings)
    provenance

type table_provenance =
  | Table_provenance : ('t, 'k, 'v) Table.Id.t -> table_provenance

let print_provenance char_trie rule_defs ppf provenance =
  let group_by_table_id = Hashtbl.create 17 in
  FactMap.iter
    (fun (Fact_id (tid, args)) provenance ->
      if Table.Id.has_provenance tid
      then
        let input_provenance, group_by_rule_id =
          match Hashtbl.find_opt group_by_table_id (Table.Id.uid tid) with
          | None ->
            let input_provenance = ref FactMap.empty in
            let group_by_rule_id = Hashtbl.create 17 in
            Hashtbl.replace group_by_table_id (Table.Id.uid tid)
              (Table_provenance tid, input_provenance, group_by_rule_id);
            input_provenance, group_by_rule_id
          | Some (_, input_provenance, group_by_rule_id) ->
            input_provenance, group_by_rule_id
        in
        match (provenance : provenance) with
        | Input ->
          input_provenance := add_if_not_exists !input_provenance tid args ()
        | Rule (rule_id, bindings) ->
          let group =
            match Hashtbl.find_opt group_by_rule_id rule_id with
            | None -> FactMap.empty
            | Some group -> group
          in
          let group = add_if_not_exists group tid args bindings in
          Hashtbl.replace group_by_rule_id rule_id group)
    provenance;
  Hashtbl.iter
    (fun _ (Table_provenance tid, input_provenance, group_by_rule_id) ->
      if Hashtbl.length group_by_rule_id > 0
      then (
        let header = Format.asprintf "Provenance for %a" Table.Id.print tid in
        let header_len = String.length header in
        let header_marker = String.make header_len '=' in
        Format.fprintf ppf "@ @ @[<v 1>@[<v>%s@ %s@]@ " header header_marker;
        if not (FactMap.is_empty !input_provenance)
        then (
          Format.fprintf ppf "Input facts@ -----------@ ";
          FactMap.iter
            (fun (Fact_id (tid, args)) () ->
              Format.fprintf ppf "@ %a" print_fact (tid, args))
            !input_provenance);
        Hashtbl.iter
          (fun rule_id group ->
            let rule_id_s = rule_id_to_string rule_id in
            let len = unique_prefix_len char_trie rule_id_s ~pos:0 in
            let header =
              Format.asprintf "%a from %s" Table.Id.print tid rule_id_s
            in
            let header_len = String.length header in
            let header_marker = String.make header_len '-' in
            Format.fprintf ppf "@ @ @[<v 1>@[<v>%a from %a@ %s@]@ "
              Table.Id.print tid
              (print_string_with_unique_prefix len)
              rule_id_s header_marker;
            Format.fprintf ppf "%a@ " (print_rule char_trie)
              (Hashtbl.find rule_defs rule_id);
            print_provenance_group char_trie rule_id ppf group;
            Format.fprintf ppf "@]")
          group_by_rule_id;
        Format.fprintf ppf "@]"))
    group_by_table_id

let print_stats ppf stats =
  let char_trie = { count = 0; trie = CharMap.empty } in
  let char_trie =
    if !use_provenance
    then
      Hashtbl.fold
        (fun _ (Rule { rule_id; _ }, _time) char_trie ->
          add_to_trie char_trie (rule_id_to_string rule_id) ~pos:0)
        stats.timings char_trie
    else char_trie
  in
  Format.fprintf ppf "@[<v>";
  let rule_defs = Hashtbl.create 17 in
  Hashtbl.iter
    (fun _ ((Rule { rule_id; _ } as rule), time) ->
      Hashtbl.replace rule_defs rule_id rule;
      Format.fprintf ppf "@[<v 2>%a@,: %f@]@ " (print_rule char_trie) rule time)
    stats.timings;
  if !use_provenance
  then (
    Format.fprintf ppf "Provenance@ ==========@ ";
    print_provenance char_trie rule_defs ppf stats.provenance);
  Format.fprintf ppf "@]"

(** Evaluate a single rule using semi-naive evaluation.

    The [previous], [diff], and [current] parameters represent the state of the
    database in which we are evaluating the cursor (see the documentaion of
    {!Cursor.seminaive_run}).

    The [incremental_db] parameter is the database where we are accumulating the
    result of the rule, and its new value is returned.

    {b Note}: there needs not be any relationship between the input database
    represented by the [(previous, diff, current)] triple and the output
    database [incremental_db].
*)
let run_rule_incremental ?stats ~previous ~diff ~current incremental_db
    (Rule { binders; cursor; provenance_table_ref; _ } as rule) =
  (match stats with
  | None -> ()
  | Some stats -> provenance_table_ref := stats.provenance);
  List.iter
    (fun (Binder { table_id; previous; current }) ->
      let incremental_table =
        incremental_get (Table.Map.get table_id) incremental_db
      in
      previous := incremental_table.current;
      current := incremental_table)
    binders;
  let time0 = Sys.time () in
  Cursor.seminaive_run cursor ~previous ~diff ~current;
  let time1 = Sys.time () in
  let seminaive_time = time1 -. time0 in
  Option.iter (fun stats -> add_timing ~stats rule seminaive_time) stats;
  (match stats with
  | None -> ()
  | Some stats -> stats.provenance <- !provenance_table_ref);
  provenance_table_ref := FactMap.empty;
  let incremental_db =
    List.fold_left
      (fun incremental_db (Binder { table_id; previous; current }) ->
        let previous = !previous and { current; difference } = !current in
        if previous == current
        then incremental_db
        else
          incremental_set (Table.Map.set table_id) { current; difference }
            incremental_db)
      incremental_db binders
  in
  incremental_db

type t =
  | Saturate of rule list
  | Fixpoint of t list

let fixpoint schedule = Fixpoint schedule

let saturate rules = Saturate rules

let run_rules_incremental ?stats rules ~previous ~diff ~current incremental_db =
  List.fold_left
    (fun incremental_db rule ->
      run_rule_incremental ?stats ~previous ~diff ~current incremental_db rule)
    incremental_db rules

(** Repeatedly apply the rules in [rules] to the database [current] until
    reaching a fixpoint.

    Returns an incremental database containing the new state of [current] along
    with all the new facts added during saturation.
*)
let saturate_rules_incremental ?stats rules ~previous ~diff ~current =
  let rec saturate_rules_incremental ?stats ~previous ~diff ~current rules
      full_diff =
    (* After one call to [run_rules_incremental], all deductions from facts in
       [current] have been processed, so we only need to keep evaluating rules
       with at least one fact in [incremental_db.difference]. *)
    let incremental_db =
      run_rules_incremental ?stats ~previous ~diff ~current rules
        (incremental ~current ~difference:Table.Map.empty)
    in
    if Table.Map.is_empty incremental_db.difference
    then incremental ~current ~difference:full_diff
    else
      saturate_rules_incremental ?stats ~previous:current
        ~diff:incremental_db.difference ~current:incremental_db.current rules
        (Table.Map.concat ~earlier:full_diff ~later:incremental_db.difference)
  in
  saturate_rules_incremental ?stats rules Table.Map.empty ~previous ~diff
    ~current

(** Run the evaluation functions in [fns] until reaching a fixpoint.
*)
let run_list_incremental fns ~previous ~diff ~current =
  (* Each evaluation of a rule that produced changes is associated with a
     timestamp (the initial used-provided [diff] is at timestamp [0]), and each
     evaluation function is associated with the state of the database last time
     it was run (initially [previous]) and the corresponding timestamp
     (initially [-1]).

     Before evaluating a function [fn], we compute the diff since its previous
     run by concatenating all the diffs with a higher timestamp. *)
  let rec cut ~cut_after result = function
    | [] -> result
    | (ts, diff) :: diffs ->
      if ts > cut_after
      then cut ~cut_after (Table.Map.concat ~earlier:diff ~later:result) diffs
      else result
  in
  let rec loop (current, diffs, ts, full_diff) fns =
    let (current, diffs, ts', full_diff), fns =
      List.fold_left_map
        (fun (db, diffs, ts, full_diff) (fn, previous, cut_after) ->
          let diff = cut ~cut_after Table.Map.empty diffs in
          let incremental_db = fn ~previous ~diff ~current:db in
          if Table.Map.is_empty incremental_db.difference
          then (db, diffs, ts, full_diff), (fn, db, ts)
          else
            let ts = ts + 1 in
            ( ( incremental_db.current,
                (ts, incremental_db.difference) :: diffs,
                ts,
                Table.Map.concat ~earlier:full_diff
                  ~later:incremental_db.difference ),
              (fn, incremental_db.current, ts) ))
        (current, diffs, ts, full_diff)
        fns
    in
    if ts' = ts
    then incremental ~current ~difference:full_diff
    else loop (current, diffs, ts', full_diff) fns
  in
  loop
    (current, [0, diff], 0, Table.Map.empty)
    (List.map (fun fn -> fn, previous, -1) fns)

let rec run_incremental ?stats schedule ~previous ~diff ~current =
  match schedule with
  | Saturate rules ->
    saturate_rules_incremental ?stats rules ~previous ~diff ~current
  | Fixpoint schedules ->
    run_list_incremental
      (List.map (run_incremental ?stats) schedules)
      ~previous ~diff ~current

let run ?stats schedule db =
  (run_incremental ?stats schedule ~previous:Table.Map.empty ~diff:db
     ~current:db)
    .current

let run_stratified ?stats schedules db =
  List.fold_left (fun db schedule -> run ?stats schedule db) db schedules
