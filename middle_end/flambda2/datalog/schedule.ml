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
  | Rule of rule_id * Bytecode.bindings

let add_if_not_exists sources tid args provenance =
  let fact = Fact_id (tid, args) in
  match FactMap.find_opt fact sources with
  | None -> FactMap.add fact provenance sources
  | Some _ -> sources

type rule =
  { rule_id : rule_id;
    rule : Lang.rule
  }

type stats =
  { rules : (rule_id, Lang.rule) Hashtbl.t;
    mutable rule_stats : Cursor.Incremental.stats list;
    with_provenance : bool;
    mutable provenance : provenance FactMap.t
  }

let extra_atoms_for_provenance ~stats ~rule_id atom =
  let (Lang.Atom (relation, args, _)) = atom in
  match relation with
  | Table tid when Table.Id.has_provenance tid ->
    let provenance_fn bindings keys =
      let fact = Fact_id (tid, keys) in
      match FactMap.find_opt fact stats.provenance with
      | None ->
        let provenance : provenance =
          Rule (rule_id, Bytecode.get_bindings bindings)
        in
        stats.provenance <- FactMap.add fact provenance stats.provenance
      | Some _ -> ()
    in
    let name = Table.Id.name tid ^ ".provenance" in
    [Lang.callback_with_bindings ~name provenance_fn args]
  | If_let _ | Table _ | Unless _ | Distinct _ | Filter _
  | Callback_with_bindings _ ->
    []

let maybe_add_provenance ~stats ~rule_id rule =
  if not stats.with_provenance
  then rule
  else
    let extra_atoms =
      Iarray.fold_right
        (fun atom extra_atoms ->
          List.append
            (extra_atoms_for_provenance ~stats ~rule_id atom)
            extra_atoms)
        rule.Lang.head []
    in
    let head = Iarray.append rule.Lang.head (Iarray.of_list extra_atoms) in
    { rule with Lang.head }

let create_rule rule = { rule_id = fresh_rule_id (); rule }

let provenance_from_db db =
  Table.Map.fold db ~init:FactMap.empty ~f:(fun (Binding (tid, _)) provenance ->
      match Table.Map.get_or_null tid db with
      | Null -> provenance
      | This table ->
        Column.fold_hlist (Table.Id.columns tid)
          (fun keys _ provenance -> add_if_not_exists provenance tid keys Input)
          table provenance)

let create_stats ?(with_provenance = false) db =
  let provenance =
    if with_provenance then provenance_from_db db else FactMap.empty
  in
  { rules = Hashtbl.create 17; rule_stats = []; provenance; with_provenance }

let record_rule ~stats rule =
  let { rule_id; rule } = rule in
  Hashtbl.replace stats.rules rule_id rule

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

let print_rule ~rule_id char_trie ppf rule =
  let rule_id = rule_id_to_string rule_id in
  let len = unique_prefix_len char_trie rule_id ~pos:0 in
  Format.fprintf ppf "%a:@ @[%a@]"
    (print_string_with_unique_prefix len)
    rule_id Lang.print_rule rule

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
        rule_id Bytecode.print_bindings bindings)
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
            Format.fprintf ppf "%a@ "
              (print_rule ~rule_id char_trie)
              (Hashtbl.find rule_defs rule_id);
            print_provenance_group char_trie rule_id ppf group;
            Format.fprintf ppf "@]")
          group_by_rule_id;
        Format.fprintf ppf "@]"))
    group_by_table_id

let print_stats ppf stats =
  let char_trie = { count = 0; trie = CharMap.empty } in
  let char_trie =
    if stats.with_provenance
    then
      Hashtbl.fold
        (fun rule_id _time char_trie ->
          add_to_trie char_trie (rule_id_to_string rule_id) ~pos:0)
        stats.rules char_trie
    else char_trie
  in
  Format.fprintf ppf "@[<v>";
  Format.pp_print_list ~pp_sep:Format.pp_print_space
    Cursor.Incremental.print_stats ppf stats.rule_stats;
  if stats.with_provenance
  then (
    Format.fprintf ppf "Provenance@ ==========@ ";
    print_provenance char_trie stats.rules ppf stats.provenance);
  Format.fprintf ppf "@]"

type t =
  | Saturate of Cursor.Incremental.t * rule list
  | Fixpoint of t list

let rec recompile_schedule_for_stats ~stats schedule =
  match schedule with
  | Fixpoint schedules ->
    Fixpoint
      (List.map
         (fun schedule -> recompile_schedule_for_stats ~stats schedule)
         schedules)
  | Saturate (_, rules) ->
    List.iter (record_rule ~stats) rules;
    let cursor =
      Cursor.Incremental.from_rules ~with_stats:true
        (List.map
           (fun { rule_id; rule } -> maybe_add_provenance ~stats ~rule_id rule)
           rules)
    in
    Saturate (cursor, rules)

let maybe_recompile_for_stats ?stats schedule =
  match stats with
  | None -> schedule
  | Some stats -> recompile_schedule_for_stats ~stats schedule

let fixpoint schedule = Fixpoint schedule

let saturate rules =
  let rules = List.map create_rule rules in
  Saturate
    ( Cursor.Incremental.from_rules (List.map (fun { rule; _ } -> rule) rules),
      rules )

(** Run the evaluation functions in [fns] until reaching a fixpoint. *)
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
          let ~output, ~added = fn ~previous ~diff ~current:db in
          if Table.Map.is_empty added
          then (db, diffs, ts, full_diff), (fn, db, ts)
          else
            let ts = ts + 1 in
            ( ( output,
                (ts, added) :: diffs,
                ts,
                Table.Map.concat ~earlier:full_diff ~later:added ),
              (fn, output, ts) ))
        (current, diffs, ts, full_diff)
        fns
    in
    if ts' = ts
    then ~output:current, ~added:full_diff
    else loop (current, diffs, ts', full_diff) fns
  in
  loop
    (current, [0, diff], 0, Table.Map.empty)
    (List.map (fun fn -> fn, previous, -1) fns)

let rec run_incremental schedule ~previous ~diff ~current =
  match schedule with
  | Saturate (cursor, _rules) ->
    Cursor.Incremental.seminaive_run cursor ~previous ~diff ~current
  | Fixpoint schedules ->
    run_list_incremental
      (List.map run_incremental schedules)
      ~previous ~diff ~current

let rec collect_stats stats = function
  | Saturate (cursor, _) ->
    Cursor.Incremental.get_and_clear_stats cursor :: stats
  | Fixpoint schedules -> List.fold_left collect_stats stats schedules

let run ?stats schedule db =
  let schedule = maybe_recompile_for_stats ?stats schedule in
  let ~output, ~added:_ =
    run_incremental schedule ~previous:Table.Map.empty ~diff:db ~current:db
  in
  (match stats with
  | None -> ()
  | Some stats -> stats.rule_stats <- collect_stats stats.rule_stats schedule);
  output
