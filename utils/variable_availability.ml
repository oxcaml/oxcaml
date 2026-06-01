(******************************************************************************
 *                                  OxCaml                                    *
 *                        Simon Spies, Jane Street                            *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

module Checkpoint = struct
  type t =
    | Typedtree
    | Raw_lambda
    | Lambda
    | Flambda2_input
    | Flambda2_simplified
    | Flambda2_output
    | Cmm
    | Debug_info

  let all =
    [ Typedtree;
      Raw_lambda;
      Lambda;
      Flambda2_input;
      Flambda2_simplified;
      Flambda2_output;
      Cmm;
      Debug_info ]

  let all_array = Array.of_list all

  let count = Array.length all_array

  let index = function
    | Typedtree -> 0
    | Raw_lambda -> 1
    | Lambda -> 2
    | Flambda2_input -> 3
    | Flambda2_simplified -> 4
    | Flambda2_output -> 5
    | Cmm -> 6
    | Debug_info -> 7

  let of_index i = all_array.(i)

  let display = function
    | Typedtree -> "typing"
    | Raw_lambda -> "raw-lambda"
    | Lambda -> "lambda"
    | Flambda2_input -> "flambda2-in"
    | Flambda2_simplified -> "flambda2-simpl"
    | Flambda2_output -> "flambda2-out"
    | Cmm -> "cmm"
    | Debug_info -> "debug-info"
end

type source_kind =
  | Parameter of int
  | Local
  | For_index
  | Comprehension_index
  | Letop_param
  | Alias

let string_of_source_kind = function
  | Parameter i -> Printf.sprintf "param[%d]" i
  | Local -> "local"
  | For_index -> "for"
  | Comprehension_index -> "comprehension"
  | Letop_param -> "letop"
  | Alias -> "alias"

module type Uid = sig
  type t

  val equal : t -> t -> bool

  val hash : t -> int

  val print : Format.formatter -> t -> unit

  val no_uid : t
end

module type Loc = sig
  type t

  val print_compact : Format.formatter -> t -> unit

  val print_with_file : Format.formatter -> t -> unit
end

module type S = sig
  type uid

  type loc

  type observed_id =
    | Source_uid of uid
    | Projected_source_uid of
        { source_uid : uid;
          field : int
        }

  type source_definition

  val is_enabled : unit -> bool

  val reset : unit -> unit

  val set_unit_name : string -> unit

  val register_source_function :
    display_name:string -> location:loc -> source_definition

  val register_source_module :
    display_name:string -> location:loc -> source_definition

  val register_source_variable :
    source_definition ->
    uid:uid ->
    name:string ->
    location:loc ->
    kind:source_kind ->
    unit

  val record_observation : checkpoint:Checkpoint.t -> observed_id -> unit

  type ok_drop_reason =
    | Merged_with of uid
    | Ignored_variable
    | Function_became_catch
    | Module_level

  val register_ok_drop : uid:uid -> reason:ok_drop_reason -> unit

  type gap_cause =
    | Phantom_let_dropped_in_to_cmm
    | Missing_phantom_let_for_let_optimization
    | Lmutlet_handler_loses_duid

  val register_gap : uid:uid -> cause:gap_cause -> unit

  val print_report : Format.formatter -> unit
end

module Make (Uid : Uid) (Loc : Loc) :
  S with type uid = Uid.t and type loc = Loc.t = struct
  type uid = Uid.t

  type loc = Loc.t

  type observed_id =
    | Source_uid of uid
    | Projected_source_uid of
        { source_uid : uid;
          field : int
        }

  module Uid_tbl = Hashtbl.Make (struct
    type t = uid

    let equal = Uid.equal

    let hash = Uid.hash
  end)

  type ok_drop_reason =
    | Merged_with of uid
    | Ignored_variable
    | Function_became_catch
    | Module_level

  type gap_cause =
    | Phantom_let_dropped_in_to_cmm
    | Missing_phantom_let_for_let_optimization
    | Lmutlet_handler_loses_duid

  type drop =
    | Ok of ok_drop_reason
    | Gap of gap_cause

  type source_definition_kind =
    | Module
    | Function

  type source_variable =
    { uid : uid;
      name : string;
      location : loc;
      kind : source_kind;
      (* [seen.(Checkpoint.index cp)] is whether [uid] was observed at [cp]. The
         whole set is kept (not just the furthest point) so a non-monotonic
         trace shows as a [(+N)] in the summary. *)
      seen : bool array
    }

  type source_definition =
    { kind : source_definition_kind;
      display_name : string;
      location : loc;
      mutable variables : source_variable list
    }

  type state =
    { mutable unit_name : string option;
      mutable source_definitions : source_definition list;
      uid_to_variable : source_variable Uid_tbl.t;
      (* How each eliminated uid was dropped: lossless ([Ok]) or a coverage gap
         ([Gap]). A dropped uid absent here is an unexplained gap. *)
      drops : drop Uid_tbl.t
    }

  let fresh_state () =
    { unit_name = None;
      source_definitions = [];
      uid_to_variable = Uid_tbl.create 32;
      drops = Uid_tbl.create 32
    }

  let state = ref (fresh_state ())

  let is_enabled () = !Clflags.dump_variable_availability

  let reset () = state := fresh_state ()

  let set_unit_name name = if is_enabled () then !state.unit_name <- Some name

  let register_source kind ~display_name ~location =
    let f = { kind; display_name; location; variables = [] } in
    let s = !state in
    s.source_definitions <- f :: s.source_definitions;
    f

  let register_source_function = register_source Function

  let register_source_module = register_source Module

  let uid_of_observed_id = function
    | Source_uid u -> u
    | Projected_source_uid { source_uid; _ } -> source_uid

  let record_observation ~checkpoint id =
    if is_enabled ()
    then
      let uid = uid_of_observed_id id in
      if not (Uid.equal uid Uid.no_uid)
      then
        match Uid_tbl.find_opt !state.uid_to_variable uid with
        | None -> ()
        | Some v -> v.seen.(Checkpoint.index checkpoint) <- true

  (* First-registered wins: the earliest classification is canonical (e.g. the
     original survivor for a merge); later passes that try to reclassify the
     same uid are ignored. *)
  let add_drop ~uid drop =
    if not (Uid.equal uid Uid.no_uid)
    then
      let s = !state in
      if Uid_tbl.mem s.drops uid then () else Uid_tbl.add s.drops uid drop

  let register_ok_drop ~uid ~reason =
    if is_enabled ()
    then
      match reason with
      | Merged_with surviving
        when Uid.equal surviving Uid.no_uid || Uid.equal surviving uid ->
        ()
      | Merged_with _ | Ignored_variable | Function_became_catch | Module_level
        ->
        add_drop ~uid (Ok reason)

  let register_gap ~uid ~cause = if is_enabled () then add_drop ~uid (Gap cause)

  let register_source_variable f ~uid ~name ~location ~kind =
    if (not (is_enabled ())) || Uid.equal uid Uid.no_uid
    then ()
    else
      let s = !state in
      (* A caller may register the same uid more than once (e.g. an enclosing
         handler registers a binder that a later traversal step then revisits);
         keep the first registration and skip duplicates. *)
      if Uid_tbl.mem s.uid_to_variable uid
      then ()
      else begin
        let v =
          { uid;
            name;
            location;
            kind;
            seen = Array.make Checkpoint.count false
          }
        in
        f.variables <- v :: f.variables;
        Uid_tbl.add s.uid_to_variable uid v;
        v.seen.(Checkpoint.index Checkpoint.Typedtree) <- true;
        (* Module-scope bindings are top-level lets, not runtime locals, so
           their absence from DWARF locals is expected: classify them as an
           ok-drop rather than leave them to surface as a coverage gap. *)
        match f.kind with
        | Module -> add_drop ~uid (Ok Module_level)
        | Function -> ()
      end

  (* The outcome of a variable, derived from its trace: [Present] if it reached
     the last checkpoint, otherwise [Dropped] across the edge after the furthest
     checkpoint it did reach. A non-monotonic trace (a uid re-observed after a
     gap) is not called out per-variable; it shows up as a [(+N)] increment in
     the pass-by-pass funnel. *)
  type outcome =
    | Present
    | Dropped of
        { from_ : Checkpoint.t;
          to_ : Checkpoint.t
        }

  let outcome_of_seen seen =
    let n = Array.length seen in
    if seen.(n - 1)
    then Present
    else begin
      let furthest = ref 0 in
      for i = 0 to n - 1 do
        if seen.(i) then furthest := i
      done;
      Dropped
        { from_ = Checkpoint.of_index !furthest;
          to_ = Checkpoint.of_index (!furthest + 1)
        }
    end

  let drop_for uid = Uid_tbl.find_opt !state.drops uid

  let is_ok_drop uid =
    match drop_for uid with Some (Ok _) -> true | Some (Gap _) | None -> false

  let reached seen cp = seen.(Checkpoint.index cp)

  let all_variables () =
    List.concat_map
      (fun (f : source_definition) -> f.variables)
      !state.source_definitions

  module Printing = struct
    let row_ok_reason = function
      | Merged_with surviving -> (
        match Uid_tbl.find_opt !state.uid_to_variable surviving with
        | Some v -> Printf.sprintf "merged-with-%s" v.name
        | None -> "merged")
      | Ignored_variable -> "ignored"
      | Function_became_catch -> "became-static-catch"
      | Module_level -> "module-level"

    (* Like [row_ok_reason] but aggregates all merges under one bucket. *)
    let tally_ok_reason = function
      | Merged_with _ -> "merged"
      | Ignored_variable -> "ignored"
      | Function_became_catch -> "became-static-catch"
      | Module_level -> "module-level"

    let gap_cause_token = function
      | Phantom_let_dropped_in_to_cmm -> "phantom-let-dropped-in-to_cmm"
      | Missing_phantom_let_for_let_optimization ->
        "missing-phantom-let-for-let-opt"
      | Lmutlet_handler_loses_duid -> "lmutlet-loses-duid"

    (* Dropped uid with no registered cause: a coverage gap whose responsible
       site has not been attributed with [register_gap] yet. Observed sources
       include tuple/record pattern destructuring lowered by
       [lambda/matching.ml] (the binders are dropped at the raw-lambda->lambda
       edge). *)
    let unknown_gap_token = "(unknown)"

    let percent n d =
      if d = 0
      then "0.00%"
      else Printf.sprintf "%.2f%%" (100. *. float n /. float d)

    let edge_str from_ to_ =
      Printf.sprintf "%s->%s" (Checkpoint.display from_)
        (Checkpoint.display to_)

    (* outcome / edge / verdict / reason cells of one variable's row. *)
    let cells_of_variable (v : source_variable) =
      match outcome_of_seen v.seen with
      | Present -> "present", "", "", ""
      | Dropped { from_; to_ } -> (
        let edge = edge_str from_ to_ in
        match drop_for v.uid with
        | Some (Ok reason) -> "dropped", edge, "[ok]", row_ok_reason reason
        | Some (Gap cause) -> "dropped", edge, "[gap]", gap_cause_token cause
        | None -> "dropped", edge, "[gap]", unknown_gap_token)

    let rstrip s =
      let n = ref (String.length s) in
      while !n > 0 && s.[!n - 1] = ' ' do
        decr n
      done;
      String.sub s 0 !n

    (* Fixed column widths keep each column at the same offset across functions
       and files, so report diffs move only where the data did. These are
       minimum widths sized for the common case: kind 13 ("comprehension"); loc
       14 (single-line "100000:999-999"); outcome 7 ("dropped"); edge 28
       ("flambda2-simpl->flambda2-out"); verdict 5 ("[gap]"). A longer value
       overflows its column for that row -- notably a multi-line loc, which
       prints as "line-line:col-col" -- shifting the later columns right on that
       line only. The reason trails, and trailing padding is stripped. *)
    let render_data (v : source_variable) =
      let outcome, edge, verdict, reason = cells_of_variable v in
      rstrip
        (Printf.sprintf "%-13s  %-14s  %-7s  %-28s  %-5s  %s"
           (string_of_source_kind v.kind)
           (Format.asprintf "%a" Loc.print_compact v.location)
           outcome edge verdict reason)

    let scope_word = function Module -> "Module" | Function -> "Function"

    let print_source_definition ppf (f : source_definition) =
      let vars = List.rev f.variables in
      Format.fprintf ppf "@,%s %s  %a@," (scope_word f.kind) f.display_name
        Loc.print_with_file f.location;
      let rows = List.map (fun v -> v.name, render_data v) vars in
      Misc.pp_two_columns ~sep:"|" ppf rows

    (* Survivors reaching each checkpoint, with the per-edge delta. A [(+N)]
       increment is only possible for a non-monotonic trace (a uid re-observed
       after a gap), which means an observation hook is missing. *)
    let print_funnel ppf ~tracked ~n_tracked =
      Format.fprintf ppf "@,Pass-by-pass survival:@,";
      let counts =
        List.map
          (fun cp ->
            ( cp,
              List.fold_left
                (fun acc v -> if reached v.seen cp then acc + 1 else acc)
                0 tracked ))
          Checkpoint.all
      in
      let w_name =
        List.fold_left
          (fun w (cp, _) -> max w (String.length (Checkpoint.display cp)))
          0 counts
      in
      let w_count = String.length (string_of_int n_tracked) in
      let _ : int option =
        List.fold_left
          (fun prev (cp, n) ->
            let delta =
              match prev with
              | Some p when n < p -> Printf.sprintf "  (-%d)" (p - n)
              | Some p when n > p -> Printf.sprintf "  (+%d)" (n - p)
              | Some _ | None -> ""
            in
            Format.fprintf ppf "  %-*s  %*d  (%7s)%s@," w_name
              (Checkpoint.display cp) w_count n (percent n n_tracked) delta;
            Some n)
          None counts
      in
      ()

    let bump tbl key =
      Hashtbl.replace tbl key
        (1 + try Hashtbl.find tbl key with Not_found -> 0)

    (* Sorted alphabetically so the row order does not depend on counts. *)
    let sorted_buckets tbl =
      Hashtbl.fold (fun reason count acc -> (reason, count) :: acc) tbl []
      |> List.sort (fun (r1, _) (r2, _) -> compare r1 r2)

    let print_drop_reasons ppf ~ok_drops ~tracked =
      let ok_tbl = Hashtbl.create 8 in
      List.iter
        (fun v ->
          match drop_for v.uid with
          | Some (Ok reason) -> bump ok_tbl (tally_ok_reason reason)
          | Some (Gap _) | None -> ())
        ok_drops;
      let gap_tbl = Hashtbl.create 8 in
      List.iter
        (fun v ->
          match outcome_of_seen v.seen with
          | Dropped _ -> (
            match drop_for v.uid with
            | Some (Gap cause) -> bump gap_tbl (gap_cause_token cause)
            | Some (Ok _) | None -> bump gap_tbl unknown_gap_token)
          | Present -> ())
        tracked;
      let ok_rows = sorted_buckets ok_tbl in
      let gap_rows = sorted_buckets gap_tbl in
      if ok_rows = [] && gap_rows = []
      then ()
      else begin
        let w_count =
          List.fold_left
            (fun w (_, c) -> max w (String.length (string_of_int c)))
            1 (ok_rows @ gap_rows)
        in
        let rows =
          List.map
            (fun (r, c) -> r, Printf.sprintf "%*d  (ok)" w_count c)
            ok_rows
          @ List.map
              (fun (r, c) -> r, Printf.sprintf "%*d  (gap)" w_count c)
              gap_rows
        in
        Format.fprintf ppf "@,Drop reasons:@,";
        Misc.pp_two_columns ~sep:"|" ppf rows
      end

    let print_unit_summary ppf =
      let all_vars = all_variables () in
      let ok_drops, tracked =
        List.partition (fun v -> is_ok_drop v.uid) all_vars
      in
      let total = List.length all_vars in
      if total = 0
      then ()
      else begin
        let n_excluded = List.length ok_drops in
        let n_tracked = List.length tracked in
        let reach =
          List.fold_left
            (fun acc v ->
              if reached v.seen Checkpoint.Debug_info then acc + 1 else acc)
            0 tracked
        in
        Format.fprintf ppf
          "@,\
           Compilation-unit summary: %d total variables; %d excluded \
           (ok-drops); %d/%d reach debug info (%s)@,"
          total n_excluded reach n_tracked (percent reach n_tracked);
        print_funnel ppf ~tracked ~n_tracked;
        print_drop_reasons ppf ~ok_drops ~tracked
      end

    let print_report ppf =
      if is_enabled ()
      then begin
        let s = !state in
        let unit_name = Option.value s.unit_name ~default:"<unknown>" in
        Format.fprintf ppf "@[<v 0>Variable availability for %s@," unit_name;
        let fns = List.rev s.source_definitions in
        List.iter (print_source_definition ppf) fns;
        print_unit_summary ppf;
        Format.fprintf ppf "@]@."
      end
  end

  let print_report = Printing.print_report
end
