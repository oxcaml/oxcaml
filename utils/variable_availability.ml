(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Simon Spies, Jane Street, London                     *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Checkpoint = struct
  type t =
    | Typedtree
    | Raw_lambda
    | Lambda
    | Flambda2_input
    | Flambda2_simplified
    | Flambda2_output
    | Cmm
    | Debug_info_variables
    | Debug_info_nonempty_range

  let all =
    [ Typedtree;
      Raw_lambda;
      Lambda;
      Flambda2_input;
      Flambda2_simplified;
      Flambda2_output;
      Cmm;
      Debug_info_variables;
      Debug_info_nonempty_range ]

  let equal a b =
    match a, b with
    | Typedtree, Typedtree
    | Raw_lambda, Raw_lambda
    | Lambda, Lambda
    | Flambda2_input, Flambda2_input
    | Flambda2_simplified, Flambda2_simplified
    | Flambda2_output, Flambda2_output
    | Cmm, Cmm
    | Debug_info_variables, Debug_info_variables
    | Debug_info_nonempty_range, Debug_info_nonempty_range -> true
    | ( ( Typedtree | Raw_lambda | Lambda | Flambda2_input | Flambda2_simplified
        | Flambda2_output | Cmm | Debug_info_variables
        | Debug_info_nonempty_range ),
        _ ) ->
      false

  let display = function
    | Typedtree -> "typing"
    | Raw_lambda -> "raw-lambda"
    | Lambda -> "lambda"
    | Flambda2_input -> "flambda2-in"
    | Flambda2_simplified -> "flambda2-simplified"
    | Flambda2_output -> "flambda2-out"
    | Cmm -> "cmm"
    | Debug_info_variables -> "debug-info"
    | Debug_info_nonempty_range -> "debug-info-nonempty"
end

type source_kind =
  | Parameter of int
  | Local
  | For_index
  | Comprehension_index
  | Letop_param
  | Alias

let string_of_source_kind = function
  | Parameter i -> Printf.sprintf "parameter[%d]" i
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

  val print : Format.formatter -> t -> unit
end

module type S = sig
  type uid

  type loc

  type observed_id =
    | Source_uid of uid
    | Projected_source_uid of { source_uid : uid; field : int }

  type source_function

  val is_enabled : unit -> bool

  val reset : unit -> unit

  val set_unit_name : string -> unit

  val register_source_function :
    display_name:string ->
    location:loc ->
    source_function

  val register_source_module :
    display_name:string ->
    location:loc ->
    source_function

  val register_source_variable :
    source_function ->
    uid:uid ->
    name:string ->
    location:loc ->
    kind:source_kind ->
    unit

  val record_observation : checkpoint:Checkpoint.t -> observed_id -> unit

  val register_merged_uid : surviving:uid -> merged:uid -> unit

  val print_report : Format.formatter -> unit
end

module Make (Uid : Uid) (Loc : Loc) :
  S with type uid = Uid.t and type loc = Loc.t = struct
  type uid = Uid.t

  type loc = Loc.t

  type observed_id =
    | Source_uid of uid
    | Projected_source_uid of { source_uid : uid; field : int }

  module Uid_tbl = Hashtbl.Make (struct
    type t = uid

    let equal = Uid.equal

    let hash = Uid.hash
  end)

  type source_variable =
    { uid : uid;
      name : string;
      location : loc;
      kind : source_kind
    }

  type source_function_kind =
    | Module
    | Function

  type source_function =
    { kind : source_function_kind;
      display_name : string;
      location : loc;
      mutable variables : source_variable list
    }

  type observation =
    { checkpoint : Checkpoint.t;
      id : observed_id
    }

  type state =
    { mutable unit_name : string option;
      mutable source_functions : source_function list;
      uid_to_function : source_function Uid_tbl.t;
      mutable observations : observation list;
      (* [merged_into] records the surviving uid for each merged uid:
         the source variable identified by the key has been
         alpha-renamed to share its runtime binding with the value. *)
      merged_into : uid Uid_tbl.t
    }

  let fresh_state () =
    { unit_name = None;
      source_functions = [];
      uid_to_function = Uid_tbl.create 64;
      observations = [];
      merged_into = Uid_tbl.create 16
    }

  (* @agent: top-level state. Is there a good way to propagate the hash
     table instead of keeping it as a singleton? *)
  let state = ref (fresh_state ())

  let is_enabled () = !Clflags.dump_variable_availability

  let reset () = state := fresh_state ()

  let set_unit_name name = (!state).unit_name <- Some name

  let register_source kind ~display_name ~location =
    let f = { kind; display_name; location; variables = [] } in
    let s = !state in
    s.source_functions <- f :: s.source_functions;
    f

  let register_source_function = register_source Function

  let register_source_module = register_source Module

  let uid_of_observed_id = function
    | Source_uid u -> u
    | Projected_source_uid { source_uid; _ } -> source_uid

  let record_observation ~checkpoint id =
    if is_enabled ()
       && not (Uid.equal (uid_of_observed_id id) Uid.no_uid)
    then
      let s = !state in
      s.observations <- { checkpoint; id } :: s.observations

  let register_merged_uid ~surviving ~merged =
    if (not (is_enabled ()))
       || Uid.equal merged Uid.no_uid
       || Uid.equal surviving Uid.no_uid
       || Uid.equal merged surviving
    then ()
    else
      let s = !state in
      (* If [merged] is already mapped, keep the original survivor:
         the typedtree iterator visits left-to-right, so the first
         registration carries the canonical survivor and subsequent
         calls would only re-establish the same edge. *)
      if Uid_tbl.mem s.merged_into merged
      then ()
      else Uid_tbl.add s.merged_into merged surviving

  let register_source_variable f ~uid ~name ~location ~kind =
    if Uid.equal uid Uid.no_uid
    then ()
    else
      let s = !state in
      (* The Tast iterator visits a function parameter's [Tpat_var] after
         we have already registered the parameter itself; skip the
         duplicate. *)
      if Uid_tbl.mem s.uid_to_function uid
      then ()
      else begin
        let v = { uid; name; location; kind } in
        f.variables <- v :: f.variables;
        Uid_tbl.add s.uid_to_function uid f;
        record_observation ~checkpoint:Checkpoint.Typedtree (Source_uid uid)
      end

  type status =
    | Present
    | Missing of Checkpoint.t
    | Short_lived
    | Dropped of
        { from_ : Checkpoint.t;
          to_ : Checkpoint.t
        }

  let status_for_variable obs =
    let seen_in cp =
      List.exists (fun o -> Checkpoint.equal o.checkpoint cp) obs
    in
    let rec scan prev = function
      | [] -> Present
      | cp :: rest ->
        if seen_in cp
        then scan (Some cp) rest
        else
          match prev with
          | None -> Missing cp
          | Some prev_cp ->
            (* A variable that reaches the DWARF emitter but has only
               trivial single-label subranges is "short-lived": its
               location list covers at most a byte or two of code. *)
            if Checkpoint.equal prev_cp Checkpoint.Debug_info_variables
               && Checkpoint.equal cp Checkpoint.Debug_info_nonempty_range
            then Short_lived
            else Dropped { from_ = prev_cp; to_ = cp }
    in
    scan None Checkpoint.all

  let observations_by_uid () =
    let tbl = Uid_tbl.create 64 in
    List.iter
      (fun o ->
        let uid = uid_of_observed_id o.id in
        let prev = try Uid_tbl.find tbl uid with Not_found -> [] in
        Uid_tbl.replace tbl uid (o :: prev))
      (!state).observations;
    tbl

  (* Walk the merge chain starting at [uid], stopping at a cycle or at
     a uid with no outgoing edge. Always includes [uid] itself. *)
  let merge_chain uid =
    let s = !state in
    let rec loop acc visited uid =
      if List.exists (Uid.equal uid) visited
      then List.rev acc
      else
        let visited = uid :: visited in
        let acc = uid :: acc in
        match Uid_tbl.find_opt s.merged_into uid with
        | None -> List.rev acc
        | Some next -> loop acc visited next
    in
    loop [] [] uid

  let observations_for_uid uid_to_obs uid =
    List.concat_map
      (fun u -> try Uid_tbl.find uid_to_obs u with Not_found -> [])
      (merge_chain uid)

  let is_merged uid = Uid_tbl.mem (!state).merged_into uid

  let reached_at ~uid_to_obs cp (v : source_variable) =
    let obs = observations_for_uid uid_to_obs v.uid in
    List.exists (fun o -> Checkpoint.equal o.checkpoint cp) obs

  module Printing = struct
    let string_of_status = function
      | Present -> "present"
      | Missing cp -> Printf.sprintf "missing @ %s" (Checkpoint.display cp)
      | Short_lived -> "short-lived"
      | Dropped { from_; to_ } ->
        Printf.sprintf "dropped @ %s -> %s"
          (Checkpoint.display from_)
          (Checkpoint.display to_)

    let percent n d =
      if d = 0
      then "0.00%"
      else Printf.sprintf "%.2f%%" (100. *. float n /. float d)

    let scope_header f =
      match f.kind with
      | Module -> Printf.sprintf "Module %s" f.display_name
      | Function -> Printf.sprintf "Function %s" f.display_name

    let print_variable_row ppf ~widths ~uid_to_obs (v : source_variable) =
      (* The uid is intentionally omitted from the report: its numeric
         stamp comes from a process-wide counter and shifts on unrelated
         changes, which would make any test comparing this output flake. *)
      let w_name, w_kind = widths in
      let format_kind = string_of_source_kind v.kind in
      let obs = observations_for_uid uid_to_obs v.uid in
      let status = string_of_status (status_for_variable obs) in
      let status =
        if is_merged v.uid then Printf.sprintf "merged; %s" status else status
      in
      Format.fprintf ppf "  %-*s  %-*s  %s  [%a]@," w_name v.name w_kind
        format_kind status Loc.print v.location

    let compute_column_widths (vars : source_variable list) =
      let max_w get =
        List.fold_left (fun m v -> max m (String.length (get v))) 0 vars
      in
      let w_name = max_w (fun v -> v.name) in
      let w_kind = max_w (fun v -> string_of_source_kind v.kind) in
      w_name, w_kind

    let count_reached ~uid_to_obs cp vars =
      List.fold_left
        (fun acc v -> if reached_at ~uid_to_obs cp v then acc + 1 else acc)
        0 vars

    let print_function_summary ppf ~uid_to_obs vars =
      let n = List.length vars in
      let reached =
        count_reached ~uid_to_obs Checkpoint.Debug_info_variables vars
      in
      let nonempty =
        count_reached ~uid_to_obs Checkpoint.Debug_info_nonempty_range vars
      in
      let short = reached - nonempty in
      Format.fprintf ppf
        "  Summary: %d / %d (%s) reached debug-info, %d / %d (%s) short-lived@,"
        reached n (percent reached n) short reached (percent short reached)

    let print_source_function ppf ~uid_to_obs (f : source_function) =
      let vars = List.rev f.variables in
      Format.fprintf ppf "@,@[<v 0>%s [%a]@," (scope_header f) Loc.print
        f.location;
      let widths = compute_column_widths vars in
      List.iter (print_variable_row ppf ~widths ~uid_to_obs) vars;
      if vars <> [] then print_function_summary ppf ~uid_to_obs vars;
      Format.fprintf ppf "@]"

    let print_unit_summary ppf ~uid_to_obs ~local_vars =
      let total_vars = List.length local_vars in
      if total_vars = 0
      then ()
      else begin
        let counts =
          List.map
            (fun cp -> cp, count_reached ~uid_to_obs cp local_vars)
            Checkpoint.all
        in
        let total_reached = List.assoc Checkpoint.Debug_info_variables counts in
        let total_nonempty =
          List.assoc Checkpoint.Debug_info_nonempty_range counts
        in
        let short_lived = total_reached - total_nonempty in
        Format.fprintf ppf
          "@,Compilation-unit summary: %d / %d (%s) reached debug-info, %d / \
           %d (%s) short-lived@,"
          total_reached total_vars
          (percent total_reached total_vars)
          short_lived total_reached
          (percent short_lived total_reached);
        Format.fprintf ppf
          "  (\"short-lived\" = reached debug-info but every subrange has \
           the same start and end label,@,";
        Format.fprintf ppf
          "   i.e. the location list covers at most a byte or two of code)@,";
        Format.fprintf ppf
          "  (module-scope bindings are excluded; they are not runtime \
           variable ranges)@,";
        let w_name =
          List.fold_left
            (fun w (cp, _) -> max w (String.length (Checkpoint.display cp)))
            0 counts
        in
        Format.fprintf ppf "Pass-by-pass survival:@,";
        let _ : int option =
          List.fold_left
            (fun prev (cp, n) ->
              let delta_str =
                match prev with
                | None -> ""
                | Some p when n < p -> Printf.sprintf "  (-%d)" (p - n)
                | Some p when n > p -> Printf.sprintf "  (+%d)" (n - p)
                | Some _ -> ""
              in
              Format.fprintf ppf "  %-*s  %6d  (%6s)%s@," w_name
                (Checkpoint.display cp)
                n (percent n total_vars) delta_str;
              Some n)
            None counts
        in
        ()
      end

    let print_report ppf =
      if is_enabled ()
      then begin
        let s = !state in
        let uid_to_obs = observations_by_uid () in
        let unit_name = Option.value s.unit_name ~default:"<unknown>" in
        Format.fprintf ppf "@[<v 0>Variable availability for %s@," unit_name;
        let fns = List.rev s.source_functions in
        List.iter (print_source_function ppf ~uid_to_obs) fns;
        let local_vars =
          List.concat_map
            (fun (f : source_function) ->
              match f.kind with Module -> [] | Function -> f.variables)
            fns
        in
        print_unit_summary ppf ~uid_to_obs ~local_vars;
        Format.fprintf ppf "@]@."
      end
  end

  let print_report = Printing.print_report
end
