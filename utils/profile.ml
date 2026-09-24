(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Pierre Chambart, OCamlPro                         *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-18-40-42-48"]

type file = string

module Int = Misc.Stdlib.Int
module String = Misc.Stdlib.String

module Counters = struct
  type t = int String.Map.t

  let create () = String.Map.empty
  let get name t = String.Map.find_opt name t |> Option.value ~default:0
  let set name count t = String.Map.add name count t
  let incr name t =
    String.Map.update name
      (fun count_opt ->
        let count = Option.value ~default:0 count_opt in
        Some (succ count))
      t
  let is_empty = String.Map.is_empty
  let union = String.Map.union (fun _ count1 count2 -> Some (count1 + count2))
  let to_string t =
    t
    |> String.Map.bindings
    |> List.map (fun (name, count) -> Printf.sprintf "%s = %d" name count)
    |> String.concat "; "
    |> Printf.sprintf "[%s]"
end

external time_include_children: bool -> float = "caml_sys_time_include_children"
let cpu_time () = time_include_children true

(* Reading the CPU clock is a system call, which is too slow to do around every
   pass, so the compiler drivers install [Unix.gettimeofday] via
   [record_action]. Compiler-libs itself cannot depend on [Unix], hence the
   default. *)
let clock = ref cpu_time
let calls = ref 0
let time () = incr calls; !clock ()

module Measure = struct
  type t = {
    time : float;
    calls : int;
    allocated_words : float;
    counters : Counters.t;
  }
  let create ?(counters = Counters.create ()) () =
    (* [Gc.counters] is much cheaper than [Gc.quick_stat], which has to walk
       the heap statistics of every domain. *)
    let minor_words, promoted_words, major_words = Gc.counters () in
    {
      time = time ();
      calls = !calls;
      allocated_words = minor_words +. major_words -. promoted_words;
      counters = counters;
    }
  let zero = {
    time = 0.;
    calls = 0;
    allocated_words = 0.;
    counters = Counters.create ();
  }
end

module Measure_diff = struct
  let timestamp = let r = ref (-1) in fun () -> incr r; !r
  type t = {
    timestamp : int;
    calls : int;
    duration : float;
    allocated_words : float;
    counters : Counters.t;
    invocations : int;
  }
  let zero () = {
    timestamp = timestamp ();
    calls = 0;
    duration = 0.;
    allocated_words = 0.;
    counters = Counters.create ();
    invocations = 0;
  }
  let accumulate t (m1 : Measure.t) (m2 : Measure.t) = {
    timestamp = t.timestamp;
    calls = t.calls + (m2.calls - m1.calls);
    duration = t.duration +. (m2.time -. m1.time);
    allocated_words =
      t.allocated_words +. (m2.allocated_words -. m1.allocated_words);
    counters = Counters.union t.counters m2.counters;
    invocations = t.invocations + 1;
  }
  let of_diff m1 m2 =
    accumulate (zero ()) m1 m2
end

type hierarchy =
  | E of (string, Measure_diff.t * hierarchy) Hashtbl.t
[@@unboxed]

(* Set while inside a pass recorded with [~debug_only:true]; while set, no
   profile information is recorded for that pass or anything beneath it. *)
let suppressed = ref false

let create () = E (Hashtbl.create 2)
let hierarchy = ref (create ())
let reset () = hierarchy := create (); suppressed := false

(* Baseline for the total, and so for the toplevel "other" row. *)
let startup_measure = ref Measure.zero

let record_call_internal ?(accumulate = false) ?(debug_only = false) ?counter_f name f =
  if !Clflags.profile_columns = [] && not (Action_trace.enabled ()) || !suppressed
  then f ()
  else if debug_only then begin
    suppressed := true;
    Misc.try_finally f ~always:(fun () -> suppressed := false)
  end else
  let E prev_hierarchy = !hierarchy in
  let start_measure = Measure.create () in
  let this_measure_diff, this_table =
    (* We allow the recording of multiple categories by the same name, for tools
       like ocamldoc that use the compiler libs but don't care about profile
       information, and so may record, say, "parsing" multiple times. *)
    if accumulate
    then
      match Hashtbl.find prev_hierarchy name with
      | exception Not_found -> Measure_diff.zero (), Hashtbl.create 2
      | measure_diff, E table ->
        Hashtbl.remove prev_hierarchy name;
        measure_diff, table
    else Measure_diff.zero (), Hashtbl.create 2
  in
  hierarchy := E this_table;
  let counters = ref (Counters.create ()) in
  Misc.try_finally (
    match counter_f with
    | Some counter_f ->
        fun () ->
          let result = f () in
          if List.mem `Counters !Clflags.profile_columns
             || Action_trace.enabled () then
            counters := counter_f result;
          result
    | None -> f
    )
    ~always:(fun () ->
        hierarchy := E prev_hierarchy;
        let end_measure = Measure.create ~counters:(!counters) () in
        let measure_diff =
          Measure_diff.accumulate this_measure_diff start_measure end_measure in
        Hashtbl.add prev_hierarchy name (measure_diff, E this_table))

let record_call = record_call_internal ?counter_f:None

let record_call_with_counters ?accumulate ?debug_only ~counter_f =
  record_call_internal ?accumulate ?debug_only ~counter_f

let record ?accumulate ?debug_only pass f x =
  record_call ?accumulate ?debug_only pass (fun () -> f x)

let record_with_counters ?accumulate ?debug_only ~counter_f pass f x =
  record_call_internal ?accumulate ?debug_only ~counter_f pass (fun () -> f x)

let file_prefix = "file="

let annotate_file_name name =
  let file_path = Clflags.prepend_directory name in
  file_prefix ^ file_path

type display = {
  to_string : max:float -> width:int -> string;
  worth_displaying : max:float -> bool;
}

let time_display precision c v : display =
  (* Because indentation is meaningful, and because the durations are
     the first element of each row, we can't pad them with spaces. *)
  let to_string_without_unit v ~width = Printf.sprintf "%0*.*f" width precision v in
  let to_string ~max:_ ~width =
    to_string_without_unit v ~width:(width - 1)
    ^ "s (" ^ string_of_int c ^ ")" in
  let worth_displaying ~max:_ =
    float_of_string (to_string_without_unit v ~width:0) <> 0. || c > 1 in
  { to_string; worth_displaying }

let memory_word_display =
  (* To make memory numbers easily comparable across rows, we choose a single
     scale for an entire column. To keep the display compact and not overly
     precise (no one cares about the exact number of bytes), we pick the largest
     scale we can and we only show 3 digits. Avoiding showing tiny numbers also
     allows us to avoid displaying passes that barely allocate compared to the
     rest of the compiler.  *)
  let bytes_of_words words = words *. float_of_int (Sys.word_size / 8) in
  let to_string_without_unit v ~width scale =
    let precision = 3 and precision_power = 1e3 in
    let v_rescaled = bytes_of_words v /. scale in
    let v_rounded =
      floor (v_rescaled *. precision_power +. 0.5) /. precision_power in
    let v_str = Printf.sprintf "%.*f" precision v_rounded in
    let index_of_dot = String.index v_str '.' in
    let v_str_truncated =
      String.sub v_str 0
        (if index_of_dot >= precision
         then index_of_dot
         else precision + 1)
    in
    Printf.sprintf "%*s" width v_str_truncated
  in
  let choose_memory_scale =
    let units = [|"B"; "kB"; "MB"; "GB"|] in
    fun words ->
      let bytes = bytes_of_words words in
      let scale = ref (Array.length units - 1) in
      while !scale > 0 && bytes < 1024. ** float_of_int !scale do
        decr scale
      done;
      1024. ** float_of_int !scale, units.(!scale)
  in
  fun v : display ->
    let to_string ~max ~width =
      let scale, scale_str = choose_memory_scale max in
      let width = width - String.length scale_str in
      to_string_without_unit v ~width scale ^ scale_str
    in
    let worth_displaying ~max =
      let scale, _ = choose_memory_scale max in
      float_of_string (to_string_without_unit v ~width:0 scale) <> 0.
    in
    { to_string; worth_displaying }

let counters_display counters  =
  let to_string ~max:_ ~width:_ = Counters.to_string counters in
  let worth_displaying ~max:_ = not (Counters.is_empty counters) in
  0., { to_string; worth_displaying }

let profile_list (E table) =
  let l = Hashtbl.fold (fun k d l -> (k, d) :: l) table [] in
  List.sort (fun (_, (p1, _)) (_, (p2, _)) ->
    compare p1.Measure_diff.timestamp p2.Measure_diff.timestamp) l

let compute_other_category (E table : hierarchy) (total : Measure_diff.t) =
  let r = ref total in
  Hashtbl.iter (fun _pass ((p2 : Measure_diff.t), _) ->
    let p1 = !r in
    r := {
      timestamp = p1.timestamp;
      calls = p1.calls - p2.calls;
      duration = p1.duration -. p2.duration;
      allocated_words = p1.allocated_words -. p2.allocated_words;
      counters = Counters.create ();
      invocations = 0;
    }
  ) table;
  !r

let profile_list_with_other ~nesting hierarchy total =
  let list = profile_list hierarchy in
  if list <> [] || nesting = 0 then
    list @ ["other", (compute_other_category hierarchy total, create ())]
  else []

type row = R of string * int * (float * display) list * row list

let rec map_hierarchy ~nesting make_row hierarchy total =
  let list = profile_list_with_other ~nesting hierarchy total in
  List.map (fun (name, (measure_diff, hierarchy)) ->
    let children =
      map_hierarchy ~nesting:(nesting + 1) make_row hierarchy measure_diff
    in
    make_row name measure_diff children
  ) list

let map_profile make_row hierarchy measure_diff =
  map_hierarchy ~nesting:0 make_row hierarchy measure_diff

let rows_of_hierarchy hierarchy measure_diff columns timings_precision =
  let make_row name (p : Measure_diff.t) children =
    let make value ~f = value, f value in
    let values = List.map (function
      | `Time ->
        make p.duration ~f:(time_display timings_precision p.calls)
      | `Alloc ->
        make p.allocated_words ~f:memory_word_display
      | `Counters -> counters_display p.counters
    ) columns in
    R (name, p.invocations, values, children)
  in
  map_profile make_row hierarchy measure_diff

let column_mapping = [
  `Time, "time";
  `Alloc, "alloc";
  `Counters, "counters"
]

let profile_json hierarchy measure_diff =
  let number value = `Number (Printf.sprintf "%.17g" value) in
  let memory words = number (words *. float_of_int (Sys.word_size / 8)) in
  let make_row name (p : Measure_diff.t) children =
    let values = List.map (fun (column, name) ->
      name, match column with
      | `Time -> number p.duration
      | `Alloc -> memory p.allocated_words
      | `Counters ->
        `Object (List.map (fun (name, count) ->
          name, `Number (string_of_int count)
        ) (String.Map.bindings p.counters))
    ) column_mapping in
    `Object (("name", `String name)
             :: ("calls", `Number (string_of_int p.calls))
             :: values @ ["children", `Array children])
  in
  `Array (map_profile make_row hierarchy measure_diff)

let snapshot () =
  let total = Measure_diff.of_diff !startup_measure (Measure.create ()) in
  !hierarchy, total

let record_action ~gettimeofday ~name f =
  clock := gettimeofday;
  (* Allocation counters start from zero at program start, but the wall clock
     does not. *)
  startup_measure := { Measure.zero with time = gettimeofday () };
  if not (Action_trace.enabled ()) then f () else
    let start = gettimeofday () in
    Fun.protect f ~finally:(fun () ->
      let finish = gettimeofday () in
      let hierarchy, total = snapshot () in
      let args = ["profile", profile_json hierarchy total] in
      let nanoseconds seconds = int_of_float (seconds *. 1e9) in
      Action_trace.with_fresh_context ~name ~f:(fun context ->
        Action_trace.Context.emit context
          (Action_trace.Event.span ~category:"compiler" ~name ~args
             ~start_in_nanoseconds:(nanoseconds start)
             ~finish_in_nanoseconds:(nanoseconds finish) ())))

let max_by_column ~n_columns rows =
  let a = Array.make n_columns 0. in
  let rec loop (R (_, _, values, rows)) =
    List.iteri (fun i (v, _) -> a.(i) <- Float.max a.(i) v) values;
    List.iter loop rows
  in
  List.iter loop rows;
  a

let width_by_column ~n_columns ~display_cell rows =
  let a = Array.make n_columns 1 in
  let rec loop (R (_, _, values, rows)) =
    List.iteri (fun i cell ->
      let _, str = display_cell i cell ~width:0 in
      a.(i) <- Int.max a.(i) (String.length str)
    ) values;
    List.iter loop rows;
  in
  List.iter loop rows;
  a

let output_rows
    ~(output_row :
        prefix:string -> cell_strings:string list -> name:string
        -> invocations:int -> unit)
    ~(new_prefix : prev:string -> curr_name:string -> string)
    ~(always_output_ancestors : bool)
    ~(pad_empty : bool)
    rows
  =
  let n_columns =
    match rows with
    | [] -> 0
    | R (_, _, values, _) :: _ -> List.length values
  in
  let maxs = max_by_column ~n_columns rows in
  let display_cell i (_, c) ~width =
    let display_cell = c.worth_displaying ~max:maxs.(i) in
    display_cell, if display_cell
                  then c.to_string ~max:maxs.(i) ~width
                  else if pad_empty then String.make width '-' else ""
  in
  let widths = width_by_column ~n_columns ~display_cell rows in
  (* We track print row functions in a queue to ensure ancestors not worth displaying have
  print functions executed if a descendant is worth displaying (possible with counters) *)
  let rec loop (R (name, invocations, values, rows)) ~prefix ~output_stack =
    let worth_displaying, cell_strings =
      values
      |> List.mapi (fun i cell -> display_cell i cell ~width:widths.(i))
      |> List.split
    in
    let should_output_row =
      List.exists (fun b -> b) worth_displaying || invocations > 10
    in
    let output_current () =
      let cell_strings = if should_output_row then cell_strings else [] in
      output_row ~prefix ~cell_strings ~name ~invocations
    in
    output_stack := output_current :: (if always_output_ancestors then !output_stack else []);
    if should_output_row then
      (List.rev !output_stack |> List.iter (fun f -> f ()); output_stack := []);
    List.iter (loop ~prefix:(new_prefix ~prev:prefix ~curr_name:name) ~output_stack) rows;
    if !output_stack <> [] then output_stack := List.tl !output_stack
  in
  List.iter (loop ~prefix:"" ~output_stack:(ref [])) rows

let output_columns output_rows_f columns ~timings_precision =
  match columns with
  | [] -> ()
  | _ :: _ ->
     let hierarchy, total = snapshot () in
     output_rows_f (rows_of_hierarchy hierarchy total columns timings_precision)

let print ppf =
  output_rows
    ~output_row:(fun ~prefix ~cell_strings ~name ~invocations ->
      let invocations =
        if invocations > 1 then Printf.sprintf " (%d calls)" invocations else ""
      in
      Format.fprintf ppf "%s%s %s%s@\n" prefix (String.concat " " cell_strings)
        name invocations)
    ~new_prefix:(fun ~prev ~curr_name:_ -> "  " ^ prev)
    ~always_output_ancestors:true
    ~pad_empty:true
  |> output_columns

let output_to_csv ppf columns =
  let sanitise_for_csv =
    String.map (fun c -> if Char.equal c ',' then '_' else c) in
  let to_csv cell_strings =
    cell_strings |> List.map sanitise_for_csv |> String.concat ","
  in
  let string_columns = List.map (fun col -> List.assoc col column_mapping) columns in
  Format.fprintf ppf "%s@\n" (to_csv ("pass name" :: string_columns));
  let add_suffix pass =
    let suffix = if String.starts_with ~prefix:file_prefix pass then "/" else "" in
    pass ^ suffix
  in
  let output_row_f =
    output_rows
      ~output_row:(fun ~prefix ~cell_strings ~name ~invocations:_ ->
        Format.fprintf ppf "%s@\n" (to_csv ((prefix ^ add_suffix name) :: cell_strings)))
      ~new_prefix:(fun ~prev ~curr_name ->
        Format.sprintf "%s%s/" prev (add_suffix curr_name))
      ~always_output_ancestors:false ~pad_empty:false
  in
  output_columns output_row_f columns

let all_columns = List.map fst column_mapping
let column_names = List.map snd column_mapping

let options_doc =
  Printf.sprintf
    " Print performance information for each pass\
   \n    The columns are: %s."
    (String.concat " " column_names)

let generate = "generate"
let transl = "transl"
let typing = "typing"
