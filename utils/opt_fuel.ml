(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                    Tobias Tebbi, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let record_path = Sys.getenv_opt "OPT_FUEL_RECORD"

let threshold =
  match Sys.getenv_opt "OPT_FUEL_THRESHOLD" with
  | Some s -> ( try Some (int_of_string s) with _ -> None)
  | None -> None

let fail_thresh =
  match Sys.getenv_opt "OPT_FUEL_FAIL" with
  | Some s -> ( try Some (int_of_string s) with _ -> None)
  | None -> None

let recording =
  match record_path, threshold, fail_thresh with
  | Some _, None, None -> true
  | _ -> false

let reading_record = Option.is_some threshold || Option.is_some fail_thresh

let file_id = ref (Sys.getcwd ())

let local_step = ref 0

let add_arg arg =
  assert (!local_step = 0);
  file_id := !file_id ^ ":" ^ arg

(* --- Recording --- *)

let flush_record () =
  if recording && !local_step > 0
  then (
    let path = Option.get record_path in
    let oc = open_out_gen [Open_append; Open_creat; Open_text] 0o644 path in
    Printf.fprintf oc "%s %d\n" !file_id !local_step;
    close_out oc;
    local_step := 0)

let _ = at_exit flush_record

(* --- Reading record for bisect/fail --- *)

let compute_step_range file_id =
  match record_path with
  | Some path -> (
    try
      let ic = open_in path in
      let global_offset = ref 0 in
      let result = ref (0, 0) in
      (try
         while true do
           let line = input_line ic in
           let line = String.trim line in
           if String.length line > 0
           then
             match String.rindex_opt line ' ' with
             | Some i -> (
               let f = String.sub line 0 i in
               let count_s =
                 String.sub line (i + 1) (String.length line - i - 1)
               in
               match int_of_string_opt count_s with
               | Some count ->
                 if String.equal f file_id
                 then result := !global_offset, !global_offset + count;
                 global_offset := !global_offset + count
               | None -> ())
             | None -> ()
         done
       with End_of_file -> ());
      close_in ic;
      !result
    with _ -> 0, 0)
  | _ -> 0, 0

let step_range =
  lazy (if reading_record then compute_step_range !file_id else 0, 0)

(* --- Public API --- *)

let format_message = function Some f -> " (" ^ f () ^ ")" | None -> ""

let should_do_opt_step ?message () : bool =
  if recording
  then (
    local_step := !local_step + 1;
    true)
  else if reading_record
  then begin
    let global_from, global_to = Lazy.force step_range in
    let step = global_from + !local_step in
    local_step := !local_step + 1;
    if step >= global_to
    then
      Misc.fatal_errorf
        "OPT_FUEL: step %d exceeds recorded max %d in %s%s; record file %s may \
         be stale or optimization non-deterministic."
        step global_to !file_id (format_message message)
        (Option.get record_path);
    match threshold, fail_thresh with
    | Some thresh, None -> step < thresh
    | None, Some fail_thresh ->
      if step = fail_thresh - 1
      then
        Misc.fatal_errorf "OPT_FUEL: critical step %d in %s%s" step !file_id
          (format_message message);
      true
    | _, _ ->
      Misc.fatal_error
        "OPT_FUEL: OPT_FUEL_THRESHOLD and OPT_FUEL_FAIL are mutually exclusive"
  end
  else true
