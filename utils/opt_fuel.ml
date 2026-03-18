(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                    Tobias Tebbi, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.           *)
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

let output_prefix = ref ""

let make_absolute path =
  if Filename.is_relative path
  then Filename.concat (Sys.getcwd ()) path
  else path

let file_id =
  lazy
    (if String.length !output_prefix > 0
     then make_absolute !output_prefix
     else make_absolute !Location.input_name)

let local_step = ref 0

let flush_record () =
  match record_path with
  | None -> ()
  | Some path ->
    if !local_step > 0
    then (
      let oc = open_out_gen [Open_append; Open_creat; Open_text] 0o644 path in
      Printf.fprintf oc "%s %d\n" (Lazy.force file_id) !local_step;
      close_out oc)

let register_record_flush =
  let do_register = lazy (at_exit flush_record) in
  fun () -> Lazy.force do_register

(* Scan record file once to find this file's global offset *)
let step_range : (int * int) Lazy.t =
  lazy
    (match record_path with
    | Some path -> (
      let file = Lazy.force file_id in
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
                   if String.equal f file
                   then result := !global_offset, !global_offset + count;
                   global_offset := !global_offset + count
                 | None -> ())
               | None -> ()
           done
         with End_of_file -> ());
        close_in ic;
        !result
      with _ -> 0, 0)
    | _ -> 0, 0)

let format_message = function Some f -> " (" ^ f () ^ ")" | None -> ""

let should_do_opt_step ?message () : bool =
  let global_from, global_to = Lazy.force step_range in
  let step = global_from + !local_step in
  local_step := !local_step + 1;
  match record_path, threshold, fail_thresh with
  | None, None, None -> true
  | Some _, None, None ->
    register_record_flush ();
    local_step := !local_step + 1;
    true
  | Some record_path, Some thresh, None ->
    if step >= global_to
    then
      Misc.fatal_errorf
        "OPT_FUEL: step %d exceeds recorded max %d in %s%s; record file %s may \
         be stale or optimization non-deterministic."
        step global_to (Lazy.force file_id) (format_message message) record_path;
    step < thresh
  | Some record_path, None, Some fail_thresh ->
    if step = fail_thresh - 1
    then
      (* We are at the last step that caused failure with this threshold. *)
      Misc.fatal_errorf "OPT_FUEL: critical step %d in %s%s" step
        (Lazy.force file_id) (format_message message);
    true
  | _ ->
    Misc.fatal_error
      "OPT_FUEL: Either provide OPT_FUEL_RECORD+OPT_FUEL_THRESHOLD or \
       OPT_FUEL_RECORD+OPT_FUEL_FAIL"
