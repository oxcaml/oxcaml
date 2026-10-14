(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                  *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Adapted from Jane Street's build_action_trace kernel, without local modes so
   that it can be built by the bootstrap compiler. *)
let trace_dir = lazy (Sys.getenv_opt "DUNE_ACTION_TRACE_DIR")

let enabled () = Option.is_some (Lazy.force trace_dir)

module Event = struct
  type fields = (string * Json.t) list

  type counters = (string * int) list

  type t =
    | Instant of
        { name : string;
          category : string;
          time : int;
          args : fields option;
          counters : counters option
        }
    | Span of
        { name : string;
          category : string;
          start : int;
          duration : int;
          args : fields option;
          counters : counters option
        }

  let instant ?args ?counters ~category ~name ~time_in_nanoseconds:time () =
    Instant { name; category; time; args; counters }

  let span ?args ?counters ~category ~name ~start_in_nanoseconds:start
      ~finish_in_nanoseconds:finish () =
    Span { name; category; start; duration = finish - start; args; counters }

  (* Divide before converting to a string to preserve epoch microseconds. *)
  let microseconds ns = `Number (string_of_int (ns / 1_000))

  let add_args ~args (json : fields) =
    match args with None -> json | Some args -> ("args", `Object args) :: json

  let rec counters_to_json (counters : counters) : fields =
    match counters with
    | [] -> []
    | (name, count) :: counters ->
      (name, `Number (string_of_int count)) :: counters_to_json counters

  let add_counters ~counters (json : fields) =
    match counters with
    | None -> json
    | Some counters -> ("counters", `Object (counters_to_json counters)) :: json

  let to_chan t chan =
    let json =
      match t with
      | Instant { name; category; time; args; counters } ->
        [ "tid", `Number "0";
          "pid", `Number "0";
          "name", `String name;
          "cat", `String category;
          "ts", microseconds time ]
        |> add_counters ~counters |> add_args ~args
      | Span { name; category; start; duration; args; counters } ->
        [ "tid", `Number "0";
          "pid", `Number "0";
          "name", `String name;
          "cat", `String category;
          "ts", microseconds start;
          "dur", microseconds duration ]
        |> add_counters ~counters |> add_args ~args
    in
    (Json.write (`Object json) chan [@nontail])
end

let make_trace_dir =
  lazy
    (match Lazy.force trace_dir with
    | None -> None
    | Some dir as some -> (
      match Sys.mkdir dir 0o777 with
      | () -> some
      | exception Sys_error _ when Sys.is_directory dir -> some
      | exception _ -> None))

module Context = struct
  type state =
    | Open of
        { chan : out_channel;
          mutable no_events_yet : bool
        }
    | Closed
    | Disabled

  type t = state ref

  let create ~name =
    ref
      (match Lazy.force make_trace_dir with
      | None -> Disabled
      | Some trace_dir ->
        let _, chan =
          Filename.open_temp_file ~temp_dir:trace_dir name ".json"
        in
        Open { chan; no_events_yet = true })

  let emit t event =
    match !t with
    | Disabled -> ()
    | Closed -> failwith "Dune action tracing context has already been closed"
    | Open t ->
      let prefix =
        if t.no_events_yet
        then begin
          t.no_events_yet <- false;
          '['
        end
        else ','
      in
      output_char t.chan prefix;
      Event.to_chan event t.chan;
      output_char t.chan '\n';
      flush t.chan

  let close t =
    match !t with
    | Closed | Disabled -> ()
    | Open { chan; no_events_yet } ->
      t := Closed;
      output_string chan (if no_events_yet then "[\n]\n" else "]\n");
      close_out chan
end

let with_fresh_context ~name ~f =
  let t = Context.create ~name in
  Fun.protect (fun () -> f t) ~finally:(fun () -> Context.close t)
