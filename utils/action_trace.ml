(** This module is adapted from
    https://github.com/ocaml/dune/blob/main/otherlibs/dune-action-trace/dune_action_trace.ml,
    but heavily modified to work with Jane Street's internal version of dune and
    specialized for usage within the compiler. *)

let trace_dir = lazy (Sys.getenv_opt "DUNE_ACTION_TRACE_DIR")

(* Category name to use for all events. *)
let category = "ocaml-compiler"

let make_trace_dir =
  lazy
    (match Lazy.force trace_dir with
    | None -> None
    | Some dir as some -> (
      match Sys.mkdir dir 0o777 with
      | () -> some
      | exception Sys_error _ when Sys.is_directory dir -> some
      | exception _ -> None))

module Writer : sig
  val write_event : Json.t -> unit
end = struct
  type t =
    | Writer of
        { chan : Out_channel.t;
          mutable no_events_yet : bool
        }
    | Disabled

  let write_event_with_writer t event_json =
    match t with
    | Disabled -> ()
    | Writer t ->
      let prefix =
        if t.no_events_yet
        then begin
          t.no_events_yet <- false;
          '['
        end
        else ','
      in
      output_char t.chan prefix;
      Json.write event_json t.chan;
      output_char t.chan '\n';
      flush t.chan

  let open_writer () =
    match Lazy.force make_trace_dir with
    | None -> Disabled
    | Some trace_dir ->
      let _, chan =
        Filename.open_temp_file ~temp_dir:trace_dir "ocaml-compiler-trace"
          ".json"
      in
      Writer { chan; no_events_yet = true }

  let close_writer t =
    match t with
    | Disabled -> ()
    | Writer { chan; no_events_yet } ->
      output_string chan (if no_events_yet then "[\n]\n" else "]\n");
      close_out chan

  let global =
    lazy
      (let writer = open_writer () in
       at_exit (fun () -> close_writer writer);
       writer)

  let write_event event = write_event_with_writer (Lazy.force global) event
end

module Event = struct
  (* Divide before converting to a string to preserve epoch microseconds. *)
  let nanos_to_micros_as_json ns = `Number (string_of_int (ns / 1_000))

  let add_args ~args json =
    match args with None -> json | Some args -> ("args", `Object args) :: json

  let rec counters_to_json counters =
    match counters with
    | [] -> []
    | (name, count) :: counters ->
      (name, `Number (string_of_int count)) :: counters_to_json counters

  let add_counters ~counters json =
    match counters with
    | None -> json
    | Some counters -> ("counters", `Object (counters_to_json counters)) :: json

  let add_common_fields ~name ~counters ~args json =
    ([ "tid", `Number "0";
       "pid", `Number "0";
       "name", `String name;
       "cat", `String category ]
    |> add_counters ~counters |> add_args ~args)
    @ json

  let instant_fields ~args ~counters ~name ~time_in_nanoseconds () =
    ["ts", nanos_to_micros_as_json time_in_nanoseconds]
    |> add_common_fields ~name ~counters ~args
    |> add_counters ~counters |> add_args ~args

  let span_fields ~args ~counters ~name ~start_in_nanoseconds
      ~finish_in_nanoseconds () =
    [ "ts", nanos_to_micros_as_json start_in_nanoseconds;
      ( "dur",
        nanos_to_micros_as_json (start_in_nanoseconds - finish_in_nanoseconds) )
    ]
    |> add_common_fields ~name ~counters ~args
end

let write_instant ?args ?counters ~name ~time_in_nanoseconds () =
  `Object (Event.instant_fields ~args ~counters ~name ~time_in_nanoseconds ())
  |> Writer.write_event

let write_span ?args ?counters ~name ~start_in_nanoseconds
    ~finish_in_nanoseconds () =
  `Object
    (Event.span_fields ~args ~counters ~name ~start_in_nanoseconds
       ~finish_in_nanoseconds ())
  |> Writer.write_event
