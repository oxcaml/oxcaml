module Private__for_use_by_dune_only = struct
  let trace_dir_env_var = "DUNE_ACTION_TRACE_DIR"

  let trace_dir =
    let dir = lazy (Sys.getenv_opt trace_dir_env_var) in
    fun () -> Lazy.force dir
end

open Private__for_use_by_dune_only

let enabled () = Option.is_some (trace_dir ())

module Event = struct
  module Json = Json

  type fields = (string * Json.t) list

  type t =
    | Instant of
        { name : string
        ; category : string
        ; time : int
        ; args : fields option
        }
    | Span of
        { name : string
        ; category : string
        ; start : int
        ; duration : int
        ; args : fields option
        }

  let instant ?args ~category ~name ~time_in_nanoseconds:time () =
    Instant { name; category; time; args }

  let span
      ?args
      ~category
      ~name
      ~start_in_nanoseconds:start
      ~finish_in_nanoseconds:finish
      () =
    let duration = finish - start in
    Span { name; category; start; duration; args }

  let microseconds_string_preserve_precision ~ns =
    (* Converting nano-seconds into a float loses precision on big floats. The Chrome
       format only cares about microsecond precision. If we were to ever want to support
       nano-second precision, this function would need to change. *)
    string_of_int (ns / 1_000)

  let add_args args (json : fields) =
    match args with
    | None -> json
    | Some args -> ("args", `Object args) :: json

  let to_chan t chan =
    let json =
      match t with
      | Instant { name; category; time; args } ->
          add_args
            args
            [ "ph", `String "i"
            ; "tid", `Number "0"
            ; "pid", `Number "0"
            ; "name", `String name
            ; "cat", `String category
            ; "ts", `Number (microseconds_string_preserve_precision ~ns:time)
            ]
      | Span { name; category; start; duration; args } ->
          add_args
            args
            [ "ph", `String "X"
            ; "tid", `Number "0"
            ; "pid", `Number "0"
            ; "name", `String name
            ; "cat", `String category
            ; "ts", `Number (microseconds_string_preserve_precision ~ns:start)
            ; "dur", `Number (microseconds_string_preserve_precision ~ns:duration)
            ]
    in
    (Json.write (`Object json) chan [@nontail])
end

let make_trace_dir =
  let make =
    lazy
      (match trace_dir () with
      | None -> None
      | Some dir as some -> (
          match Sys.mkdir dir 0o777 with
          | () -> some
          | exception Sys_error _ when Sys.is_directory dir ->
              (* We hit this branch when an action produces multiple
               trace events, and by the time we force this lazy cell, we have already
               created the trace directory (e.g., by running a different executable that
               links in this library). We pay an extra system call ([Sys.is_directory]) in
               this case right now. Can we do better? One option is to examine the payload
               of [Sys_error], e.g., check that it contains the string "File exists", but
               that seems pretty fragile. *)
              some
          | exception _ -> None))
  in
  fun () -> Lazy.force make

module Context = struct
  type state =
    | Open of
        { chan : out_channel
        ; mutable no_events_yet : bool
        }
    | Closed
    | Disabled

  type t = state ref

  let create_state ~name =
    match make_trace_dir () with
    | None -> Disabled
    | Some trace_dir ->
        let _, chan = Filename.open_temp_file ~temp_dir:trace_dir name ".json" in
        Open { chan; no_events_yet = true }

  let create ~name = ref (create_state ~name)

  let emit (t : t) event =
    match !t with
    | Disabled -> ()
    | Closed -> failwith "Dune action tracing context has already been closed"
    | Open t ->
        let prefix =
          if t.no_events_yet
          then (
            t.no_events_yet <- false;
            '[')
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

module For_testing = struct
  module Json = Json

  let microseconds_string_preserve_precision =
    Event.microseconds_string_preserve_precision
end
