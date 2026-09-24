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

(* Adapted from Jane Street's build_action_trace kernel, without local modes
   so that it can be built by the bootstrap compiler. *)
module Json = struct
  type t =
    [ `Null
    | `False
    | `True
    | `String of string
    | `Number of string
    | `Object of (string * t) list
    | `Array of t list
    ]

  let copy_substring chan s ~start ~limit =
    if limit > start then output_substring chan s start (limit - start)

  let rec output_escaped_chars chan s ~n ~start ~pos =
    if pos < n then
      match String.get s pos with
      | '\b' -> escape chan s ~n ~start ~pos ~e:"\\b"
      | '\t' -> escape chan s ~n ~start ~pos ~e:"\\t"
      | '\n' -> escape chan s ~n ~start ~pos ~e:"\\n"
      | '\012' -> escape chan s ~n ~start ~pos ~e:"\\f"
      | '\r' -> escape chan s ~n ~start ~pos ~e:"\\r"
      | '\\' -> escape chan s ~n ~start ~pos ~e:"\\\\"
      | '"' -> escape chan s ~n ~start ~pos ~e:"\\\""
      | '\000' .. '\031' as c ->
        escape chan s ~n ~start ~pos
          ~e:(Printf.sprintf "\\u%04x" (Char.code c))
      | '\032' .. '\127' ->
        output_escaped_chars chan s ~n ~start ~pos:(pos + 1)
      | _ ->
        let decoded = String.get_utf_8_uchar s pos in
        if Uchar.utf_decode_is_valid decoded then
          output_escaped_chars chan s ~n ~start
            ~pos:(pos + Uchar.utf_decode_length decoded)
        else
          (* Replace each invalid byte with the UTF-8 replacement character. *)
          escape chan s ~n ~start ~pos ~e:"\xef\xbf\xbd"
    else copy_substring chan s ~start ~limit:pos

  and escape chan s ~n ~start ~pos ~e =
    copy_substring chan s ~start ~limit:pos;
    output_string chan e;
    output_escaped_chars chan s ~n ~start:(pos + 1) ~pos:(pos + 1)

  let output_quoted_string chan s =
    output_char chan '"';
    output_escaped_chars chan s ~n:(String.length s) ~start:0 ~pos:0;
    output_char chan '"'

  let rec output chan (t : t) =
    match t with
    | `Null -> output_string chan "null"
    | `String s -> output_quoted_string chan s
    | `Number f -> output_string chan f
    | `True -> output_string chan "true"
    | `False -> output_string chan "false"
    | `Array l ->
      output_char chan '[';
      output_array_body chan l;
      output_char chan ']'
    | `Object o ->
      output_char chan '{';
      output_object_body chan o;
      output_char chan '}'

  and output_array_body chan = function
    | [] -> ()
    | [x] -> output chan x
    | x :: xs ->
      output chan x;
      output_char chan ',';
      output_array_body chan xs

  and output_object_body chan = function
    | [] -> ()
    | [(x, y)] ->
      output_quoted_string chan x;
      output_char chan ':';
      output chan y
    | (x, y) :: xs ->
      output_quoted_string chan x;
      output_char chan ':';
      output chan y;
      output_char chan ',';
      output_object_body chan xs

  let write t chan = output chan t
end

let trace_dir = lazy (Sys.getenv_opt "DUNE_ACTION_TRACE_DIR")
let enabled () = Option.is_some (Lazy.force trace_dir)

module Event = struct
  type fields = (string * Json.t) list

  type t =
    | Instant of
        { name : string;
          category : string;
          time : int;
          args : fields option }
    | Span of
        { name : string;
          category : string;
          start : int;
          duration : int;
          args : fields option }

  let instant ?args ~category ~name ~time_in_nanoseconds:time () =
    Instant { name; category; time; args }

  let span ?args ~category ~name ~start_in_nanoseconds:start
      ~finish_in_nanoseconds:finish () =
    Span { name; category; start; duration = finish - start; args }

  (* Divide before converting to a string to preserve epoch microseconds. *)
  let microseconds ns = `Number (string_of_int (ns / 1_000))

  let to_chan t chan =
    let args, fields =
      match t with
      | Instant { name; category; time; args } ->
        args,
        ["tid", `Number "0"; "pid", `Number "0";
         "name", `String name; "cat", `String category;
         "ts", microseconds time]
      | Span { name; category; start; duration; args } ->
        args,
        ["tid", `Number "0"; "pid", `Number "0";
         "name", `String name; "cat", `String category;
         "ts", microseconds start; "dur", microseconds duration]
    in
    let fields =
      match args with
      | None -> fields
      | Some args -> ("args", `Object args) :: fields
    in
    Json.write (`Object fields) chan
end

let make_trace_dir = lazy (
  match Lazy.force trace_dir with
  | None -> None
  | Some dir as some ->
    match Sys.mkdir dir 0o777 with
    | () -> some
    | exception Sys_error _ when Sys.is_directory dir -> some
    | exception _ -> None)

module Context = struct
  type state =
    | Open of { chan : out_channel; mutable no_events_yet : bool }
    | Closed
    | Disabled

  type t = state ref

  let create ~name =
    ref (match Lazy.force make_trace_dir with
      | None -> Disabled
      | Some trace_dir ->
        let _, chan =
          Filename.open_temp_file ~temp_dir:trace_dir name ".json" in
        Open { chan; no_events_yet = true })

  let emit t event =
    match !t with
    | Disabled -> ()
    | Closed -> failwith "Dune action tracing context has already been closed"
    | Open t ->
      let prefix =
        if t.no_events_yet then begin
          t.no_events_yet <- false;
          '['
        end else ','
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
