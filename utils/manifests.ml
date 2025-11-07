(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*              Andrei Odintsov, Jane Street UK Partnership LLP           *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Reader = struct
  module Path : sig
    type load_root_relative
    type cwd_relative
    type 'a t

    val of_string : string -> load_root_relative t
    val to_string : cwd_relative t -> string
    val make_cwd_relative : load_root_relative t -> cwd_relative t

    module Hash_set : sig
      type path = load_root_relative t
      type t

      val create : unit -> t
      val mem : t -> path -> bool
      val add : t -> path -> unit
    end
  end = struct
    type load_root_relative = [ `Load_root_relative ]
    type cwd_relative = [ `Cwd_relative ]

    type _ t =
      | Cwd_relative : string -> cwd_relative t
      | Load_root_relative : string -> load_root_relative t

    let of_string path = Load_root_relative path
    let to_string (Cwd_relative path) = path
    let root = lazy (Sys.getenv "OXCAML_MANIFEST_LOAD_PATH_ROOT")

    let make_cwd_relative (Load_root_relative path) =
      let root = Lazy.force root in
      Cwd_relative (Filename.concat root path)
    ;;

    module Hash_set = struct
      type path = load_root_relative t
      type t = unit Misc.Stdlib.String.Tbl.t

      let create () = Misc.Stdlib.String.Tbl.create 42
      let mem t (Load_root_relative path) = Misc.Stdlib.String.Tbl.mem t path
      let add t (Load_root_relative path) = Misc.Stdlib.String.Tbl.add t path ()
    end
  end

  type t = { visited : Path.Hash_set.t }

  let create () = { visited = Path.Hash_set.create () }

  let visit { visited } ~f path =
    if Path.Hash_set.mem visited path
    then ()
    else (
      Path.Hash_set.add visited path;
      f (Path.make_cwd_relative path))
  ;;

  let iter_lines ~f path =
    let ic = open_in (Path.to_string path) in
    Misc.try_finally
      (fun () ->
        let rec loop () =
          try
            let line = String.trim (input_line ic) in
            match line with
            | "" -> loop ()
            | line ->
              f line;
              loop ()
          with
          | End_of_file -> ()
        in
        loop ())
      ~always:(fun () -> close_in ic)
  ;;

  exception Parse_error of string

  let split_and_unescape ~buffer line =
    let len = String.length line in
    let rec loop i current_token tokens =
      if i >= len
      then (
        let token = Buffer.contents current_token in
        let tokens = if token = "" then tokens else token :: tokens in
        Buffer.clear current_token;
        List.rev tokens)
      else (
        match String.unsafe_get line i with
        | '\\' when i + 1 < len ->
          (match String.unsafe_get line (i + 1) with
           | '\\' ->
             Buffer.add_char current_token '\\';
             loop (i + 2) current_token tokens
           | ' ' ->
             Buffer.add_char current_token ' ';
             loop (i + 2) current_token tokens
           | 'n' ->
             Buffer.add_char current_token '\n';
             loop (i + 2) current_token tokens
           | 'r' ->
             Buffer.add_char current_token '\r';
             loop (i + 2) current_token tokens
           | c -> raise (Parse_error (Printf.sprintf "Invalid escape sequence: \\%c" c)))
        | '\\' -> raise (Parse_error "Trailing backslash")
        | ' ' ->
          let token = Buffer.contents current_token in
          let tokens = if token = "" then tokens else token :: tokens in
          Buffer.clear current_token;
          loop (i + 1) current_token tokens
        | c ->
          Buffer.add_char current_token c;
          loop (i + 1) current_token tokens)
    in
    loop 0 buffer []
  ;;

  let parse_line ~buffer line =
    match split_and_unescape ~buffer line with
    | [ "file"; filename; location ] ->
      let location = Path.of_string location in
      `File (filename, location)
    | [ "manifest"; _; location ] ->
      let location = Path.of_string location in
      `Manifest location
    | _ -> raise (Parse_error ("Cannot parse manifest file line: " ^ line))
  ;;

  let rec iter_manifest t ~f ~manifest_path =
    let buffer = Buffer.create 16 in
    visit t manifest_path ~f:(fun manifest_path ->
      iter_lines manifest_path ~f:(fun line ->
        match parse_line ~buffer line with
        | `File (filename, location) ->
          visit t location ~f:(fun location -> f ~filename ~location)
        | `Manifest manifest_path -> iter_manifest t ~f ~manifest_path))
  ;;
end

module For_testing = struct
  exception Parse_error = Reader.Parse_error

  let split_and_unescape = Reader.split_and_unescape
end
