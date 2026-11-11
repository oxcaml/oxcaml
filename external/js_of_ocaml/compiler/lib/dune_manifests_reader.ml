open! Stdlib

(* A copy of [Dune_manifests_reader] in the OxCaml compiler's
   [utils/load_path.ml], which is not exported. Keep the format handling in
   sync with it. *)

let manifest_files_root =
  lazy
    (let var_name = "MANIFEST_FILES_ROOT" in
     match Sys.getenv_opt var_name with
     | None -> failwith (var_name ^ " not set")
     | Some path -> path)

let resolve_against_root path = Filename.concat (Lazy.force manifest_files_root) path

(* Splits a line into tokens separated by spaces. Inside tokens, the escape sequences
   '\\', '\ ', '\n', '\r' and '\t' are the ones the writer can produce. *)
let split_and_unescape line ~buffer =
  let len = String.length line in
  let end_token tokens =
    let token = Buffer.contents buffer in
    Buffer.clear buffer;
    if String.is_empty token then tokens else token :: tokens
  in
  let rec loop i tokens =
    if i >= len
    then List.rev (end_token tokens)
    else
      match String.unsafe_get line i with
      | '\\' when i + 1 < len ->
          let unescaped =
            match String.unsafe_get line (i + 1) with
            | '\\' -> '\\'
            | ' ' -> ' '
            | 'n' -> '\n'
            | 'r' -> '\r'
            | 't' -> '\t'
            | c -> failwith (Printf.sprintf "Invalid escape sequence in manifest: \\%c" c)
          in
          Buffer.add_char buffer unescaped;
          loop (i + 2) tokens
      | '\\' -> failwith "Trailing backslash in manifest"
      | ' ' -> loop (i + 1) (end_token tokens)
      | c ->
          Buffer.add_char buffer c;
          loop (i + 1) tokens
  in
  loop 0 []

type entry =
  | File of
      { filename : string
      ; location : string
      }
  | Manifest of string

let parse_line line ~buffer =
  match split_and_unescape line ~buffer with
  (* [file_x] additionally carries a bit only meaningful to the OCaml compiler. *)
  | [ ("file" | "file_x"); filename; location ] -> Some (File { filename; location })
  (* The second component of a [manifest] entry is only there for human readability. *)
  | [ "manifest"; _; location ] -> Some (Manifest location)
  | [] -> None
  | _ -> failwith ("Cannot parse manifest file line: " ^ line)

(* Maps bare file names to their locations, resolved against [MANIFEST_FILES_ROOT]. *)
let files : string String.Hashtbl.t option ref = ref None

let read manifests =
  let files = String.Hashtbl.create 64 in
  (* Locations already read, shared between manifests and files, so that manifest DAGs
     are read once. *)
  let visited = String.Hashtbl.create 64 in
  let visit location ~f =
    if not (String.Hashtbl.mem visited location)
    then (
      String.Hashtbl.add visited location ();
      f (resolve_against_root location))
  in
  let buffer = Buffer.create 64 in
  let rec read_manifest location =
    visit location ~f:(fun path ->
        List.iter (file_lines_bin path) ~f:(fun line ->
            match parse_line (String.trim line) ~buffer with
            | None -> ()
            | Some (File { filename; location }) ->
                (* As in the compiler, later entries take precedence. *)
                visit location ~f:(fun path ->
                    String.Hashtbl.replace files (Filename.basename filename) path)
            | Some (Manifest location) -> read_manifest location))
  in
  List.iter manifests ~f:read_manifest;
  files

let set = function
  | [] -> ()
  | manifests -> files := Some (read manifests)

let resolve name =
  match !files with
  | Some files when String.equal (Filename.basename name) name -> (
      match String.Hashtbl.find_opt files name with
      | Some path -> path
      | None -> name)
  | Some _ | None -> name
