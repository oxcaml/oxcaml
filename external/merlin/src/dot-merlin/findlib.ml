open Merlin_utils
open Std

let { Logger.log } = Logger.for_section "Findlib"

module Config = struct
  type t =
    { conf : string option; path : string list; toolchain : string option }

  let default = { conf = None; path = []; toolchain = None }
end

module Package = struct
  type t =
    { name : string;
      directory : string;
      ppx : string option;
      ppxopt : (string * string list) list
    }
end

let path_separator =
  match Sys.os_type with
  | "Cygwin" | "Win32" -> ";"
  | _ -> ":"

(** Extend the environment for ocamlfind to run in based on [config] *)
let environment ~(config : Config.t) =
  let overrides =
    List.filter_map
      [ ("OCAMLFIND_CONF", config.conf);
        ( "OCAMLPATH",
          match config.path with
          | [] -> None
          | path -> Some (String.concat ~sep:path_separator path) )
      ]
      ~f:(fun (name, value) -> Option.map value ~f:(fun value -> (name, value)))
  in
  let inherited =
    List.filter
      (Array.to_list (Unix.environment ()))
      ~f:(fun binding ->
        not
          (List.exists overrides ~f:(fun (name, _) ->
               String.is_prefixed ~by:(name ^ "=") binding)))
  in
  Array.of_list
    (List.map overrides ~f:(fun (name, value) -> name ^ "=" ^ value) @ inherited)

(** [run ~config args] runs ocamlfind with the given args and returns the
    stdout. Results are cached. *)
let run =
  let cache = Hashtbl.create 17 in
  fun ~(config : Config.t) args ->
    let key = (config, args) in
    match Hashtbl.find_opt cache key with
    | Some lines -> Ok lines
    | None -> (
      let args =
        match config.toolchain with
        | None -> args
        | Some toolchain -> "-toolchain" :: toolchain :: args
      in
      log ~title:"run" "ocamlfind %s" (String.concat ~sep:" " args);
      match
        Unix.open_process_args_full "ocamlfind"
          (Array.of_list ("ocamlfind" :: args))
          (environment ~config)
      with
      | exception Unix.Unix_error (error, _, _) ->
        Error (sprintf "Cannot run ocamlfind: %s" (Unix.error_message error))
      | stdout, stdin, stderr -> (
        close_out stdin;
        let output = In_channel.input_all stdout in
        let errors = In_channel.input_all stderr in
        match Unix.close_process_full (stdout, stdin, stderr) with
        | WEXITED 0 ->
          if String.is_non_empty errors then log ~title:"run" "%s" errors;
          let lines =
            List.filter (String.split_on_char ~sep:'\n' output) ~f:(fun line ->
                String.is_non_empty line)
          in
          Hashtbl.add cache key lines;
          Ok lines
        | _ -> Error (String.trim errors)))

let unexpected_output lines =
  Error
    (sprintf "Unexpected output from ocamlfind: %S"
       (String.concat ~sep:"\n" lines))

let package_directory ~config package =
  match run ~config [ "query"; "-format"; "%d"; package ] with
  | Ok [ directory ] -> Ok (Some directory)
  | Ok lines -> unexpected_output lines
  | Error message
    when String.equal message
           (sprintf "ocamlfind: Package `%s' not found" package) -> Ok None
  | Error _ as error -> error

let ocaml_stdlib ~config =
  match run ~config [ "printconf"; "stdlib" ] with
  | Ok [ stdlib ] -> Ok stdlib
  | Ok lines -> unexpected_output lines
  | Error _ as error -> error

(* The [ppxopt] property looks like ["ppx_a,-opt1,-opt2 ppx_b,-opt3"]: entries
   separated by spaces, each a ppx package name followed by its options. *)
let parse_ppxopt value =
  let split ~on s =
    List.filter (String.split_on_char ~sep:on s) ~f:(fun word ->
        String.is_non_empty word)
  in
  List.filter_map (split ~on:' ' value) ~f:(fun entry ->
      match split ~on:',' entry with
      | ppx :: options -> Some (ppx, options)
      | [] -> None)

(* Separates the fields of the [-format] we ask for. Tabs do not occur in
   package names, directories or (word-oriented) META properties. *)
let field_separator = '\t'

let query ~config packages =
  let open Result.Infix in
  match packages with
  | [] -> Ok []
  | _ :: _ ->
    let format =
      String.concat
        ~sep:(String.make 1 field_separator)
        [ "%p"; "%d"; "%(ppx)"; "%(ppxopt)" ]
    in
    let* lines =
      run ~config ("query" :: "-recursive" :: "-format" :: format :: packages)
    in
    Result.List.map lines ~f:(fun line ->
        match String.split_on_char ~sep:field_separator line with
        | [ name; directory; ppx; ppxopt ] ->
          Ok
            { Package.name;
              directory;
              ppx = (if String.is_non_empty ppx then Some ppx else None);
              ppxopt = parse_ppxopt ppxopt
            }
        | _ -> unexpected_output [ line ])

let resolve_path ~config ~base path =
  let open Result.Infix in
  if String.is_non_empty path then
    let package_directory package =
      let* directory = package_directory ~config package in
      match directory with
      | Some directory -> Ok directory
      | None ->
        Error (sprintf "Package `%s' referenced by %s not found" package path)
    in
    match path.[0] with
    | '^' | '+' ->
      let path = String.drop 1 path in
      let* stdlib = ocaml_stdlib ~config in
      Ok (Filename.concat stdlib path)
    | '@' ->
      let path = String.drop 1 path in
      begin match String.lsplit2 path ~on:'/' with
      | Some (package, rest_path) ->
        let* directory = package_directory package in
        Ok (Filename.concat directory rest_path)
      | None -> package_directory path
      end
    | _ ->
      Ok
        (if Filename.is_relative path && not (Filename.is_implicit path) then
           Filename.concat base path
         else path)
  else Ok path
