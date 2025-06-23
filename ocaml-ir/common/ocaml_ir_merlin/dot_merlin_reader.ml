open Misc
open Std
type file =
  {
  recurse: bool ;
  includes: string list ;
  path: string ;
  directives: Dot_protocol.Directive.Raw.t list }
module Cache =
  (File_cache.Make)(struct
                      type t = file
                      let read path =
                        let ic = open_in path in
                        let acc = ref [] in
                        let recurse = ref false in
                        let includes = ref [] in
                        let tell l = acc := (l :: (!acc)) in
                        try
                          let rec aux () =
                            let line = String.trim (input_line ic) in
                            if line = ""
                            then ()
                            else
                              if String.is_prefixed ~by:"B " line
                              then tell (`B (String.drop 2 line))
                              else
                                if String.is_prefixed ~by:"S " line
                                then tell (`S (String.drop 2 line))
                                else
                                  if String.is_prefixed ~by:"BH " line
                                  then tell (`BH (String.drop 3 line))
                                  else
                                    if String.is_prefixed ~by:"SH " line
                                    then tell (`SH (String.drop 3 line))
                                    else
                                      if String.is_prefixed ~by:"SRC " line
                                      then tell (`S (String.drop 4 line))
                                      else
                                        if String.is_prefixed ~by:"CMI " line
                                        then tell (`CMI (String.drop 4 line))
                                        else
                                          if
                                            String.is_prefixed ~by:"CMT "
                                              line
                                          then
                                            tell (`CMT (String.drop 4 line))
                                          else
                                            if
                                              String.is_prefixed
                                                ~by:"UNIT_NAME_FOR " line
                                            then
                                              tell
                                                (`UNIT_NAME_FOR
                                                   (String.drop 14 line))
                                            else
                                              if
                                                String.is_prefixed ~by:"PKG "
                                                  line
                                              then
                                                tell
                                                  (`PKG
                                                     (rev_split_words
                                                        (String.drop 4 line)))
                                              else
                                                if
                                                  String.is_prefixed
                                                    ~by:"EXT " line
                                                then
                                                  tell
                                                    (`EXT
                                                       (rev_split_words
                                                          (String.drop 4 line)))
                                                else
                                                  if
                                                    String.is_prefixed
                                                      ~by:"FLG " line
                                                  then
                                                    tell
                                                      (`FLG
                                                         (Shell.split_command
                                                            (String.drop 4
                                                               line)))
                                                  else
                                                    if
                                                      String.is_prefixed
                                                        ~by:"REC" line
                                                    then recurse := true
                                                    else
                                                      if
                                                        String.is_prefixed
                                                          ~by:". " line
                                                      then
                                                        includes :=
                                                          ((String.trim
                                                              (String.drop 2
                                                                 line))
                                                          :: (!includes))
                                                      else
                                                        if
                                                          String.is_prefixed
                                                            ~by:"STDLIB "
                                                            line
                                                        then
                                                          tell
                                                            (`STDLIB
                                                               (String.drop 7
                                                                  line))
                                                        else
                                                          if
                                                            String.is_prefixed
                                                              ~by:"SUFFIX "
                                                              line
                                                          then
                                                            tell
                                                              (`SUFFIX
                                                                 (String.drop
                                                                    7 line))
                                                          else
                                                            if
                                                              String.is_prefixed
                                                                ~by:"READER "
                                                                line
                                                            then
                                                              tell
                                                                (`READER
                                                                   (List.rev
                                                                    (rev_split_words
                                                                    (String.drop
                                                                    7 line))))
                                                            else
                                                              if
                                                                String.is_prefixed
                                                                  ~by:"EXCLUDE_QUERY_DIR"
                                                                  line
                                                              then
                                                                tell
                                                                  `EXCLUDE_QUERY_DIR
                                                              else
                                                                if
                                                                  String.is_prefixed
                                                                    ~by:"#"
                                                                    line
                                                                then ();
                            aux () in
                          aux ()
                        with
                        | End_of_file ->
                            (close_in_noerr ic;
                             (let recurse = !recurse
                              and includes = !includes in
                              {
                                recurse;
                                includes;
                                path;
                                directives = (List.rev (!acc))
                              }))
                        | exn -> (close_in_noerr ic; raise exn)
                      let cache_name = "Mconfig_dot"
                    end)
let find fname =
  if (Sys.file_exists fname) && (not (Sys.is_directory fname))
  then Some fname
  else
    (let rec loop dir =
       let fname = Filename.concat dir ".merlin" in
       if (Sys.file_exists fname) && (not (Sys.is_directory fname))
       then Some fname
       else
         (let parent = Filename.dirname dir in
          if parent <> dir then loop parent else None) in
     loop fname)
let directives_of_files filenames =
  let marked = Hashtbl.create 7 in
  let rec process acc =
    function
    | x::rest when Hashtbl.mem marked x -> process acc rest
    | x::rest ->
        (Hashtbl.add marked x ();
         (let file = Cache.read x in
          let dir = Filename.dirname file.path in
          let rest =
            (List.map ~f:(canonicalize_filename ~cwd:dir) file.includes) @
              rest in
          let rest =
            if file.recurse
            then
              let dir =
                if (Filename.basename file.path) <> ".merlin"
                then dir
                else Filename.dirname dir in
              (if dir <> file.path
               then
                 match find dir with
                 | Some fname -> fname :: rest
                 | None -> rest
               else rest)
            else rest in
          process (file :: acc) rest))
    | [] -> List.rev acc in
  process [] filenames
type config =
  {
  pass_forward: Dot_protocol.Directive.no_processing_required list ;
  to_canonicalize: (string * Dot_protocol.Directive.include_path) list ;
  stdlib: string option ;
  packages_to_load: string list }
let empty_config =
  {
    pass_forward = [];
    to_canonicalize = [];
    stdlib = None;
    packages_to_load = []
  }
let prepend_config ~cwd ~cfg =
  List.fold_left ~init:cfg
    ~f:(fun cfg (d : Dot_protocol.Directive.Raw.t) ->
          match d with
          | `B _ | `S _ | `BH _ | `SH _ | `CMI _ | `CMT _ as directive ->
              {
                cfg with
                to_canonicalize = ((cwd, directive) :: (cfg.to_canonicalize))
              }
          | `EXT _ | `SUFFIX _ | `FLG _ | `READER _ | `EXCLUDE_QUERY_DIR as
              directive ->
              { cfg with pass_forward = (directive :: (cfg.pass_forward)) }
          | `PKG ps ->
              { cfg with packages_to_load = (ps @ cfg.packages_to_load) }
          | `STDLIB path ->
              let canon_path = canonicalize_filename ~cwd path in
              { cfg with stdlib = (Some canon_path) }
          | `UNIT_NAME_FOR str ->
              {
                cfg with
                pass_forward = ((`UNIT_NAME_FOR str) :: (cfg.pass_forward))
              })
let process_one ~cfg { path; directives;_} =
  let cwd = Filename.dirname path in
  prepend_config ~cwd ~cfg (List.rev directives)
let expand =
  let filter path =
    let name = Filename.basename path in
    (name <> "") &&
      (((name.[0]) <> '.') && (try Sys.is_directory path with | _ -> false)) in
  fun ~stdlib _dir path ->
    let path = expand_directory stdlib path in expand_glob ~filter path []
let postprocess ~stdlib_override cfg =
  let stdlib =
    match stdlib_override with
    | stdlib when try Sys.is_directory stdlib with | _ -> false -> stdlib
    | _ -> Option.get cfg.stdlib in
  List.concat
    [List.concat_map cfg.to_canonicalize
       ~f:(fun (dir, directive) ->
             let dirs =
               match directive with
               | `B path ->
                   List.map (expand ~stdlib dir path) ~f:(fun p -> `B p)
               | `S path ->
                   List.map (expand ~stdlib dir path) ~f:(fun p -> `S p)
               | `BH path ->
                   List.map (expand ~stdlib dir path) ~f:(fun p -> `BH p)
               | `SH path ->
                   List.map (expand ~stdlib dir path) ~f:(fun p -> `SH p)
               | `CMI path ->
                   List.map (expand ~stdlib dir path) ~f:(fun p -> `CMI p)
               | `CMT path ->
                   List.map (expand ~stdlib dir path) ~f:(fun p -> `CMT p) in
             (dirs :> Dot_protocol.directive list));
    (cfg.pass_forward :> Dot_protocol.directive list)]
let load ~dot_merlin_file ~stdlib_override =
  let directives = directives_of_files [dot_merlin_file] in
  let cfg =
    List.fold_left directives ~init:empty_config
      ~f:(fun cfg file -> process_one ~cfg file) in
  let directives = postprocess ~stdlib_override cfg in directives
let dot_merlin_file_in_dir dir = Filename.concat dir ".merlin"
