(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Local_store

module Dir : sig
  type entry = {
    basename : string;
    path : string
  }

  type t

  val path : t -> string
  val files : t -> entry list
  val basenames : t -> string list
  val hidden : t -> bool

  val create : hidden:bool -> string -> t

  val find : t -> string -> string option
  val find_normalized : t -> string -> string option
end = struct
  type entry = {
    basename : string;
    path : string
  }

  type t = {
    path : string;
    files : entry list;
    hidden : bool
  }

  let path t = t.path
  let files t = t.files
  let basenames t = List.map (fun { basename; _ } -> basename) t.files
  let hidden t = t.hidden

  let find t fn =
    List.find_map (fun { basename; path } ->
      if String.equal basename fn then
        Some path
      else
        None) t.files

  let find_normalized t fn =
    let fn = Misc.normalized_unit_filename fn in
    let search { basename; path } =
      if Misc.normalized_unit_filename basename = fn then
        Some path
      else
        None
    in
    List.find_map search t.files

  (* For backward compatibility reason, simulate the behavior of
     [Misc.find_in_path]: silently ignore directories that don't exist
     + treat [""] as the current directory. *)
  let readdir_compat dir =
    try
      Sys.readdir (if dir = "" then Filename.current_dir_name else dir)
    with Sys_error _ ->
      [||]

  let create ~hidden path =
    let files = Array.to_list (readdir_compat path)
      |> List.map (fun basename -> { basename; path = Filename.concat path basename }) in
    { path; files; hidden }
end

type visibility = Visible | Hidden

(** Stores cached paths to files *)
module Path_cache : sig
  (* Clear cache *)
  val reset : unit -> unit

  (* Same as [add] below, but will replace existing entries.

     [prepend_add] is faster than [add] and intended for use in [init] and [remove_dir]:
     since we are starting from an empty cache, we can avoid checking whether a unit name
     already exists in the cache simply by adding entries in reverse order. *)
  val prepend_add : Dir.t -> unit

  (* Add path to cache. If path with same basename is already in cache, skip adding. *)
  val add : Dir.t -> unit

  (* Like [prepent_add], but only adds a single file to the cache. *)
  val prepend_add_single : hidden:bool -> string -> string -> unit

  (* Search for a basename in cache. Ignore case if [uncap] is true *)
  val find : uncap:bool -> string -> string * visibility
end = struct
  module STbl = Misc.Stdlib.String.Tbl

  (* Mapping from basenames to full filenames *)
  type registry = string STbl.t

  let visible_files : registry ref = s_table STbl.create 42
  let visible_files_uncap : registry ref = s_table STbl.create 42

  let hidden_files : registry ref = s_table STbl.create 42
  let hidden_files_uncap : registry ref = s_table STbl.create 42

  let reset () =
    STbl.clear !hidden_files;
    STbl.clear !hidden_files_uncap;
    STbl.clear !visible_files;
    STbl.clear !visible_files_uncap

  let prepend_add_single ~hidden basename path =
    if hidden then begin
      STbl.replace !hidden_files basename path;
      STbl.replace !hidden_files_uncap (Misc.normalized_unit_filename basename) path
    end else begin
      STbl.replace !visible_files basename path;
      STbl.replace !visible_files_uncap (String.uncapitalize_ascii basename) path
    end

  let prepend_add dir =
    List.iter (fun ({ basename; path } : Dir.entry) ->
      prepend_add_single ~hidden:(Dir.hidden dir) basename path
    ) (Dir.files dir)

  let add dir =
    let update base fn visible_files hidden_files =
      if (Dir.hidden dir) && not (STbl.mem !hidden_files base) then
        STbl.replace !hidden_files base fn
      else if not (STbl.mem !visible_files base) then
        STbl.replace !visible_files base fn
    in
    List.iter
      (fun ({ basename = base; path = fn }: Dir.entry) ->
         update base fn visible_files hidden_files;
         let ubase = Misc.normalized_unit_filename base in
         update ubase fn visible_files_uncap hidden_files_uncap)
      (Dir.files dir)

  let find fn visible_files hidden_files =
    try (STbl.find !visible_files fn, Visible) with
    | Not_found -> (STbl.find !hidden_files fn, Hidden)

  let find ~uncap fn =
    if uncap then
      find (String.uncapitalize_ascii fn) visible_files_uncap hidden_files_uncap
    else
      find fn visible_files hidden_files
end

type auto_include_callback =
  (Dir.t -> string -> string option) -> string -> string

let visible_dirs = s_ref []
let visible_basenames = s_ref []
let hidden_dirs = s_ref []
let no_auto_include _ _ = raise Not_found
let auto_include_callback = ref no_auto_include

let reset () =
  assert (not Config.merlin || Local_store.is_bound ());
  Path_cache.reset ();
  hidden_dirs := [];
  visible_dirs := [];
  visible_basenames := [];
  auto_include_callback := no_auto_include


type dirs_and_files =
  { dirs : Dir.t list
  ; basenames : string list
  }

let get_visible () =
  { dirs = List.rev !visible_dirs
  ; basenames = !visible_basenames
  }

let get_path_list () =
  Misc.rev_map_end Dir.path !visible_dirs (List.rev_map Dir.path !hidden_dirs)

type paths =
  { visible : string list;
    hidden : string list }

let get_paths () =
  { visible = List.rev_map Dir.path !visible_dirs;
    hidden = List.rev_map Dir.path !hidden_dirs }

let get_visible_path_list () = List.rev_map Dir.path !visible_dirs
let get_hidden_path_list () = List.rev_map Dir.path !hidden_dirs

module Manifest_reader : sig
  type t

  val create : unit -> t

  val iter_manifest :
    t -> f:(filename:string -> location:[ `Cwd_relative of string ] -> unit) -> [`Root_relative of string] -> unit
end = struct
  type t = {
    visited: Misc.Stdlib.String.Set.t ref
  }

  let create () = {
    visited = ref Misc.Stdlib.String.Set.empty
  }

  (* CR aodintsov: Probably need a better name. *)
  let root = lazy (Sys.getenv "OXCAML_LOAD_PATH_ROOT")

  let path_from_root (`Root_relative path) =
    let root = Lazy.force root in
    `Cwd_relative (Filename.concat root path)

  let visit { visited } ~f (`Root_relative path) =
    if Misc.Stdlib.String.Set.mem path !visited then
      ()
    else (
      visited := Misc.Stdlib.String.Set.add path !visited;
      f (path_from_root (`Root_relative path)))

  let iter_lines ~f (`Cwd_relative path) =
    let ic = open_in path in
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
          with End_of_file -> ()
        in
        loop ())
      ~always:(fun () -> close_in ic)

  let parse_line line =
    (* CR aodintsov: Better parsing/escaping. *)
    match String.split_on_char ' ' line with
    | "file" :: filename :: location :: [] ->
      let location = `Root_relative location in
      `File (filename, location)
    | "manifest" :: _ :: location :: [] ->
      let location = `Root_relative location in
      `Manifest location
    | _ ->
      (* CR aodintsov: Fix errors. *)
      raise Not_found

  let rec iter_manifest t ~f path =
    visit t path ~f:(fun path ->
      iter_lines path ~f:(fun line ->
        match parse_line line with
        | `File (filename, location) ->
          visit t location ~f:(fun location -> f ~filename ~location)
        | `Manifest location ->
          iter_manifest t ~f location))
end

let init_manifests () =
  let manifest_reader = Manifest_reader.create () in
  List.iter (fun manifest ->
    let manifest = `Root_relative manifest in
    Manifest_reader.iter_manifest manifest_reader manifest ~f:(fun ~filename ~location:(`Cwd_relative location) ->
      let basename = Filename.basename filename in
      visible_basenames := basename :: !visible_basenames;
      Path_cache.prepend_add_single ~hidden:false basename location
    )) !Clflags.include_manifests;

  List.iter (fun manifest ->
    let manifest = `Root_relative manifest in
    Manifest_reader.iter_manifest manifest_reader manifest ~f:(fun ~filename ~location:(`Cwd_relative location) ->
      let basename = Filename.basename filename in
      Path_cache.prepend_add_single ~hidden:true basename location
    )) !Clflags.hidden_include_manifests


let init  ~auto_include ~visible ~hidden =
  reset ();
  visible_dirs := List.rev_map (Dir.create ~hidden:false) visible;
  hidden_dirs := List.rev_map (Dir.create ~hidden:true) hidden;
  List.iter Path_cache.prepend_add !hidden_dirs;
  List.iter Path_cache.prepend_add !visible_dirs;
  init_manifests ();
  auto_include_callback := auto_include

let remove_dir dir =
  assert (not Config.merlin || Local_store.is_bound ());
  let visible = List.filter (fun d -> Dir.path d <> dir) !visible_dirs in
  let hidden = List.filter (fun d -> Dir.path d <> dir) !hidden_dirs in
  if    List.compare_lengths visible !visible_dirs <> 0
     || List.compare_lengths hidden !hidden_dirs <> 0 then begin
    reset ();
    visible_dirs := visible;
    hidden_dirs := hidden;
    List.iter Path_cache.prepend_add hidden;
    List.iter Path_cache.prepend_add visible
  end

(* General purpose version of function to add a new entry to load path: We only
   add a basename to the cache if it is not already present, in order to enforce
   left-to-right precedence. *)
let add (dir : Dir.t) =
  assert (not Config.merlin || Local_store.is_bound ());
  Path_cache.add dir;
  if (Dir.hidden dir) then
    hidden_dirs := dir :: !hidden_dirs
  else
    visible_dirs := dir :: !visible_dirs

let append_dir = add

let add_dir ~hidden dir = add (Dir.create ~hidden dir)

(* Add the directory at the start of load path - so basenames are
   unconditionally added. *)
let prepend_dir (dir : Dir.t) =
  assert (not Config.merlin || Local_store.is_bound ());
  Path_cache.prepend_add dir;
  if (Dir.hidden dir) then
    hidden_dirs := !hidden_dirs @ [dir]
  else
    visible_dirs := !visible_dirs @ [dir]

let is_basename fn = Filename.basename fn = fn

let auto_include_libs libs alert find_in_dir fn =
  let scan (lib, lazy dir) =
    let file = find_in_dir dir fn in
    let alert_and_add_dir _ =
      alert lib;
      append_dir dir
    in
    Option.iter alert_and_add_dir file;
    file
  in
  match List.find_map scan libs with
  | Some base -> base
  | None -> raise Not_found

let auto_include_otherlibs =
  (* Ensure directories are only ever scanned once *)
  let expand = Misc.expand_directory Config.standard_library in
  let otherlibs =
    let read_lib lib = lazy (Dir.create ~hidden:false (expand ("+" ^ lib))) in
    List.map (fun lib -> (lib, read_lib lib)) ["dynlink"; "str"; "unix"] in
  auto_include_libs otherlibs

let find fn =
  assert (not Config.merlin || Local_store.is_bound ());
  try
    if is_basename fn && not !Sys.interactive then
      fst (Path_cache.find ~uncap:false fn)
    else
      Misc.find_in_path (get_path_list ()) fn
  with Not_found ->
    !auto_include_callback Dir.find fn

let find_normalized_with_visibility fn =
  assert (not Config.merlin || Local_store.is_bound ());
  try
    if is_basename fn && not !Sys.interactive then
      Path_cache.find ~uncap:true fn
    else
      try
        (Misc.find_in_path_normalized (get_visible_path_list ()) fn, Visible)
      with
      | Not_found ->
        (Misc.find_in_path_normalized (get_hidden_path_list ()) fn, Hidden)
  with Not_found ->
    let fn_uncap = String.uncapitalize_ascii fn in
    (!auto_include_callback Dir.find_normalized fn_uncap, Visible)

let find_normalized fn = fst (find_normalized_with_visibility fn)
