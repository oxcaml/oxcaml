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

(** Management of include directories.

    This module offers a high level interface to locating files in the load
    path, which is constructed from [-I] and [-H] command line flags and a few
    other parameters.

    It makes the assumption that the contents of include directories
    doesn't change during the execution of the compiler.
*)

(* CR aodintsov/mshinwell: merge the remaining oxcaml changes
   upstream *)

(** Add a directory to the end of the load path (i.e. at lowest priority.) *)
val add_dir : hidden:bool -> string -> unit

(** Remove a directory from the load path *)
val remove_dir : string -> unit

(** Remove all directories *)
val reset : unit -> unit

module Dir : sig
  (** Represent one directory in the load path. *)
  type t

  val create : hidden:bool -> string -> t

  (** All the files in that directory. This doesn't include files in
      sub-directories of this directory. *)
  val basenames : t -> string list
end

(** The type of callback functions on for [init ~auto_include] *)
type auto_include_callback =
  (Dir.t -> string -> string option) -> string -> string

(** No automatic directory inclusion: misses in the load path raise [Not_found]
    as normal. *)
val no_auto_include : auto_include_callback

(** [init ~visible ~hidden] is the same as
    [reset ();
     List.iter add_dir (List.rev hidden);
     List.iter add_dir (List.rev visible)] *)
val init :
  auto_include:auto_include_callback ->
  visible:string list ->
  hidden:string list ->
  unit

(** [auto_include_otherlibs alert] is a callback function to be passed to
    {!Load_path.init} and automatically adds [-I +lib] to the load path after
    calling [alert lib]. *)
val auto_include_otherlibs : (string -> unit) -> auto_include_callback

(** Return the list of directories passed to [add_dir] so far. *)
val get_path_list : unit -> string list

type paths =
  { visible : string list;
    hidden : string list
  }

(** Return the directories passed to [add_dir] so far. *)
val get_paths : unit -> paths

(** Locate a file in the load path. Raise [Not_found] if the file
    cannot be found. This function is optimized for the case where the
    filename is a basename, i.e. doesn't contain a directory
    separator. *)
val find : string -> string

(** Same as [find], but search also for normalized unit name (see
    {!Misc.normalized_unit_filename}), i.e. if name is [Foo.ml], allow
    [/path/Foo.ml] and [/path/foo.ml] to match. *)
val find_normalized : string -> string

type visibility =
  | Visible
  | Hidden

(** Same as [find_normalized], but also reports whether the cmi was found in a
    -I directory (Visible) or a -H directory (Hidden) *)
val find_normalized_with_visibility : string -> string * visibility

(** Old name for {!append_dir} *)
val add : Dir.t -> unit [@@deprecated]

(** [append_dir d] adds [d] to the end of the load path (i.e. at lowest
    priority. *)
val append_dir : Dir.t -> unit

(** [prepend_dir d] adds [d] to the start of the load path (i.e. at highest
    priority. *)
val prepend_dir : Dir.t -> unit

(** Same as [get_paths ()], except that it returns a [Dir.t list], and doesn't
    include the -H paths. *)
val get_visible : unit -> Dir.t list
