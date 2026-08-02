(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*       Fabrice Le Fessant, EPI Gallium, INRIA Paris-Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

val init_backtrace_recording : unit -> unit
(** Turn exception-backtrace recording off for the compiler, overriding any
    ambient [OCAMLRUNPARAM] ["b"] setting, unless the [OXCAML_BACKTRACES]
    environment variable is set to something other than [""], ["0"], ["false"],
    ["no"] or ["off"] (compared case-insensitively), in which case turn it on.
    Recording state is per-domain; the compiler is single-domain. Call once at
    driver startup, before any compilation work. *)

val init_path :
  ?auto_include:Load_path.auto_include_callback -> ?dir:string -> unit -> unit
val init_parameters : unit -> unit
val initial_env : unit -> Env.t

val with_ppf_file :
  file_prefix:string -> file_extension:string -> (Format.formatter -> 'a) -> 'a

val with_ppf_dump : ?stdout:unit ->
  file_prefix:string -> (Format.formatter -> 'a) -> 'a

val get_profile_file_prefix :
  expected_suffix:string -> default_name:string -> string

val auto_include : Load_path.auto_include_callback
(** [auto_include find_in_dir fn] is a callback function to be passed to
    {!Load_path.init} and automatically adds [-I +lib] to the load path after
    displaying an alert. *)
