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

val init_path :
  ?auto_include:Load_path.auto_include_callback -> ?dir:string -> unit -> unit
val init_parameters : unit -> unit

(** [Load_path.find] for a file that a link, an archive or a pack is made
    from, and for the files that the compiler adds to a link.  The name is
    looked for exactly as [Load_path.find] does it; the file found, like
    one named by its path, is then taken from the library tree of the
    selected codegen variant if it is inside the default library root
    ([Codegen_variant.in_selected_root]).  Only a file that this invocation
    wrote in a directory of that root that the variant's tree stands for is
    not searched for: it is that file
    ([Codegen_variant.is_product_in_replaced_directory]).  Under the default
    variant this is [Load_path.find].  Raises [Not_found]. *)
val find_input : string -> string

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
