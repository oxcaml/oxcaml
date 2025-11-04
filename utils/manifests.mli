(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Jane Street Group, LLC                         *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Reader : sig
  module Path : sig
    type load_root_relative
    type cwd_relative
    type 'a t

    val of_string : string -> load_root_relative t
    val to_string : cwd_relative t -> string
  end

  type t

  val create : unit -> t

  val iter_manifest
    :  t
    -> f:(filename:string -> location:Path.cwd_relative Path.t -> unit)
    -> manifest_path:Path.load_root_relative Path.t
    -> unit
end
