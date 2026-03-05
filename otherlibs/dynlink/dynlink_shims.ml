(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   David Allsopp, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Config = Dynlink_config

(* Types for Cmo_format.compilation_unit_descr.cu_format and
   Cmo_format.compilation_unit_descr.cu_arg_descr (not used by Dynlink) *)
module Lambda : sig
  type arg_descr
  type main_module_block_format
end = struct
  type arg_descr = unit
  type main_module_block_format = unit
end

module Name = struct
    type t = string

    let of_string t = t
    let to_string t = t
  end

module Linkage_name = Name

module Compilation_unit = struct
  module Name = Name

  (* cf. Compilation_unit.t, which is either a string or a [full] *)
  type t = Obj.t

  type full =
    | With_prefix of
        { name : Name.t;
          for_pack_prefix : Name.t list
        }
    | Global of global_module_name
  and global_module_name =
    {
      head : Name.t;
      args : argument list;
    }
  and argument =
    {
      param: Name.t;
      value: global_module_name;
    }

  let is_instance t =
    Obj.tag t = 1 && Obj.is_block (Obj.field t 1)

  let rec full_path_as_string t =
    let tag = Obj.tag t in
    assert (tag < 2 || tag = Obj.string_tag);
    if tag = Obj.string_tag then
      Sys.opaque_identity (Obj.obj t : Name.t)
    else
      match (Obj.obj t : full) with
      | With_prefix { name; for_pack_prefix = [] } ->
          name
      | With_prefix { name; for_pack_prefix } ->
          String.concat "." for_pack_prefix ^ "." ^ name
      | Global {head; args} ->
          head ^ String.concat "" (List.map print_arg args)
  and print_arg { param; value } =
    match value with
    | { head; args = []} ->
      Printf.sprintf "[%s:%s]" param head
    | _ ->
      let t = Obj.repr (Global value) in
      Printf.sprintf "[%s:%s]" param (full_path_as_string t)

  module Prefix = struct
    let empty = ()
  end

  let create _prefix name = Obj.repr name
end

module Import_info = struct
  module Intf = struct
    module Nonalias = struct
      module Kind = struct
        type t =
        | Normal of Compilation_unit.t
        | Parameter
      end

      type t = Kind.t * Digest.t
    end
  end
end
