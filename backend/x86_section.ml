(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-40-41-42"]

module Section_name = struct
  module S = struct
    type t =
      { name : string list;
        name_str : string;
        flags : string option;
        args : string list
      }

    let equal t1 t2 = List.equal String.equal t1.name t2.name

    let hash t = Hashtbl.hash t.name

    let compare t1 t2 = List.compare String.compare t1.name t2.name

    let make name flags args =
      { name; name_str = String.concat "," name; flags; args }

    let of_string name =
      { name = [name]; name_str = name; flags = None; args = [] }

    let to_string t = t.name_str

    let flags t = t.flags

    let alignment t =
      let rec align = function
        | [] -> 0L
        | [hd] -> Option.value ~default:0L (Int64.of_string_opt hd)
        | _hd :: tl -> align tl
      in
      align t.args

    let is_text_like t = String.starts_with ~prefix:".text" t.name_str

    let is_data_like t = String.starts_with ~prefix:".data" t.name_str

    let is_note_like t = String.starts_with ~prefix:".note" t.name_str
  end

  include S
  module Map = Map.Make (S)
  module Tbl = Hashtbl.Make (S)
end
