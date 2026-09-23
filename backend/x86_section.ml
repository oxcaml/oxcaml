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
        args : string list;
        link_symbol : string option
      }

    (* As in gas, same-name sections with different linked-to symbols ("o"
       flag) are distinct sections. Flags and other args are not part of the
       identity: a section's later occurrences may omit them. *)
    let equal t1 t2 =
      List.equal String.equal t1.name t2.name
      && Option.equal String.equal t1.link_symbol t2.link_symbol

    let hash t = Hashtbl.hash (t.name, t.link_symbol)

    let compare t1 t2 =
      match List.compare String.compare t1.name t2.name with
      | 0 -> Option.compare String.compare t1.link_symbol t2.link_symbol
      | c -> c

    let make name flags args =
      let has_link_order =
        match flags with
        | Some flags -> String.contains flags 'o'
        | None -> false
      in
      (* With the "o" flag, the linked-to symbol is the last operand. *)
      let link_symbol =
        match List.rev args with
        | last :: _ when has_link_order -> Some last
        | _ -> None
      in
      { name; name_str = String.concat "," name; flags; args; link_symbol }

    let of_string name =
      { name = [name];
        name_str = name;
        flags = None;
        args = [];
        link_symbol = None
      }

    let to_string t = t.name_str

    let flags t = t.flags

    let link_symbol t = t.link_symbol

    let alignment t =
      let rec align = function
        | [] -> 0L
        | [hd] -> Option.value ~default:0L (Int64.of_string_opt hd)
        | _hd :: tl -> align tl
      in
      align t.args

    let is_text_like t = String.starts_with ~prefix:".text" t.name_str

    let is_data_like t = String.starts_with ~prefix:".data" t.name_str

    (* Named like a note, or declared as one with a type operand
       ("@note", as in [Asm_section.Eh_notes_piece]). *)
    let is_note_like t =
      String.starts_with ~prefix:".note" t.name_str
      || List.exists (String.equal "@note") t.args
  end

  include S
  module Map = Map.Make (S)
  module Tbl = Hashtbl.Make (S)
end
