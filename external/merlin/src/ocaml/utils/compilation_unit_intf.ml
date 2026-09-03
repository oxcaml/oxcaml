(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Zesen Qian, Jane Street                         *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-9-40-41-42"]

module Fmt = Format_doc

type t = string

let doc_print = Fmt.pp_print_string

include Identifiable.Make (struct
  type nonrec t = t

  let compare = String.compare

  let equal = String.equal

  let hash = Hashtbl.hash

  let print ppf t = Fmt.compat doc_print ppf t

  let output = Misc.output_of_doc_print doc_print
end)

let print = doc_print

let of_string t = t

let to_string t = t

let dummy = "*dummy*"

let predef_exn = "*predef*"

let print_as_inline_code ppf t = Misc.Style.inline_code ppf (to_string t)

module Found = struct
  type cui = t

  (* The path is advisory only: it is ignored by [compare], [equal] and
     [hash], and merely tells loaders where to look before searching the load
     path. The empty string means the path is unknown. *)
  type t =
    { intf : cui;
      cmi_path : Misc.filepath
    }

  let create intf ~cmi_path = { intf; cmi_path }

  let without_cmi_path intf = { intf; cmi_path = "" }

  let intf t = t.intf

  let with_cmi_path t cmi_path = { t with cmi_path }

  let cmi_path t = match t.cmi_path with "" -> None | path -> Some path

  let doc_print ppf t = doc_print ppf t.intf

  include Identifiable.Make (struct
    type nonrec t = t

    let compare t1 t2 = String.compare t1.intf t2.intf

    let equal t1 t2 = String.equal t1.intf t2.intf

    let hash t = Hashtbl.hash t.intf

    let print ppf t = Fmt.compat doc_print ppf t

    let output = Misc.output_of_doc_print doc_print
  end)

  let print = doc_print
end
