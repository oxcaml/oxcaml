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
