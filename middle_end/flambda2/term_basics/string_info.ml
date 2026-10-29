(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2019 OCamlPro SAS                                    *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = string

include Container_types.Make (struct
  type t = string

  let compare = String.compare

  let equal = String.equal

  let hash = String.hash

  let print ppf str =
    let size = String.length str in
    let s, dots =
      let max_size = 10 in
      let long = size > max_size in
      if long then String.sub str 0 8, "..." else str, ""
    in
    Format.fprintf ppf "(size %d) (contents \"%S\"%s)" size s dots
end)
