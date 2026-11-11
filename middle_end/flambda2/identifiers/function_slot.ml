(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

include Slot.Make (struct
  let colour = Flambda_colours.function_slot

  type payload = int

  let print_payload ppf size = Format.fprintf ppf " [size %d]" size
end)

let create compilation_unit ~name ~size = create compilation_unit ~name size

let size = payload

let size_from_arity ~num_complex_params ~is_tupled =
  if is_tupled || num_complex_params > 1 then 3 else 2
