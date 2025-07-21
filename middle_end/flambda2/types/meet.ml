(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                       Basile Clément, OCamlPro                         *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Exposed to the outside world *)
let meet env t1 t2 =
  try
    if Flambda_features.use_n_way_join ()
    then Meet_and_n_way_join.meet env t1 t2
    else Meet_and_join.meet env t1 t2
  with Misc.Fatal_error ->
    let bt = Printexc.get_raw_backtrace () in
    Format.eprintf "\n@[<v 2>%tContext is:%t meet of@ @[%a@]@ and@ @[%a@]@]\n"
      Flambda_colours.error Flambda_colours.pop Type_grammar.print t1
      Type_grammar.print t2;
    Format.eprintf "@[<v 2>%tIn typing environment:%t@ %a@]\n"
      Flambda_colours.error Flambda_colours.pop Typing_env.print env;
    Printexc.raise_with_backtrace Misc.Fatal_error bt

(* Internal use only *)
let[@inline] meet_type () =
  if Flambda_features.use_n_way_join ()
  then Meet_and_n_way_join.meet_type
  else Meet_and_join.meet_type

let meet_shape env t ~shape : _ Or_bottom.t =
  if Typing_env.is_bottom env
  then Bottom
  else match meet env t shape with Bottom -> Bottom | Ok (_, env) -> Ok env
