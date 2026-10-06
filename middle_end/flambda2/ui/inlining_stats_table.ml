(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Mark Shinwell, Jane Street Europe                *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type value =
  | Int of int
  | Float of float

let table : (string, value) Hashtbl.t = Hashtbl.create 128

let enabled () = Flambda_features.dump_inlining_stats ()

let add key n =
  if n <> 0 && enabled ()
  then
    match Hashtbl.find_opt table key with
    | None -> Hashtbl.replace table key (Int n)
    | Some (Int m) -> Hashtbl.replace table key (Int (m + n))
    | Some (Float f) -> Hashtbl.replace table key (Float (f +. Float.of_int n))

let add_float key f =
  if enabled ()
  then
    match Hashtbl.find_opt table key with
    | None -> Hashtbl.replace table key (Float f)
    | Some (Int m) -> Hashtbl.replace table key (Float (Float.of_int m +. f))
    | Some (Float g) -> Hashtbl.replace table key (Float (g +. f))

let incr key = add key 1

let set_max key n =
  if enabled ()
  then
    match Hashtbl.find_opt table key with
    | Some (Int m) when m >= n -> ()
    | None | Some (Int _ | Float _) -> Hashtbl.replace table key (Int n)

let print_and_reset ppf ~unit_name =
  Format.fprintf ppf "inlining stats for %s@\n" unit_name;
  Hashtbl.fold (fun key value acc -> (key, value) :: acc) table []
  |> List.sort (fun (key1, _) (key2, _) -> String.compare key1 key2)
  |> List.iter (fun (key, value) ->
      match value with
      | Int n -> Format.fprintf ppf "  %d %s@\n" n key
      | Float f -> Format.fprintf ppf "  %.6f %s@\n" f key);
  Format.pp_print_flush ppf ();
  Hashtbl.reset table
