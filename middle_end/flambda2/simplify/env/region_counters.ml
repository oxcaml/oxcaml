(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

module Region = struct
  type t =
    | Function_entry of Code_id.t
    | Handler of Continuation.t

  let compare a b =
    match a, b with
    | Function_entry a, Function_entry b -> Code_id.compare a b
    | Handler a, Handler b -> Continuation.compare a b
    | Function_entry _, Handler _ -> -1
    | Handler _, Function_entry _ -> 1
end

module Region_map = Map.Make (Region)
module Region_set = Set.Make (Region)

type region = Region.t =
  | Function_entry of Code_id.t
  | Handler of Continuation.t

type t =
  { mutable counters : Fdo_counter.t list Region_map.t;
    mutable continues_into : Continuation.t list Region_map.t
  }

let create () =
  { counters = Region_map.empty; continues_into = Region_map.empty }

let add map region values =
  let existing = Option.value (Region_map.find_opt region map) ~default:[] in
  Region_map.add region (values @ existing) map

let add_counters t region counters =
  if not (List.is_empty counters)
  then t.counters <- add t.counters region counters

let add_continuation_into t region cont =
  t.continues_into <- add t.continues_into region [cont]

let counters_into t region =
  (* Regions that continue into each other without ever branching (a loop with
     no exit) are visited once. *)
  let visited = ref Region_set.empty in
  let rec go acc region =
    if Region_set.mem region !visited
    then acc
    else (
      visited := Region_set.add region !visited;
      let acc =
        Option.value (Region_map.find_opt region t.counters) ~default:[] @ acc
      in
      List.fold_left
        (fun acc cont -> go acc (Handler cont))
        acc
        (Option.value (Region_map.find_opt region t.continues_into) ~default:[]))
  in
  List.sort_uniq Stdlib.compare (go [] region)
