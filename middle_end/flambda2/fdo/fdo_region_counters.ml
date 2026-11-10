(******************************************************************************
 *                                  OxCaml                                    *
 *                               Jane Street                                  *
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
  (* Regions are named by continuations and code ids, which are unique within
     the simplification of a compilation unit. Note that inlining doesn't start
     a region. *)
  type t =
    | Function_entry of Code_id.t
    | Handler of Continuation.t

  let compare a b =
    match a, b with
    | Function_entry a, Function_entry b -> Code_id.compare a b
    | Handler a, Handler b -> Continuation.compare a b
    | Function_entry _, Handler _ -> -1
    | Handler _, Function_entry _ -> 1

  module Map = Map.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  module Set = Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end

(* A [Use.t] is a successor of a [Region.t]. *)
module Use = struct
  type t =
    { cont : Continuation.t;
      use_id : Apply_cont_rewrite_id.t option
    }

  let compare t1 t2 =
    let c = Continuation.compare t1.cont t2.cont in
    if c <> 0
    then c
    else Option.compare Apply_cont_rewrite_id.compare t1.use_id t2.use_id

  module Set = Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  (* The handler of a continuation specialized for some of its uses is that of
     the specialized continuation. *)
  let handler { cont; use_id } ~specialization_map : Region.t =
    match use_id with
    | None -> Handler cont
    | Some use_id -> (
      match Continuation_callsite_map.find cont use_id specialization_map with
      | exception Not_found -> Handler cont
      | specialized -> Handler specialized)
end

type t =
  { counters : Fdo_annotation.t Region.Map.t;
    successors : Use.Set.t Region.Map.t
  }

let empty = { counters = Region.Map.empty; successors = Region.Map.empty }

let counters_of t region =
  Option.value
    (Region.Map.find_opt region t.counters)
    ~default:Fdo_annotation.empty

let successors_of t region =
  Option.value (Region.Map.find_opt region t.successors) ~default:Use.Set.empty

let add_counters t region counters =
  if Fdo_annotation.is_empty counters
  then t
  else
    { t with
      counters =
        Region.Map.add region
          (Fdo_annotation.combine (counters_of t region) counters)
          t.counters
    }

let add_successor t region cont ~use_id =
  { t with
    successors =
      Region.Map.add region
        (Use.Set.add { cont; use_id } (successors_of t region))
        t.successors
  }

module Visit = struct
  type t =
    { visited : Region.Set.t;
      counters : Fdo_annotation.t
    }

  let empty = { visited = Region.Set.empty; counters = Fdo_annotation.empty }
end

(* Regions can continue into each other in a cycle (a loop that never branches),
   hence [visited]. *)
let counters_into t ~specialization_map region =
  let rec visit region ({ visited; counters } as acc : Visit.t) : Visit.t =
    if Region.Set.mem region visited
    then acc
    else
      let acc_with_me : Visit.t =
        { visited = Region.Set.add region visited;
          counters = Fdo_annotation.combine counters (counters_of t region)
        }
      in
      Use.Set.fold
        (fun use acc -> visit (Use.handler use ~specialization_map) acc)
        (successors_of t region) acc_with_me
  in
  (visit region Visit.empty).counters

let counters_into_handler t ~specialization_map cont ~use_id =
  counters_into t ~specialization_map
    (Use.handler { cont; use_id } ~specialization_map)

let entry_annotation t ~specialization_map code_id entry_annotation =
  Fdo_annotation.With_body_hash.with_annotation entry_annotation
    (Fdo_annotation.combine
       (Fdo_annotation.With_body_hash.annotation entry_annotation)
       (counters_into t ~specialization_map (Function_entry code_id)))

let attach_to_switches t ~specialization_map expr =
  Flambda.map_applies_and_switches expr ~apply:Fun.id
    ~switch:
      (Fdo_term_counters.map_switch_counters ~f:(fun action counters ->
           Fdo_annotation.combine counters
             (counters_into_handler t ~specialization_map
                (Apply_cont_expr.continuation action)
                ~use_id:None)))
