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

type t =
  { counters : Fdo_annotation.t Region.Map.t;
    continues_into : Continuation.Set.t Region.Map.t
  }

let empty = { counters = Region.Map.empty; continues_into = Region.Map.empty }

let counters_of t region =
  Option.value
    (Region.Map.find_opt region t.counters)
    ~default:Fdo_annotation.empty

let continuations_of t region =
  Option.value
    (Region.Map.find_opt region t.continues_into)
    ~default:Continuation.Set.empty

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

let add_continuation_into t region cont =
  { t with
    continues_into =
      Region.Map.add region
        (Continuation.Set.add cont (continuations_of t region))
        t.continues_into
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
let counters_into t region =
  let rec visit region ({ visited; counters } as acc : Visit.t) : Visit.t =
    if Region.Set.mem region visited
    then acc
    else
      Continuation.Set.fold
        (fun cont acc -> visit (Region.Handler cont) acc)
        (continuations_of t region)
        ({ visited = Region.Set.add region visited;
           counters = Fdo_annotation.combine counters (counters_of t region)
         }
          : Visit.t)
  in
  (visit region Visit.empty).counters

let entry_annotation t code_id entry_annotation =
  Fdo_annotation.With_body_hash.with_annotation entry_annotation
    (Fdo_annotation.combine
       (Fdo_annotation.With_body_hash.annotation entry_annotation)
       (counters_into t (Function_entry code_id)))
