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

(* A [Use.t] is the (unique) successor of a [Region.t]. *)
module Use = struct
  type t =
    { cont : Continuation.t;
          (* This is the unique continuation called at the end of a region,
             either via an [Apply_cont] or an [Apply]'s return continuation. *)
      use_id : Apply_cont_rewrite_id.t
          (* We use this to look up the specialized version of [cont] in a
             [specialization_map] if it has been specialized. *)
    }

  let handler { cont; use_id } ~specialization_map : Region.t =
    match Continuation_callsite_map.find cont use_id specialization_map with
    | exception Not_found -> Handler cont
    | specialized -> Handler specialized
end

type t =
  { counters : Fdo_annotation.t Region.Map.t;
    successors : Use.t Region.Map.t
  }

let empty = { counters = Region.Map.empty; successors = Region.Map.empty }

let counters_of t region =
  Option.value
    (Region.Map.find_opt region t.counters)
    ~default:Fdo_annotation.empty

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
  if Flambda_features.check_invariants () && Region.Map.mem region t.successors
  then Misc.fatal_error "Fdo_region_counters: region with two successors";
  { t with
    successors = Region.Map.add region { Use.cont; use_id } t.successors
  }

module Visit = struct
  type t =
    { visited : Region.Set.t;
      counters : Fdo_annotation.t
    }

  let empty = { visited = Region.Set.empty; counters = Fdo_annotation.empty }
end

(* Regions can continue into each other in a cycle (consider [let loop () = f
   (); loop ()]), hence [visited]. *)
let counters_into t ~specialization_map region =
  let rec visit region ({ visited; counters } : Visit.t) =
    if Region.Set.mem region visited
    then counters
    else
      let acc : Visit.t =
        { visited = Region.Set.add region visited;
          counters = Fdo_annotation.combine counters (counters_of t region)
        }
      in
      match Region.Map.find_opt region t.successors with
      | None -> acc.counters
      | Some use -> visit (Use.handler use ~specialization_map) acc
  in
  visit region Visit.empty

let counters_into_handler t ~specialization_map cont ~use_id =
  counters_into t ~specialization_map
    (Use.handler { cont; use_id } ~specialization_map)

let entry_annotation t ~specialization_map code_id entry_annotation =
  Fdo_annotation.With_body_hash.with_annotation entry_annotation
    (Fdo_annotation.combine
       (Fdo_annotation.With_body_hash.annotation entry_annotation)
       (counters_into t ~specialization_map (Function_entry code_id)))

(* CR yusumez: The arms of a collapsed switch all get the count of the whole
   switch, which is incorrect if the branches weren't actually identical and
   e.g. turned into a table lookup. *)

(* CR yusumez: Back edges into a loop header are rebuilt before the header's own
   switch, so if that switch collapses when rebuilt, its arms' counters miss
   them and only reach the edges entering the loop.

   These counters could be passed around separately and patched when we
   translate out of Flambda, but that involves too much work to handle this
   specific case for now. We should see if this is worth it, or figure out a
   better solution. *)

(* CR-someday yusumez: A region entered by an exception (a handler) has no edge
   for its preserved counters. We'd need to have annotations on exceptional
   edges for that. *)

(* CR-someday yusumez: The counters preserved in the unit toplevel outside of
   continuation handlers are dropped, as it has no region. *)

(* CR-someday yusumez: Simplifying a copy again doesn't rename the counters of
   the sets of closures in its body, which the previous run renamed. But a
   function of such a set that the previous run kept as the original's code (for
   lack of improvements) can be improved by this run: its new code then has the
   original's counters, so their counts are merged with the original's. *)

(* CR-someday yusumez: The body of a call inlined at an application with no
   counter (e.g. the second application of a split over-application) gets no
   inlining context, so its counts are merged with those of the callee's
   out-of-line code. *)
