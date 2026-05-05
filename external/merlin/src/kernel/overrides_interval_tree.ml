module Interval = struct
  type 'a t = { loc : Location.t; payload : 'a }

  let create ~(loc : Location.t) ~payload =
    match loc.loc_start.pos_cnum <= loc.loc_end.pos_cnum with
    | true -> Ok { loc; payload }
    | false -> Error "input loc_start greater than loc_end"

  let compare_loc t1 t2 = Location_aux.compare t1.loc t2.loc

  let loc t = t.loc
  let low t = t.loc.loc_start.pos_cnum
  let high t = t.loc.loc_end.pos_cnum

  let compare_range t1 t2 = Int.compare (high t1 - low t1) (high t2 - low t2)

  let payload t = t.payload
end

(** The type representing an interval tree node. *)
type 'a t =
  | Empty
  | Node of
      { center : Lexing.position;
            (** [center] is an approximation of the median of all intervals contained in the subtree [t]. *)
        left : 'a t;
            (** [left] is the subtree containing all intervals to the left of [center]. *)
        right : 'a t;
            (** [left] is the subtree containing all intervals to the right of [center]. *)
        intervals : 'a Interval.t list
            (** [intervals] is a list of all intervals that contain [center] *)
      }

(** Implementation based off of
    {{:https://en.wikipedia.org/wiki/Interval_tree#With_a_point}}this description.  *)
let rec find_helper t position =
  match t with
  | Empty -> []
  | Node node -> (
    let of_t =
      List.filter
        (fun (interval : _ Interval.t) ->
          Location_aux.compare_pos position (Interval.loc interval) = 0)
        node.intervals
    in
    match Std.Lexing.compare_pos position node.center with
    | n when n < 0 ->
      let of_left = find_helper node.left position in
      of_left @ of_t
    | n when n > 0 ->
      let of_right = find_helper node.right position in
      of_right @ of_t
    | _ -> of_t)

let find t point =
  let tightest_interval =
    find_helper t point |> Std.List.min_elt ~cmp:Interval.compare_range
  in
  match tightest_interval with
  | None -> None
  | Some interval -> Some (Interval.payload interval)

let rec of_alist_helper (lst : _ Interval.t list) =
  match List.length lst with
  | 0 -> Empty
  | length ->
    let median =
      (* The start position of the range of the middle interval is a close approximation
         to the median. *)
      let median_interval = List.nth lst (length / 2) in
      (Interval.loc median_interval).loc_start
    in
    let to_left, to_overlap, to_right =
      List.fold_right
        (fun (interval : _ Interval.t) (to_left, to_overlap, to_right) ->
          match Location_aux.compare_pos median (Interval.loc interval) with
          | n when n > 0 -> (interval :: to_left, to_overlap, to_right)
          | n when n < 0 -> (to_left, to_overlap, interval :: to_right)
          | _ -> (to_left, interval :: to_overlap, to_right))
        lst ([], [], [])
    in
    let left = of_alist_helper to_left in
    let right = of_alist_helper to_right in
    let intervals = to_overlap in
    Node { center = median; left; right; intervals }

let of_alist lst =
  lst
  (* Sorting using [Interval.compare_loc] does not guarantee a well-balanced interval tree
     construction on all possible inputs because [Interval.compare_loc] compares
     [loc_start] first, then compares [loc_end]. However, because this is used only by
     [Overrides.t] which typically handles disjoint and sparse [Location.t] ranges, this
     sorting should be a good heuristic. *)
  |> List.stable_sort Interval.compare_loc
  |> of_alist_helper
