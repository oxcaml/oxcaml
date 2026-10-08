[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
module DLL = Doubly_linked_list

(* The profile bounds the weight of the function's entry and of the conditional
   and switch edges carrying pseudo-instrumentation counters: exactly, when it
   has the counters in exactly this inlining context; within an interval,
   usually with an estimate in it, when it only has them in related contexts
   (see [Source_position_profile.count]); not at all, for counters it knows
   nothing about. The other edges (unconditional control flow,
   compiler-generated checks, exception edges, returns and other exits, and the
   exceptions a block may raise past the function) start unbounded.

   Flow conservation narrows the intervals: the executions of a block arrive
   through its incoming edges and leave through its outgoing ones, so each edge
   lies within what the other side allows once the rest of its own side is
   accounted for. This propagates to a fixed point (or for as many passes as
   there are edges: a cycle without any bound, an infinite loop, would raise its
   lower bounds forever). An edge whose interval the constraint contradicts
   yields to it when it is not exact; when it is exact and so is the constraint,
   the profile contradicts itself around the block, which is an error beyond a
   tolerance for sampling noise (unless -no-fdo-profile-check); within it, the
   smaller of the two measurements is raised to the larger, since sampling loses
   events rather than inventing them.

   What the profile leaves open is then settled one block side at a time,
   preferring a side whose other side is already exact: its open edges with an
   estimate take it and the others share the rest of the deficit against the
   other side evenly, within their upper bounds; if the estimates alone do not
   fit the deficit, they are scaled to it instead (their excess over their lower
   bounds uniformly) and the others take their lower bounds. Exceptions escaping
   the function are rare and keep their lower bound. Propagation resumes after
   each. The result depends on the block order only through these choices. *)

let tolerance_absolute = 100.

let tolerance_relative = 0.1

type edge =
  | Entry  (** into the entry block, from the callers *)
  | Edge of Label.t * Label.t  (** normal or exceptional successor edge *)
  | Exit of Label.t  (** out of a block without successors *)
  | Escape of Label.t  (** exceptions raised past the function *)

type bound = Source_position_profile.bound =
  { lower : float;
    upper : float;
    estimate : float option
  }

let exact x = { lower = x; upper = x; estimate = Some x }

let unbounded = { lower = 0.; upper = infinity; estimate = None }

let is_exact { lower; upper; _ } = Float.equal lower upper

(* The bounds of a sum. It has an estimate when every term has one. *)
let add a b =
  { lower = a.lower +. b.lower;
    upper = a.upper +. b.upper;
    estimate =
      (match a.estimate, b.estimate with
      | Some a, Some b -> Some (a +. b)
      | None, _ | _, None -> None)
  }

(* Restrict a bound to an interval, keeping its estimate as close as
   possible. *)
let within { estimate; _ } ~lower ~upper =
  { lower;
    upper;
    estimate =
      Option.map (fun e -> Float.min upper (Float.max lower e)) estimate
  }

let sum = List.fold_left add (exact 0.)

(* The bounds of a quantity that several measurements bound: their intersection,
   with the lowest estimate in it. Measurements that contradict each other
   (disjoint bounds) yield the lowest of them. *)
let meet a b =
  let lower = Float.max a.lower b.lower and upper = Float.min a.upper b.upper in
  if Float.compare lower upper <= 0
  then
    let estimate =
      match a.estimate, b.estimate with
      | Some x, Some y -> Some (Float.min x y)
      | (Some _ as estimate), None | None, estimate -> estimate
    in
    within { lower; upper; estimate } ~lower ~upper
  else if Float.compare a.upper b.upper <= 0
  then a
  else b

let is_self_loop = function
  | Edge (src, dst) -> Label.equal src dst
  | Entry | Exit _ | Escape _ -> false

let equal_edge a b =
  match a, b with
  | Entry, Entry -> true
  | Edge (src1, dst1), Edge (src2, dst2) ->
    Label.equal src1 src2 && Label.equal dst1 dst2
  | Exit a, Exit b | Escape a, Escape b -> Label.equal a b
  | (Entry | Edge _ | Exit _ | Escape _), _ -> false

module Edge_tbl = Hashtbl.Make (struct
  type t = edge

  let equal = equal_edge

  let hash = function
    | Entry -> 0
    | Edge (src, dst) -> (Label.to_int src * 65599) + Label.to_int dst + 1
    | Exit label -> (Label.to_int label * 65599) + 2
    | Escape label -> (Label.to_int label * 65599) + 3
end)

(* The edges around every block. A self loop is on both sides. *)
type incidence =
  { incoming : edge list;
    outgoing : edge list
  }

let incidence (cfg : Cfg.t) ~order =
  let table = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
  List.iter
    (fun label ->
      Label.Tbl.replace table label { incoming = []; outgoing = [] })
    order;
  let add_incoming label edge =
    let i = Label.Tbl.find table label in
    Label.Tbl.replace table label { i with incoming = edge :: i.incoming }
  in
  List.iter
    (fun src ->
      let block = Cfg.get_block_exn cfg src in
      let successors =
        Label.Set.elements (Cfg.successor_labels ~normal:true ~exn:true block)
      in
      let outgoing =
        match successors with
        | [] -> [Exit src]
        | _ :: _ ->
          List.map (fun dst -> Edge (src, dst)) successors
          @ if Cfg.can_raise_interproc block then [Escape src] else []
      in
      List.iter
        (fun edge ->
          match edge with
          | Edge (_, dst) -> add_incoming dst edge
          | Entry | Exit _ | Escape _ -> ())
        outgoing;
      let i = Label.Tbl.find table src in
      Label.Tbl.replace table src { i with outgoing })
    order;
  add_incoming cfg.entry_label Entry;
  table

let print_counter ppf counter =
  Format.pp_print_string ppf (Fdo_counter.to_string counter)

let print_counters ppf counters =
  List.iter
    (fun (counter : Fdo_counter.t) ->
      Format.fprintf ppf " %s[%a]"
        (if Fdo_counter.is_function_entry counter.position then "+" else "")
        print_counter counter)
    counters

let body_status profile (cfg : Cfg.t) =
  match cfg.fun_fdo_entry_counters, cfg.fun_function_body_hash with
  | ( { position = Fdo_counter.Function_entry function_id; _ } :: _,
      Some function_body_hash ) ->
    Some
      (Source_position_profile.body_status profile ~function_id
         ~function_body_hash)
  | ( { position = Fdo_counter.Position _ | Fdo_counter.Instantiation_site _; _ }
      :: _,
      _ )
  | [], _
  | _ :: _, None ->
    None

(* The bounds on an edge from the counters on it. Every counter on a machine
   edge is counted on each traversal of it, whether it is the counter of the
   edge itself, of a source edge merged into it, or one preserved for removed
   code the edge leads to (see [Region_counters]). If the profiled build
   compiled the code the same way, a counter on no other edge measures this one,
   and a counter also on other edges ([shared]: one preserved on all the edges
   into some code, or copied with the branch it is on) measures their total,
   which only bounds this one from above. The bounds are intersected. *)
let bounds_of_counters profile ~shared counters =
  let bound counter =
    let bound = Source_position_profile.count profile counter in
    if shared counter
    then { lower = 0.; upper = bound.upper; estimate = None }
    else bound
  in
  match List.sort_uniq Stdlib.compare counters with
  | [] -> unbounded
  | counter :: counters ->
    List.fold_left
      (fun acc counter -> meet acc (bound counter))
      (bound counter) counters

(* The instrumented successor edges of a block, in first-occurrence order, with
   the counters of all the successor positions leading to them; none when no
   position carries counters. *)
let counted_edges (block : Cfg.basic_block) =
  let successors = Cfg.branch_successors block.terminator.desc in
  if
    List.for_all
      (fun (successor : Cfg.successor) -> List.is_empty successor.fdo_counters)
      successors
  then []
  else
    let edges : (Label.t * Fdo_counter.t list ref) list ref = ref [] in
    List.iter
      (fun (successor : Cfg.successor) ->
        match
          List.find_opt
            (fun (dst, _) -> Label.equal dst successor.target)
            !edges
        with
        | Some (_, counters) -> counters := !counters @ successor.fdo_counters
        | None ->
          edges := (successor.target, ref successor.fdo_counters) :: !edges)
      successors;
    List.rev_map (fun (dst, counters) -> dst, !counters) !edges

(* The counters of every instrumented edge, by source block then destination, so
   that a profile for the function can be written by hand (see
   oxcaml/tests/fdo). *)
let edge_counters (cfg : Cfg.t) =
  let table : (Label.t * Fdo_counter.t list) list Label.Tbl.t =
    Label.Tbl.create 16
  in
  let by_kind_then_counter (a : Fdo_counter.t) (b : Fdo_counter.t) =
    Stdlib.compare
      (Fdo_counter.is_function_entry a.position, a)
      (Fdo_counter.is_function_entry b.position, b)
  in
  Cfg.iter_blocks cfg ~f:(fun src block ->
      match counted_edges block with
      | [] -> ()
      | edges ->
        Label.Tbl.replace table src
          (List.map
             (fun (dst, counters) ->
               dst, List.sort_uniq by_kind_then_counter counters)
             edges));
  table

let counters_of edge_counters src dst =
  match Label.Tbl.find_opt edge_counters src with
  | Some edges -> Option.value (List.assoc_opt dst edges) ~default:[]
  | None -> []

let profile_bounds profile (cfg : Cfg.t) =
  let edges =
    Cfg.fold_blocks cfg ~init:[] ~f:(fun src block edges ->
        List.fold_left
          (fun edges (dst, counters) -> (Edge (src, dst), counters) :: edges)
          edges (counted_edges block))
  in
  let edges =
    match cfg.fun_fdo_entry_counters with
    | [] -> edges
    | counters -> (Entry, counters) :: edges
  in
  (* The number of edges each counter is on. *)
  let edges_on : (Fdo_counter.t, int) Hashtbl.t = Hashtbl.create 64 in
  List.iter
    (fun (_, counters) ->
      List.iter
        (fun counter ->
          Hashtbl.replace edges_on counter
            (1 + Option.value (Hashtbl.find_opt edges_on counter) ~default:0))
        (List.sort_uniq Stdlib.compare counters))
    edges;
  let shared counter = Hashtbl.find edges_on counter > 1 in
  let bounds : bound Edge_tbl.t = Edge_tbl.create 64 in
  List.iter
    (fun (edge, counters) ->
      Edge_tbl.replace bounds edge (bounds_of_counters profile ~shared counters))
    edges;
  bounds

type state =
  { bounds : bound Edge_tbl.t;  (** the current bound of an edge *)
    settled : bound Edge_tbl.t
        (** the bound an edge was settled from, for the settled ones *)
  }

let bound state edge =
  Option.value (Edge_tbl.find_opt state.bounds edge) ~default:unbounded

let solve (cfg : Cfg.t) ~order incidence state =
  let bound = bound state in
  let set edge i = Edge_tbl.replace state.bounds edge i in
  let sides label =
    let { incoming; outgoing } = Label.Tbl.find incidence label in
    [incoming, outgoing; outgoing, incoming]
  in
  let sum_of edges = sum (List.map bound edges) in
  let check_tolerance label =
    let { incoming; outgoing } = Label.Tbl.find incidence label in
    let inflow = (sum_of incoming).lower
    and outflow = (sum_of outgoing).lower in
    let gap = Float.abs (inflow -. outflow) in
    if
      !Oxcaml_flags.fdo_profile_check
      && Float.compare gap tolerance_absolute > 0
      && Float.compare gap (tolerance_relative *. Float.max inflow outflow) > 0
    then
      Misc.fatal_errorf
        "FDO profile inconsistent with %s: block %a receives %.0f executions \
         and passes on %.0f"
        cfg.fun_name Label.format label inflow outflow
  in
  (* Narrow every edge of the block by what the rest of the block allows.
     Returns whether anything changed. *)
  let narrow label =
    let changed = ref false in
    List.iter
      (fun (this, other) ->
        let other = sum_of other in
        List.iter
          (fun edge ->
            if not (is_self_loop edge)
            then
              let rest =
                sum_of (List.filter (fun e -> not (equal_edge e edge)) this)
              in
              let constraint_lower = Float.max 0. (other.lower -. rest.upper) in
              let constraint_upper = other.upper -. rest.lower in
              let current = bound edge in
              let lower = Float.max current.lower constraint_lower in
              let upper = Float.min current.upper constraint_upper in
              if Float.compare lower upper <= 0
              then (
                if
                  not
                    (Float.equal lower current.lower
                    && Float.equal upper current.upper)
                then (
                  set edge (within current ~lower ~upper);
                  changed := true))
              else if not (is_exact current)
              then (
                (* A bound that is not exact yields to the rest of the block. *)
                set edge
                  (within current ~lower:constraint_lower
                     ~upper:(Float.max constraint_lower constraint_upper));
                changed := true)
              else if Float.equal constraint_lower constraint_upper
              then (
                (* Two exact measurements disagree around this block. The
                   smaller side of the block is raised to the larger: here, if
                   this edge is on it; from the other side's edges otherwise. *)
                check_tolerance label;
                if Float.compare current.lower constraint_lower < 0
                then (
                  set edge (exact constraint_lower);
                  changed := true)))
          this)
      (sides label);
    !changed
  in
  let num_edges =
    1
    + Label.Tbl.fold
        (fun _ { outgoing; _ } acc -> acc + List.length outgoing)
        incidence 0
  in
  let propagate () =
    let passes = ref (num_edges + 1) in
    let rec loop () =
      let changed =
        List.fold_left (fun acc label -> narrow label || acc) false order
      in
      decr passes;
      if changed && !passes > 0 then loop ()
    in
    loop ()
  in
  (* Spread [deficit] over the lower bounds of [edges], each taking a share
     proportional to its weight (evenly, when the weights are all zero), up to
     its upper bound: an edge that would exceed its upper bound takes it, and
     the rest share what remains. *)
  let rec spread edges deficit ~weight =
    match edges with
    | [] -> ()
    | _ :: _ -> (
      let total =
        List.fold_left (fun acc edge -> acc +. weight edge) 0. edges
      in
      let share edge =
        if Float.compare total 0. > 0
        then deficit *. weight edge /. total
        else deficit /. Float.of_int (List.length edges)
      in
      let capped, uncapped =
        List.partition
          (fun edge ->
            let { lower; upper; _ } = bound edge in
            Float.compare (upper -. lower) (share edge) <= 0)
          edges
      in
      match capped with
      | [] ->
        List.iter
          (fun edge -> set edge (exact ((bound edge).lower +. share edge)))
          edges
      | _ :: _ ->
        let used =
          List.fold_left
            (fun acc edge ->
              let { lower; upper; _ } = bound edge in
              set edge (exact upper);
              acc +. (upper -. lower))
            0. capped
        in
        spread uncapped (deficit -. used) ~weight)
  in
  (* Settle the open edges of one side of a block. Each takes its lower bound;
     the deficit against the other side goes first to the estimates, which take
     theirs, and then evenly to the edges without one (see [spread]). If the
     estimates alone overshoot the deficit, or there is nothing else to take
     what they leave, they are scaled to it instead: the deficit is spread over
     them in proportion to their excess over their lower bounds. Exceptions
     escaping the function are rare and keep their lower bound unless nothing
     else on the side is open. *)
  let settle_side (this, other) =
    let open_ =
      List.filter
        (fun edge -> (not (is_self_loop edge)) && not (is_exact (bound edge)))
        this
    in
    let receivers =
      match
        List.filter
          (function Escape _ -> false | Entry | Edge _ | Exit _ -> true)
          open_
      with
      | [] -> open_
      | receivers -> receivers
    in
    List.iter
      (fun edge ->
        let b = bound edge in
        Edge_tbl.replace state.settled edge b;
        if not (List.exists (equal_edge edge) receivers)
        then set edge (exact b.lower))
      open_;
    let deficit = Float.max 0. ((sum_of other).lower -. (sum_of this).lower) in
    let excess edge =
      let { lower; estimate; _ } = bound edge in
      match estimate with Some e -> e -. lower | None -> 0.
    in
    let estimated, unestimated =
      List.partition
        (fun edge -> Option.is_some (bound edge).estimate)
        receivers
    in
    let estimated_excess =
      List.fold_left (fun acc e -> acc +. excess e) 0. estimated
    in
    match unestimated with
    | _ :: _ when Float.compare estimated_excess deficit <= 0 ->
      List.iter
        (fun edge -> set edge (exact (Option.get (bound edge).estimate)))
        estimated;
      spread unestimated (deficit -. estimated_excess) ~weight:(fun _ -> 1.)
    | _ :: _ | [] ->
      List.iter (fun edge -> set edge (exact (bound edge).lower)) unestimated;
      spread estimated deficit ~weight:excess
  in
  (* The side to settle next: the first, in block order, with open edges whose
     other side is exact (so the deficit is known); failing that, the first with
     open edges at all, taking the other side's lower bounds. *)
  let next_to_settle () =
    let has_open side =
      List.exists
        (fun edge -> (not (is_self_loop edge)) && not (is_exact (bound edge)))
        side
    in
    let candidates =
      List.concat_map
        (fun label ->
          let { incoming; outgoing } = Label.Tbl.find incidence label in
          List.filter
            (fun (this, _) -> has_open this)
            [outgoing, incoming; incoming, outgoing])
        order
    in
    match List.find_opt (fun (_, other) -> not (has_open other)) candidates with
    | Some side -> Some side
    | None -> ( match candidates with [] -> None | side :: _ -> Some side)
  in
  let rec settle () =
    propagate ();
    match next_to_settle () with
    | None -> ()
    | Some side ->
      settle_side side;
      settle ()
  in
  settle ()

type t =
  { cfg_with_layout : Cfg_with_layout.t;
    status : Source_position_profile.body_status option;
        (** [None] for a function without an entry counter *)
    incidence : incidence Label.Tbl.t;
    state : state;
    profile_bounds : bound Edge_tbl.t;
        (** the bounds the profile gave, before solving *)
    order : Label.t list  (** the blocks in layout order *)
  }

let is_hot t =
  match t.status with
  | Some (Same_body | Changed_body) -> true
  | Some Unknown_function | None -> false

let compute profile cfg_with_layout =
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let order = DLL.to_list (Cfg_with_layout.layout cfg_with_layout) in
  let status = body_status profile cfg in
  let incidence = incidence cfg ~order in
  let profile_bounds = profile_bounds profile cfg in
  let state =
    { bounds = Edge_tbl.copy profile_bounds; settled = Edge_tbl.create 16 }
  in
  let t =
    { cfg_with_layout; status; incidence; state; profile_bounds; order }
  in
  if is_hot t then solve cfg ~order incidence state;
  t

let to_count x = Int64.of_float (Float.round x)

let flow t edges = (sum (List.map (bound t.state) edges)).lower

let block_count t label =
  let { incoming; outgoing } = Label.Tbl.find t.incidence label in
  to_count (Float.max (flow t incoming) (flow t outgoing))

let successor_edge_weights t label =
  let cfg = Cfg_with_layout.cfg t.cfg_with_layout in
  let block = Cfg.get_block_exn cfg label in
  List.map
    (fun dst -> dst, to_count (bound t.state (Edge (label, dst))).lower)
    (Label.Set.elements (Cfg.successor_labels ~normal:true ~exn:false block))

let dump_entry ppf (cfg : Cfg.t) =
  List.iteri
    (fun i counter ->
      Format.fprintf ppf "  entry: %s[%a]@."
        (if i = 0 then "" else "+")
        print_counter counter)
    cfg.fun_fdo_entry_counters

let print_bound ppf { lower; upper; estimate } =
  Option.iter (fun e -> Format.fprintf ppf "%.0f " e) estimate;
  if Float.is_finite upper
  then Format.fprintf ppf "in %.0f..%.0f" lower upper
  else Format.fprintf ppf "in %.0f.." lower

let dump ppf t =
  let cfg = Cfg_with_layout.cfg t.cfg_with_layout in
  Format.fprintf ppf "*** FDO block frequencies for %s%s@." cfg.fun_name
    (if is_hot t then "" else ": not profiled");
  Option.iter
    (fun (status : Source_position_profile.body_status) ->
      Format.fprintf ppf "  body: %s@."
        (match status with
        | Same_body -> "same"
        | Changed_body -> "changed"
        | Unknown_function -> "unknown"))
    t.status;
  dump_entry ppf cfg;
  let edge_counters = edge_counters cfg in
  let print_edge ~weights edge =
    (match edge with
    | Entry ->
      Format.fprintf ppf "  edge entry -> %a" Label.format cfg.entry_label
    | Edge (src, dst) ->
      Format.fprintf ppf "  edge %a -> %a%a" Label.format src Label.format dst
        print_counters
        (counters_of edge_counters src dst)
    | Exit src -> Format.fprintf ppf "  edge %a -> exit" Label.format src
    | Escape src -> Format.fprintf ppf "  edge %a -> escape" Label.format src);
    if weights
    then (
      let final = bound t.state edge in
      Format.fprintf ppf ": %.0f" final.lower;
      let given =
        Option.value
          (Edge_tbl.find_opt t.profile_bounds edge)
          ~default:unbounded
      in
      match Edge_tbl.find_opt t.state.settled edge with
      | Some from -> Format.fprintf ppf " (estimated %a)" print_bound from
      | None ->
        if not (is_exact given)
        then Format.fprintf ppf " (derived)"
        else if not (Float.equal final.lower given.lower)
        then Format.fprintf ppf " (measured %.0f)" given.lower);
    Format.fprintf ppf "@."
  in
  if not (is_hot t)
  then
    (* The instrumented edges alone. *)
    Label.Tbl.iter
      (fun src edges ->
        List.iter
          (fun (dst, _) -> print_edge ~weights:false (Edge (src, dst)))
          edges)
      edge_counters
  else (
    List.iter
      (fun label ->
        Format.fprintf ppf "  block %a: %Ld@." Label.format label
          (block_count t label))
      t.order;
    (* The edges by decreasing weight, ties in block order. *)
    let position = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
    List.iteri (fun i label -> Label.Tbl.replace position label i) t.order;
    let position = function
      | Entry -> -1, 0
      | Edge (src, dst) ->
        Label.Tbl.find position src, Label.Tbl.find position dst
      | Exit src -> Label.Tbl.find position src, max_int
      | Escape src -> Label.Tbl.find position src, max_int - 1
    in
    List.concat_map
      (fun label -> (Label.Tbl.find t.incidence label).outgoing)
      t.order
    |> List.cons Entry
    |> List.filter (fun edge -> not (is_self_loop edge))
    |> List.sort (fun edge1 edge2 ->
        let c =
          Float.compare (bound t.state edge2).lower (bound t.state edge1).lower
        in
        if c <> 0 then c else Stdlib.compare (position edge1) (position edge2))
    |> List.iter (print_edge ~weights:true))
