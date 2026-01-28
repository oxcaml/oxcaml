[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare
open! Regalloc_utils

module RegisterStamp = struct
  type t = int

  type pair = t * t

  let pair (x : t) (y : t) = if x <= y then x, y else y, x

  let fst = fst

  let snd = snd

  module PS = Hashtbl.Make (struct
    type t = pair

    let equal (left : t) (right : t) : bool =
      Int.equal (fst left) (fst right) && Int.equal (snd left) (snd right)

    let hash ((x, y) : t) = (x lsl 17) lxor y
  end)

  module PairSet = struct
    type t = unit PS.t

    let default_size = 256

    let make ~num_registers =
      let estimated_size = (num_registers * num_registers) asr 5 in
      PS.create
        (if estimated_size < default_size then default_size else estimated_size)

    let clear set = PS.clear set

    let mem set (x : pair) = PS.mem set x

    let add set (x : pair) = PS.replace set x ()

    let cardinal set = PS.length set

    let iter set ~f = PS.iter (fun key () -> f key) set
  end
end

module Degree = struct
  type t = int

  let infinite = max_int

  let to_string deg = if deg = max_int then "+inf" else string_of_int deg

  let to_float deg = if deg = max_int then Float.infinity else Float.of_int deg
end

(** Interference graph representation.

    Maintains both an edge set and adjacency lists for performance. The edge set
    (adj_set) provides O(1) membership testing, while adjacency lists (adj_list)
    enable efficient iteration. This duplication is intentional: the IRC
    algorithm requires both fast membership testing (during coalescing) and
    efficient iteration (during simplification and color assignment). *)
type t =
  { adj_set : RegisterStamp.PairSet.t;
    adj_list : Reg.t list Reg.Tbl.t;
    degree : int Reg.Tbl.t
  }

let[@inline] make ~num_registers =
  { adj_set = RegisterStamp.PairSet.make ~num_registers;
    adj_list = Reg.Tbl.create num_registers;
    degree = Reg.Tbl.create num_registers
  }

let[@inline] clear graph =
  RegisterStamp.PairSet.clear graph.adj_set;
  Reg.Tbl.clear graph.adj_list;
  Reg.Tbl.clear graph.degree

let[@inline] add_edge graph u v =
  let is_interesting_reg reg =
    match reg.Reg.loc with
    | Reg _ -> true
    | Unknown -> true
    | Stack (Local _ | Incoming _ | Outgoing _ | Domainstate _) -> false
  in
  let pair = RegisterStamp.pair u.Reg.stamp v.Reg.stamp in
  if
    (not (Reg.same u v))
    && is_interesting_reg u && is_interesting_reg v && same_reg_class u v
    && not (RegisterStamp.PairSet.mem graph.adj_set pair)
  then (
    RegisterStamp.PairSet.add graph.adj_set pair;
    let add_adj_list x y =
      Reg.Tbl.replace graph.adj_list x (y :: Reg.Tbl.find graph.adj_list x)
    in
    let incr_degree x =
      let deg = Reg.Tbl.find graph.degree x in
      if debug && deg = Degree.infinite
      then fatal "trying to increment the degree of a precolored node";
      Reg.Tbl.replace graph.degree x (succ deg)
    in
    let deg_u = Reg.Tbl.find graph.degree u in
    let deg_v = Reg.Tbl.find graph.degree v in
    if deg_u <> Degree.infinite
    then (
      add_adj_list u v;
      incr_degree u);
    if deg_v <> Degree.infinite
    then (
      add_adj_list v u;
      incr_degree v))

let[@inline] mem_edge graph reg1 reg2 =
  RegisterStamp.PairSet.mem graph.adj_set
    (RegisterStamp.pair reg1.Reg.stamp reg2.Reg.stamp)

let[@inline] adj_list graph reg = Reg.Tbl.find graph.adj_list reg

let[@inline] iter_adjacent graph reg ~f = List.iter (adj_list graph reg) ~f

let[@inline] iter_adjacent_if graph reg ~should_visit ~f =
  List.iter (adj_list graph reg) ~f:(fun r -> if should_visit r then f r)

let[@inline] for_all_adjacent graph reg ~f =
  List.for_all (adj_list graph reg) ~f

let[@inline] for_all_adjacent_if graph reg ~should_visit ~f =
  List.for_all (adj_list graph reg) ~f:(fun r ->
      if should_visit r then f r else true)

let[@inline] degree graph reg =
  match Reg.Tbl.find_opt graph.degree reg with
  | None -> fatal "%a is not in the degree map" Printreg.reg reg
  | Some x -> x

let[@inline] set_degree graph reg d = Reg.Tbl.replace graph.degree reg d

let[@inline] incr_degree graph reg =
  let deg = degree graph reg in
  Reg.Tbl.replace graph.degree reg (succ deg)

let[@inline] decr_degree graph reg =
  let deg = degree graph reg in
  if deg <> Degree.infinite then Reg.Tbl.replace graph.degree reg (pred deg)

let[@inline] adj_set graph = graph.adj_set

let[@inline] cardinal graph = RegisterStamp.PairSet.cardinal graph.adj_set

let[@inline] init_register graph reg =
  Reg.Tbl.replace graph.adj_list reg [];
  Reg.Tbl.replace graph.degree reg 0

let[@inline] init_register_with_degree graph reg ~degree =
  Reg.Tbl.replace graph.adj_list reg [];
  Reg.Tbl.replace graph.degree reg degree
