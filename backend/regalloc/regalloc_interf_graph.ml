[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare
open! Regalloc_utils

module Edge = struct
  (* CR vkarvonen: Consider packing the stamps into a single [int]. *)
  type t = Reg.Stamp.t * Reg.Stamp.t

  let make x y = if Reg.Stamp.compare x y <= 0 then x, y else y, x

  let fst = fst

  let snd = snd

  let to_string (x, y) =
    Printf.sprintf "(%s, %s)" (Reg.Stamp.to_string x) (Reg.Stamp.to_string y)

  let equal (left : t) (right : t) : bool =
    Reg.Stamp.equal (fst left) (fst right)
    && Reg.Stamp.equal (snd left) (snd right)

  let hash ((x, y) : t) = (Reg.Stamp.hash x lsl 17) lxor Reg.Stamp.hash y
end

module EdgeTbl = Hashtbl.Make (Edge)

module type S = sig
  type t

  val make : num_registers:int -> t

  val clear : t -> unit

  val mem : t -> Edge.t -> bool

  val add : t -> Edge.t -> unit

  val capacity : t -> int

  module For_debug : sig
    val cardinal : t -> int

    val iter : t -> f:(Edge.t -> unit) -> unit
  end
end

module EdgeSet : S = struct
  type t = unit EdgeTbl.t

  let default_size = 256

  let make ~num_registers =
    let estimated_size = (num_registers * num_registers) asr 5 in
    EdgeTbl.create
      (if estimated_size < default_size then default_size else estimated_size)

  let clear set = EdgeTbl.clear set

  let mem set x = EdgeTbl.mem set x

  let add set x = EdgeTbl.replace set x ()

  let capacity _set = max_int

  module For_debug = struct
    let cardinal set = EdgeTbl.length set

    let iter set ~f = EdgeTbl.iter (fun key () -> f key) set
  end
end

module BitMatrix : S = struct
  type t =
    { bits : bytes;
      num_registers : int
    }

  external unsafe_get_uint8 : bytes -> int -> int = "%bytes_unsafe_get"
  [@@portable]

  external unsafe_set_uint8 : bytes -> int -> int -> unit = "%bytes_unsafe_set"
  [@@portable]

  let[@inline always] bit_location ((i, j) : Edge.t) num_registers =
    (* Compute linear bit offset for edge (i,j) in triangular storage. Layout:
       row i contains edges (i,i), (i,i+1), ..., (i,n-1). Offset = (elements
       before row i) + (position within row i) = [i*n - i*(i-1)/2] + (j-i) *)
    let i = Reg.Stamp.to_int i in
    let j = Reg.Stamp.to_int j in
    let bit_offset = (i * num_registers) - ((i * (i - 1)) asr 1) + (j - i) in
    let byte_index = bit_offset lsr 3 in
    let bit_position = bit_offset land 7 in
    byte_index, bit_position

  let make ~num_registers =
    let num_edges = (num_registers * (num_registers + 1)) lsr 1 in
    let num_bytes = (num_edges + 7) lsr 3 in
    { bits = Bytes.make num_bytes '\000'; num_registers }

  let clear t = Bytes.fill t.bits 0 (Bytes.length t.bits) '\000'

  let mem t edge =
    let byte_index, bit_position = bit_location edge t.num_registers in
    let byte_val = unsafe_get_uint8 t.bits byte_index in
    byte_val land (1 lsl bit_position) <> 0

  let add t edge =
    let byte_index, bit_position = bit_location edge t.num_registers in
    let byte_val = unsafe_get_uint8 t.bits byte_index in
    unsafe_set_uint8 t.bits byte_index (byte_val lor (1 lsl bit_position))

  let capacity t = t.num_registers

  module For_debug = struct
    let cardinal t =
      (* NOTE: This operation is O(n²) where n is the number of registers, as it
         scans all bytes in the bit matrix. Unlike EdgeSet.cardinal which is
         O(1), this implementation counts set bits on-demand. *)
      let count = ref 0 in
      for byte_index = 0 to Bytes.length t.bits - 1 do
        let byte_val = unsafe_get_uint8 t.bits byte_index in
        for bit_position = 0 to 7 do
          if byte_val land (1 lsl bit_position) <> 0 then incr count
        done
      done;
      !count

    let iter t ~f =
      for i = 0 to t.num_registers - 1 do
        for j = i to t.num_registers - 1 do
          let edge = Reg.Stamp.of_int_unsafe i, Reg.Stamp.of_int_unsafe j in
          if mem t edge then f edge
        done
      done
  end
end

module Degree = struct
  type t = int

  let infinite = max_int

  let to_string deg = if deg = max_int then "+inf" else string_of_int deg

  let to_float deg = if deg = max_int then Float.infinity else Float.of_int deg
end

let bit_matrix_threshold : int Lazy.t =
  Regalloc_utils.int_of_param ~default:0 "BIT_MATRIX_THRESHOLD"

(** Interference graph representation.

    Maintains both an edge set and adjacency lists for performance. The edge set
    (adj_set) provides O(1) membership testing, while adjacency lists (adj_list)
    enable efficient iteration. This duplication is intentional: the IRC
    algorithm requires both fast membership testing (during coalescing) and
    efficient iteration (during simplification and color assignment). *)
type edge_set =
  | EdgeSet of EdgeSet.t
  | BitMatrix of BitMatrix.t

type t =
  { mutable adj_set : edge_set;
    adj_list : Reg.t list Reg.Tbl.t;
    degree : int Reg.Tbl.t
  }

let[@inline] make () =
  let num_registers = Reg.For_testing.get_stamp () in
  let adj_set =
    if num_registers < Lazy.force bit_matrix_threshold
    then BitMatrix (BitMatrix.make ~num_registers)
    else EdgeSet (EdgeSet.make ~num_registers)
  in
  { adj_set;
    adj_list = Reg.Tbl.create num_registers;
    degree = Reg.Tbl.create num_registers
  }

let[@inline] clear graph =
  let num_registers = Reg.For_testing.get_stamp () in
  (match graph.adj_set with
  | EdgeSet set -> EdgeSet.clear set
  | BitMatrix matrix ->
    if BitMatrix.capacity matrix < num_registers
    then graph.adj_set <- BitMatrix (BitMatrix.make ~num_registers)
    else BitMatrix.clear matrix);
  Reg.Tbl.clear graph.adj_list;
  Reg.Tbl.clear graph.degree

let[@inline] add_edge graph u v =
  let is_interesting_reg reg =
    match reg.Reg.loc with
    | Reg _ -> true
    | Unknown -> true
    | Stack (Local _ | Incoming _ | Outgoing _ | Domainstate _) -> false
  in
  let edge = Edge.make u.Reg.stamp v.Reg.stamp in
  let[@inline] mem_edge () =
    match graph.adj_set with
    | EdgeSet set -> EdgeSet.mem set edge
    | BitMatrix matrix -> BitMatrix.mem matrix edge
  in
  if
    (not (Reg.same u v))
    && is_interesting_reg u && is_interesting_reg v && same_reg_class u v
    && not (mem_edge ())
  then (
    (match graph.adj_set with
    | EdgeSet set -> EdgeSet.add set edge
    | BitMatrix matrix -> BitMatrix.add matrix edge);
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
  let edge = Edge.make reg1.Reg.stamp reg2.Reg.stamp in
  match graph.adj_set with
  | EdgeSet set -> EdgeSet.mem set edge
  | BitMatrix matrix -> BitMatrix.mem matrix edge

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

let[@inline] init_register graph reg =
  Reg.Tbl.replace graph.adj_list reg [];
  Reg.Tbl.replace graph.degree reg 0

let[@inline] init_register_with_infinite_degree graph reg =
  Reg.Tbl.replace graph.adj_list reg [];
  Reg.Tbl.replace graph.degree reg Degree.infinite

module For_debug = struct
  let cardinal_edges graph =
    match graph.adj_set with
    | EdgeSet set -> EdgeSet.For_debug.cardinal set
    | BitMatrix matrix -> BitMatrix.For_debug.cardinal matrix

  let iter_edges graph ~f =
    match graph.adj_set with
    | EdgeSet set -> EdgeSet.For_debug.iter set ~f
    | BitMatrix matrix -> BitMatrix.For_debug.iter matrix ~f
end
