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

  module type S = sig
    type t

    val make : num_registers:int -> t

    val clear : t -> unit

    val mem : t -> pair -> bool

    val add : t -> pair -> unit

    val capacity : t -> int

    module For_debug : sig
      val cardinal : t -> int

      val iter : t -> f:(pair -> unit) -> unit
    end
  end

  module PairSet : S = struct
    type t = unit PS.t

    let default_size = 256

    let make ~num_registers =
      let estimated_size = (num_registers * num_registers) asr 5 in
      PS.create
        (if estimated_size < default_size then default_size else estimated_size)

    let clear set = PS.clear set

    let mem set (x : pair) = PS.mem set x

    let add set (x : pair) = PS.replace set x ()

    let capacity _set = max_int

    module For_debug = struct
      let cardinal set = PS.length set

      let iter set ~f = PS.iter (fun key () -> f key) set
    end
  end

  module BitMatrix : S = struct
    type t =
      { bits : bytes;
        num_registers : int
      }

    external unsafe_get_uint8 : bytes -> int -> int = "%bytes_unsafe_get"
    [@@portable]

    external unsafe_set_uint8 : bytes -> int -> int -> unit
      = "%bytes_unsafe_set"
    [@@portable]

    let[@inline always] bit_location ((i, j) : pair) num_registers =
      (* Compute linear bit offset for pair (i,j) in triangular storage. Layout:
         row i contains pairs (i,i), (i,i+1), ..., (i,n-1). Offset = (elements
         before row i) + (position within row i) = [i*n - i*(i-1)/2] + (j-i) *)
      let bit_offset = (i * num_registers) - ((i * (i - 1)) asr 1) + (j - i) in
      let byte_index = bit_offset lsr 3 in
      let bit_position = bit_offset land 7 in
      byte_index, bit_position

    let make ~num_registers =
      let num_pairs = (num_registers * (num_registers + 1)) lsr 1 in
      let num_bytes = (num_pairs + 7) lsr 3 in
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
        (* NOTE: This operation is O(n²) where n is the number of registers, as
           it scans all bytes in the bit matrix. Unlike PairSet.cardinal which
           is O(1), this implementation counts set bits on-demand. *)
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
            let pair = i, j in
            if mem t pair then f pair
          done
        done
    end
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
  | PairSet of RegisterStamp.PairSet.t
  | BitMatrix of RegisterStamp.BitMatrix.t

type t =
  { mutable adj_set : edge_set;
    adj_list : Reg.t list Reg.Tbl.t;
    degree : int Reg.Tbl.t
  }

let[@inline] make () =
  let num_registers = Reg.For_testing.get_stamp () in
  let adj_set =
    if num_registers < Lazy.force bit_matrix_threshold
    then BitMatrix (RegisterStamp.BitMatrix.make ~num_registers)
    else PairSet (RegisterStamp.PairSet.make ~num_registers)
  in
  { adj_set;
    adj_list = Reg.Tbl.create num_registers;
    degree = Reg.Tbl.create num_registers
  }

let[@inline] clear graph =
  let num_registers = Reg.For_testing.get_stamp () in
  (match graph.adj_set with
  | PairSet set -> RegisterStamp.PairSet.clear set
  | BitMatrix matrix ->
    if RegisterStamp.BitMatrix.capacity matrix < num_registers
    then graph.adj_set <- BitMatrix (RegisterStamp.BitMatrix.make ~num_registers)
    else RegisterStamp.BitMatrix.clear matrix);
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
  let mem_pair =
    match graph.adj_set with
    | PairSet set -> RegisterStamp.PairSet.mem set pair
    | BitMatrix matrix -> RegisterStamp.BitMatrix.mem matrix pair
  in
  if
    (not (Reg.same u v))
    && is_interesting_reg u && is_interesting_reg v && same_reg_class u v
    && not mem_pair
  then (
    (match graph.adj_set with
    | PairSet set -> RegisterStamp.PairSet.add set pair
    | BitMatrix matrix -> RegisterStamp.BitMatrix.add matrix pair);
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
  let pair = RegisterStamp.pair reg1.Reg.stamp reg2.Reg.stamp in
  match graph.adj_set with
  | PairSet set -> RegisterStamp.PairSet.mem set pair
  | BitMatrix matrix -> RegisterStamp.BitMatrix.mem matrix pair

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

let[@inline] init_register_with_degree graph reg ~degree =
  Reg.Tbl.replace graph.adj_list reg [];
  Reg.Tbl.replace graph.degree reg degree

module For_debug = struct
  let cardinal_pairs graph =
    match graph.adj_set with
    | PairSet set -> RegisterStamp.PairSet.For_debug.cardinal set
    | BitMatrix matrix -> RegisterStamp.BitMatrix.For_debug.cardinal matrix

  let iter_pairs graph ~f =
    match graph.adj_set with
    | PairSet set -> RegisterStamp.PairSet.For_debug.iter set ~f
    | BitMatrix matrix -> RegisterStamp.BitMatrix.For_debug.iter matrix ~f
end
