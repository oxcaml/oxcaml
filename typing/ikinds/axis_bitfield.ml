(* Low-level bitfield backend shared by axis-based lattices.  See the interface
   for the canonical axis layout. *)

type t = int

let axis_sizes = [| 3; 2; 2; 2; 3; 2; 3; 3; 3; 2; 3 |]
let num_axes = Array.length axis_sizes

let axis_cardinality ~axis =
  if axis < 0 || axis >= num_axes then invalid_arg "Axis_bitfield.axis_cardinality";
  axis_sizes.(axis)

let max_level ~axis = axis_cardinality ~axis - 1

(* widths[i] = 2 for size-3 axes, 1 for size-2 *)
let widths =
  Array.map
    (function 3 -> 2 | 2 -> 1 | _ -> invalid_arg "Axis_bitfield.axis_sizes")
    axis_sizes

(* consecutive packing offsets *)
let offsets =
  let off = Array.make num_axes 0 in
  let a = ref 0 in
  for i = 0 to num_axes - 1 do
    off.(i) <- !a;
    a := !a + widths.(i)
  done;
  off

(* 1 if axis i has a high bit (i.e. width=2), else 0 *)
let has_hi = Array.init num_axes (fun i -> (widths.(i) lsr 1) land 1)

let lo_mask = Array.init num_axes (fun i -> 1 lsl offsets.(i))
let hi_mask =
  Array.init num_axes (fun i -> if has_hi.(i) = 1 then 1 lsl (offsets.(i) + 1) else 0)
let axis_mask = Array.init num_axes (fun i -> lo_mask.(i) lor hi_mask.(i))

let mask ~axis =
  if axis < 0 || axis >= num_axes then invalid_arg "Axis_bitfield.mask";
  axis_mask.(axis)

let mask_many ~axes =
  List.fold_left (fun acc axis -> acc lor mask ~axis) 0 axes

(* OR of all low bits (for size-2 axes that’s their only bit). *)
let lows = Array.fold_left (lor) 0 lo_mask

let bot = 0
let top = Array.fold_left (lor) 0 axis_mask

let join (a : t) (b : t) = a lor b
let meet (a : t) (b : t) = a land b
let leq (a : t) (b : t) = (a land b) = a
let equal (a : t) (b : t) = a = b
let hash = Hashtbl.hash

(* Branchless get: for 3-ary axes level = lo + hi (00→0, 01→1, 11→2).
   For 2-ary axes hi=0 (masked by has_hi). *)
let get (v : t) ~axis =
  if axis < 0 || axis >= num_axes then invalid_arg "Axis_bitfield.get";
  let off = offsets.(axis) in
  let lo = (v lsr off) land 1 in
  let hi = (v lsr (off + 1) land has_hi.(axis)) in
  lo + hi

(* Branchless set:
    low_bit  = (lev | (lev >> 1)) & 1  (0→0, 1→1, 2→1)
    high_bit = (lev >> 1) & has_hi
      (0→0, 1→0, 2→1; zeroed for 1-bit axes)
    No range checks—caller keeps lev in-range. *)
let set (v : t) ~axis ~level =
  if axis < 0 || axis >= num_axes then invalid_arg "Axis_bitfield.set";
  let off = offsets.(axis) in
  let cleared = v land lnot axis_mask.(axis) in
  let lo = (level lor (level lsr 1)) land 1 in
  let hi = (level lsr 1) land has_hi.(axis) in
  cleared lor (lo lsl off) lor (hi lsl (off + 1))

let single_axis ~axis ~level = set bot ~axis ~level

(* Per-axis constants *)
let areality_global = single_axis ~axis:0 ~level:0
let areality_regional = single_axis ~axis:0 ~level:1
let areality_local = single_axis ~axis:0 ~level:2

let linearity_many = single_axis ~axis:1 ~level:0
let linearity_once = single_axis ~axis:1 ~level:1

let uniqueness_aliased = single_axis ~axis:2 ~level:0
let uniqueness_unique = single_axis ~axis:2 ~level:1

let portability_portable = single_axis ~axis:3 ~level:0
let portability_nonportable = single_axis ~axis:3 ~level:1

let contention_contended = single_axis ~axis:4 ~level:0
let contention_shared = single_axis ~axis:4 ~level:1
let contention_uncontended = single_axis ~axis:4 ~level:2

let yielding_unyielding = single_axis ~axis:5 ~level:0
let yielding_yielding = single_axis ~axis:5 ~level:1

let statefulness_stateless = single_axis ~axis:6 ~level:0
let statefulness_observing = single_axis ~axis:6 ~level:1
let statefulness_stateful = single_axis ~axis:6 ~level:2

let visibility_immutable = single_axis ~axis:7 ~level:0
let visibility_read = single_axis ~axis:7 ~level:1
let visibility_read_write = single_axis ~axis:7 ~level:2

let externality_external = single_axis ~axis:8 ~level:0
let externality_external64 = single_axis ~axis:8 ~level:1
let externality_internal = single_axis ~axis:8 ~level:2

let nullability_non_null = single_axis ~axis:9 ~level:0
let nullability_maybe_null = single_axis ~axis:9 ~level:1

let separability_non_float = single_axis ~axis:10 ~level:0
let separability_separable = single_axis ~axis:10 ~level:1
let separability_maybe_separable = single_axis ~axis:10 ~level:2

let encode ~levels : t =
  if Array.length levels <> num_axes then invalid_arg "Axis_bitfield.encode";
  let v = ref bot in
  for axis = 0 to num_axes - 1 do
    v := set !v ~axis ~level:levels.(axis)
  done;
  !v

let decode (v : t) : int array =
  Array.init num_axes (fun axis -> get v ~axis)

let non_bot_axes (v : t) : int list =
  let rec loop axis acc =
    if axis = num_axes then List.rev acc
    else
      let acc' = if v land axis_mask.(axis) <> 0 then axis :: acc else acc in
      loop (axis + 1) acc'
  in
  loop 0 []

let lnot_lsr_1_lows = lnot (lows lsr 1)

let co_sub (a : t) (b : t) : t =
  let r = a land lnot b in
  r lor ((r lsr 1) land lnot_lsr_1_lows)

let to_string (v : t) : string =
  let lv = decode v |> Array.to_list |> List.map string_of_int in
  "[" ^ String.concat "," lv ^ "]"

let mask_shallow = mask_many ~axes:[9; 10]
