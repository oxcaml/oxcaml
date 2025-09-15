let axis_sizes = [| 3; 2; 2; 2; 3; 2; 3; 3; 3; 2; 3 |]
let num_axes = 11

(* widths[i] = 2 for size-3 axes, 1 for size-2 *)
let widths =
  Array.map (function 3 -> 2 | 2 -> 1 | _ -> invalid_arg "bad axis size") axis_sizes

(* consecutive packing offsets *)
let offsets =
  let off = Array.make num_axes 0 in
  let a = ref 0 in
  for i = 0 to num_axes - 1 do
    off.(i) <- !a; a := !a + widths.(i)
  done; off

(* 1 if axis i has a high bit (i.e. width=2), else 0 *)
let has_hi = Array.init num_axes (fun i -> (widths.(i) lsr 1) land 1)

let lo_mask   = Array.init num_axes (fun i -> 1 lsl offsets.(i))
let hi_mask   = Array.init num_axes (fun i ->
                  if has_hi.(i) = 1 then (1 lsl (offsets.(i) + 1)) else 0)
let axis_mask = Array.init num_axes (fun i -> lo_mask.(i) lor hi_mask.(i))

(* OR of all low bits (for size-2 axes that’s their only bit). For this layout: 0xD5BD. *)
let lows = Array.fold_left (lor) 0 lo_mask

type t = int
let bot : t = 0
(* For this layout top happens to be all 17 bits set: 0x1_FFFF. *)
let top : t = Array.fold_left (lor) 0 axis_mask

let join (a:t) (b:t) : t = a lor b
let meet (a:t) (b:t) : t = a land b
let leq  (a:t) (b:t) : bool = (a land b) = a
let equal (a:t) (b:t) : bool = a = b
let hash = Hashtbl.hash

(* Branchless get: for 3-ary axes level = lo + hi (00→0, 01→1, 11→2).
    For 2-ary axes hi=0 (masked by has_hi). *)
let get_axis (v:t) ~axis:i : int =
  let off = offsets.(i) in
  let lo  = (v lsr off) land 1 in
  let hi  = ((v lsr (off + 1)) land has_hi.(i)) in
  lo + hi

(* Branchless set:
    low_bit  = (lev | (lev >> 1)) & 1  (0→0, 1→1, 2→1)
    high_bit = (lev >> 1) & has_hi     (0→0, 1→0, 2→1; zeroed for 1-bit axes)
    No range checks—caller keeps lev in-range. *)
let set_axis (v:t) ~axis:i ~level:lev : t =
  let off = offsets.(i) in
  let cleared = v land (lnot axis_mask.(i)) in
  let lo  = (lev lor (lev lsr 1)) land 1 in
  let hi  = (lev lsr 1) land has_hi.(i) in
  cleared lor (lo lsl off) lor (hi lsl (off + 1))

let encode ~levels : t =
  let v = ref 0 in
  for i = 0 to num_axes - 1 do
    v := set_axis !v ~axis:i ~level:levels.(i)
  done;
  !v

let decode (v:t) : int array =
  Array.init num_axes (fun i -> get_axis v ~axis:i)

let find_non_bot_axis (v:t) : int option =
  let rec loop i =
    if i = num_axes then None
    else if (v land axis_mask.(i)) <> 0 then Some i
    else loop (i + 1)
  in loop 0

let pp (v:t) : string =
  let lv = decode v |> Array.to_list |> List.map string_of_int in
  "[" ^ String.concat "," lv ^ "]"
let to_string = pp

(* Axis-wise residual:
    r = a & ~b zeroes axes where b >= a; only invalid per-axis pattern is 10 (from 11 - 01).
    (r >> 1) copies surviving high bits down to their own low slots; AND with ~ (lows >> 1)
    kills spillovers from low bits into neighbors; OR repairs 10 -> 11. *)

let lnot_lsr_1_lows = lnot (lows lsr 1)
let co_sub (a:t) (b:t) : t =
  let r = a land (lnot b) in
  r lor ((r lsr 1) land lnot_lsr_1_lows)